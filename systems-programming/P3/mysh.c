#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>    // (exit, malloc, free, getcwd)
#include <fcntl.h>     // for open flags
#include <sys/types.h> // for pid_t
#include <sys/wait.h>  // for waitpid
#include <errno.h>     // checking system call errors
#include <dirent.h>    // for directory operations (wildcard expansion)
#include <fnmatch.h>   // for matching wildcard patterns

#define STDIN_FD 0   // standard input file descriptor
#define STDOUT_FD 1  // standard output file descriptor
#define STDERR_FD 2  // standard error file descriptor

#define READ_BUFFER_SIZE 4096


typedef struct {
    char **args;
    int count;
    char *input_file;
    char *output_file;
} CommandSegment;

// function prototypes
char *read_command_line(int input_fd);
char **parse_command_dynamic(char *line, int *arg_count);
char **expand_wildcards_in_args_dynamic(char **args, int arg_count, int *new_arg_count);
char *find_program_path(const char *prog_name);
int execute_single_command(CommandSegment *cmd, int input_fd, int output_fd, int close_stdin_if_batch, int is_in_child);
int execute_pipeline(CommandSegment *left_cmd, CommandSegment *right_cmd, int close_stdin_if_batch);
char **append_arg(char **array, int *count, int *capacity, char *arg);
void free_command_segment(CommandSegment *cmd);


// appends an argument to a dynamic array, growing it as needed
char **append_arg(char **array, int *count, int *capacity, char *arg) {
    if (*count >= *capacity) {
        if (*capacity == 0) {
            *capacity = 10;
        } else {
            *capacity = *capacity * 2;
        }
        array = realloc(array, (*capacity) * sizeof(char *));
        if (!array) {
            perror("realloc failed in append_arg");
            exit(EXIT_FAILURE);
        }
    }
    array[*count] = arg; // argument should already be allocated 
    (*count)++;
    return array;
}

// frees memory associated with a CommandSegment
void free_command_segment(CommandSegment *cmd) {
    if (!cmd) return;
    if (cmd->args) {
        for (int i = 0; i < cmd->count; i++) {
            free(cmd->args[i]);
        }
        free(cmd->args);
    }
    free(cmd->input_file);
    free(cmd->output_file);
    // do not free(cmd) here unless it was dynamically allocated
}



// reads a full command line using read(), dynamically growing the buffer
// and preserving any leftover input for following calls
char *read_command_line(int input_fd) {
    // static persistent buffer and related variables
    static char *persist = NULL;
    static size_t persist_size = 0;  // number of bytes stored in persist
    static size_t persist_cap = 0;   // capacity of persist
    char buffer[READ_BUFFER_SIZE];
    ssize_t bytes_read;

    // loop until we find a newline in the persistent buffer
    for (;;) {
        // check if there's a newline already present in the persistent buffer.
        if (persist_size > 0) {
            char *newline_ptr = memchr(persist, '\n', persist_size);
            if (newline_ptr) {
                // newline found, calculate the length of the line to return
                size_t line_len = newline_ptr - persist;
                char *line = malloc(line_len + 1);
                if (!line) {
                    perror("malloc failed");
                    exit(EXIT_FAILURE);
                }
                memcpy(line, persist, line_len);
                line[line_len] = '\0';

                // remove the line (including the newline) from the persistent buffer
                size_t remaining = persist_size - (line_len + 1);
                if (remaining > 0) {
                    memmove(persist, persist + line_len + 1, remaining);
                }
                persist_size = remaining;
                return line;
            }
        }

        // no newline found, read more data
        bytes_read = read(input_fd, buffer, READ_BUFFER_SIZE);
        if (bytes_read < 0) {
            if (errno == EINTR)
                continue; // interrupted by signal, try again
            perror("read error");
            free(persist);
            persist = NULL;
            persist_size = 0;
            persist_cap = 0;
            return NULL;
        }
        if (bytes_read == 0) { // end of file reached
            if (persist_size > 0) {
                // return the remaining data as the final command
                char *line = malloc(persist_size + 1);
                if (!line) {
                    perror("malloc failed");
                    exit(EXIT_FAILURE);
                }
                memcpy(line, persist, persist_size);
                line[persist_size] = '\0';
                persist_size = 0;
                return line;
            } else {
                // no remaining data
                return NULL;
            }
        }

        // append the new data to the persistent buffer
        if (persist_cap < persist_size + bytes_read) {
            size_t new_cap = (persist_size + bytes_read);
            if (new_cap < 1024) {
                new_cap = 1024;
            }
            char *new_persist = realloc(persist, new_cap);
            if (!new_persist) {
                perror("realloc failed for persistent buffer");
                free(persist);
                persist = NULL;
                persist_size = 0;
                persist_cap = 0;
                exit(EXIT_FAILURE);
            }
            persist = new_persist;
            persist_cap = new_cap;
        }
        memcpy(persist + persist_size, buffer, bytes_read);
        persist_size += bytes_read;
        // loop again to check for a newline in the updated persistent buffer
    }
}


// tokenizes the input line dynamically, handling comments
char **parse_command_dynamic(char *line, int *arg_count) {
    int capacity = 0; // start with 0 capacity, append_arg will allocate
    int count = 0;
    char **args = NULL;

    // remove comments
    char *comment_start = strchr(line, '#');
    if (comment_start)
        *comment_start = '\0';

    char *token;
    char *saveptr;
    const char *delimiters = " \t\n"; // include newline just in case

    token = strtok_r(line, delimiters, &saveptr);
    while (token != NULL) {
        char *token_copy = strdup(token);
        if (!token_copy) {
            perror("strdup failed");
            for (int i = 0; i < count; ++i)
                free(args[i]);
            free(args);
            exit(EXIT_FAILURE);
        }
        args = append_arg(args, &count, &capacity, token_copy);
        token = strtok_r(NULL, delimiters, &saveptr);
    }

    // add NULL terminator for execv compatibility
    args = append_arg(args, &count, &capacity, NULL);
    *arg_count = count - 1; // the actual count is one less than size (with NULL)

    return args;
}

// expands wildcards, dynamically resizing the argument list
char **expand_wildcards_in_args_dynamic(char **args, int arg_count, int *new_arg_count) {
    int new_capacity = 0;
    int new_count = 0;
    char **new_args = NULL;
    int skip_next = 0; // flag to skip filename after redirection operation

    for (int i = 0; i < arg_count; i++) {
        if (skip_next) {
            new_args = append_arg(new_args, &new_count, &new_capacity, strdup(args[i]));
            skip_next = 0;
            continue;
        }

        if (strcmp(args[i], "<") == 0 || strcmp(args[i], ">") == 0 || strcmp(args[i], "|") == 0) {
            new_args = append_arg(new_args, &new_count, &new_capacity, strdup(args[i]));
            if (strcmp(args[i], "<") == 0 || strcmp(args[i], ">") == 0) {
                skip_next = 1; // the next token is a filename so don't expand it
            }
            continue;
        }

        if (strchr(args[i], '*') != NULL) {
            char *pattern = args[i];
            char *slash = strrchr(pattern, '/');
            char *dir_path = ".";
            char *base_pattern = pattern;

            if (slash) {
                *slash = '\0';
                dir_path = pattern;
                base_pattern = slash + 1;
            }

            DIR *d = opendir(dir_path);
            int match_found = 0;
            if (d) {
                struct dirent *entry;
                while ((entry = readdir(d)) != NULL) {
                    if (entry->d_name[0] == '.' && base_pattern[0] != '.')
                        continue;
                    if (fnmatch(base_pattern, entry->d_name, FNM_PATHNAME | FNM_NOESCAPE) == 0) {
                        match_found = 1;
                        char *match_name;
                        if (slash) {
                            size_t needed = strlen(dir_path) + 1 + strlen(entry->d_name) + 1;
                            match_name = malloc(needed);
                            if (!match_name) {
                                perror("malloc failed for wildcard path");
                                exit(EXIT_FAILURE);
                            }
                            snprintf(match_name, needed, "%s/%s", dir_path, entry->d_name);
                        } else {
                            match_name = strdup(entry->d_name);
                            if (!match_name) {
                                perror("strdup failed for wildcard match");
                                exit(EXIT_FAILURE);
                            }
                        }
                        new_args = append_arg(new_args, &new_count, &new_capacity, match_name);
                    }
                }
                closedir(d);
            }
            if (slash)
                *slash = '/';
            if (!match_found)
                new_args = append_arg(new_args, &new_count, &new_capacity, strdup(args[i]));
        } else {
            new_args = append_arg(new_args, &new_count, &new_capacity, strdup(args[i]));
        }
    }

    new_args = append_arg(new_args, &new_count, &new_capacity, NULL);
    *new_arg_count = new_count - 1;
    return new_args;
}


// searches for an executable in specified paths or uses the provided path
char *find_program_path(const char *prog_name) {
    if (strchr(prog_name, '/')) {
        if (access(prog_name, X_OK) == 0) {
            char* path = strdup(prog_name);
            if (!path) { perror("strdup"); exit(EXIT_FAILURE); }
            return path;
        } else {
            return NULL;
        }
    }

    if (strcmp(prog_name, "cd") == 0 || strcmp(prog_name, "pwd") == 0 ||
        strcmp(prog_name, "which") == 0 || strcmp(prog_name, "exit") == 0 ||
        strcmp(prog_name, "die") == 0) {
        return NULL;
    }

    const char *search_paths[] = {"/usr/local/bin", "/usr/bin", "/bin", NULL};
    for (int i = 0; search_paths[i] != NULL; i++) {
        size_t needed = strlen(search_paths[i]) + 1 + strlen(prog_name) + 1;
        char *potential_path = malloc(needed);
        if (!potential_path) {
            perror("malloc failed in find_program_path");
            exit(EXIT_FAILURE);
        }
        snprintf(potential_path, needed, "%s/%s", search_paths[i], prog_name);
        if (access(potential_path, X_OK) == 0)
            return potential_path;
        free(potential_path);
    }
    return NULL;
}


// executes a single command segment (built-in or external)
// handles I/O redirection, pipes, and executes commands
int execute_single_command(CommandSegment *cmd, int input_fd, int output_fd, int close_stdin_if_batch, int is_in_child) {
    int command_status = 0;
    int original_stdin = -1, original_stdout = -1;
    int fd_in = -1, fd_out = -1;

    // I/O redirection setup
    if (cmd->input_file != NULL) {
        fd_in = open(cmd->input_file, O_RDONLY);
        if (fd_in < 0) {
            perror(cmd->input_file);
            if (is_in_child) exit(EXIT_FAILURE);
            return 1;
        }
        if (is_in_child) {
            if (dup2(fd_in, STDIN_FD) < 0) { perror("dup2 input failed"); exit(EXIT_FAILURE); }
            close(fd_in);
        } else {
            original_stdin = dup(STDIN_FD);
            if (dup2(fd_in, STDIN_FD) < 0) {
                perror("dup2 input failed");
                close(fd_in);
                if (original_stdin != -1) close(original_stdin);
                return 1;
            }
            close(fd_in);
        }
    } else if (input_fd != STDIN_FD) {
        if (is_in_child) {
            if (dup2(input_fd, STDIN_FD) < 0) { perror("dup2 pipe input failed"); exit(EXIT_FAILURE); }
            close(input_fd);
        } else {
            fprintf(stderr, "Internal error: Pipe input redirection for non-child built-in.\n");
            return 1;
        }
    } else if (close_stdin_if_batch && is_in_child) {
        close(STDIN_FD);
    }

    if (cmd->output_file != NULL) {
        fd_out = open(cmd->output_file, O_WRONLY | O_CREAT | O_TRUNC, 0640);
        if (fd_out < 0) {
            perror(cmd->output_file);
            if (original_stdin != -1) { dup2(original_stdin, STDIN_FD); close(original_stdin); }
            if (is_in_child) exit(EXIT_FAILURE);
            return 1;
        }
        if (is_in_child) {
            if (dup2(fd_out, STDOUT_FD) < 0) { perror("dup2 output failed"); exit(EXIT_FAILURE); }
            close(fd_out);
        } else {
            original_stdout = dup(STDOUT_FD);
            if (dup2(fd_out, STDOUT_FD) < 0) {
                perror("dup2 output failed");
                close(fd_out);
                if (original_stdout != -1) close(original_stdout);
                return 1;
            }
            close(fd_out);
        }
    } else if (output_fd != STDOUT_FD) {
        if (is_in_child) {
            if (dup2(output_fd, STDOUT_FD) < 0) { perror("dup2 pipe output failed"); exit(EXIT_FAILURE); }
            close(output_fd);
        } else {
            fprintf(stderr, "Internal error: Pipe output redirection for non-child built-in.\n");
            return 1;
        }
    }

    // command execution
    if (cmd->count == 0)
        command_status = 0;
    else if (strcmp(cmd->args[0], "exit") == 0) {
        if (is_in_child) exit(EXIT_SUCCESS);
        fprintf(stderr, "Internal error: execute_single_command called directly for exit\n");
        exit(EXIT_SUCCESS);
    } else if (strcmp(cmd->args[0], "die") == 0) {
        for (int i = 1; i < cmd->count; i++) {
            fprintf(stderr, "%s", cmd->args[i]);
            if (i != cmd->count - 1) {
                fprintf(stderr, " ");
            }
        }
        fprintf(stderr, "\n");
        if (is_in_child) exit(EXIT_FAILURE);
        fprintf(stderr, "Internal error: execute_single_command called directly for die\n");
        exit(EXIT_FAILURE);
    } else if (strcmp(cmd->args[0], "cd") == 0) {
        if (is_in_child) {
            fprintf(stderr, "Internal error: 'cd' cannot be executed in a sub-process for pipeline.\n");
            exit(EXIT_FAILURE);
        }
        if (cmd->count != 2) {
            fprintf(stderr, "cd: incorrect number of arguments\n");
            command_status = 1;
        } else {
            if (chdir(cmd->args[1]) == -1) {
                perror(cmd->args[1]);
                command_status = 1;
            } else {
                command_status = 0;
            }
        }
    } else if (strcmp(cmd->args[0], "pwd") == 0) {
        if (cmd->count > 1) {
            fprintf(stderr, "pwd: too many arguments\n");
            command_status = 1;
        } else {
            char *cwd = getcwd(NULL, 0);
            if (!cwd) {
                perror("pwd: getcwd failed");
                command_status = 1;
            } else {
                printf("%s\n", cwd);
                fflush(stdout);
                free(cwd);
                command_status = 0;
            }
        }
        if (is_in_child) {
            if (command_status == 0) {
                exit(EXIT_SUCCESS);
            } else {
                exit(EXIT_FAILURE);
            }
        }
    } else if (strcmp(cmd->args[0], "which") == 0) {
        if (cmd->count != 2) {
            fprintf(stderr, "which: incorrect number of arguments\n");
            command_status = 1;
        } else {
            if (strcmp(cmd->args[1], "cd") == 0 || strcmp(cmd->args[1], "pwd") == 0 ||
                strcmp(cmd->args[1], "exit") == 0 || strcmp(cmd->args[1], "die") == 0 ||
                strcmp(cmd->args[1], "which") == 0) {
                command_status = 1;
            } else {
                char *found_path = find_program_path(cmd->args[1]);
                if (!found_path)
                    command_status = 1;
                else {
                    printf("%s\n", found_path);
                    fflush(stdout);
                    free(found_path);
                    command_status = 0;
                }
            }
        }
        if (is_in_child) {
            if (command_status == 0) {
                exit(EXIT_SUCCESS);
            } else {
                exit(EXIT_FAILURE);
            }
        }
    } else {
        if (!is_in_child) {
            fprintf(stderr, "Internal error: execute_single_command called for external command outside child.\n");
            command_status = 1;
        } else {
            char *prog_path = find_program_path(cmd->args[0]);
            if (!prog_path) {
                fprintf(stderr, "%s: command not found\n", cmd->args[0]);
                exit(EXIT_FAILURE);
            }
            execv(prog_path, cmd->args);
            perror(prog_path);
            free(prog_path);
            exit(EXIT_FAILURE);
        }
    }

    if (original_stdin != -1) {
        if (dup2(original_stdin, STDIN_FD) < 0) perror("dup2 restore stdin failed");
        close(original_stdin);
    }
    if (original_stdout != -1) {
        if (dup2(original_stdout, STDOUT_FD) < 0) perror("dup2 restore stdout failed");
        close(original_stdout);
    }
    return command_status;
}


// executes a pipeline of two commands
int execute_pipeline(CommandSegment *left_cmd, CommandSegment *right_cmd, int close_stdin_if_batch) {
    int pipefd[2];
    int status_left, status_right;
    pid_t pid1, pid2;
    int final_status = 1;

    if (pipe(pipefd) < 0) {
        perror("pipe failed");
        return 1;
    }

    pid1 = fork();
    if (pid1 < 0) {
        perror("fork (left pipeline) failed");
        close(pipefd[0]);
        close(pipefd[1]);
        return 1;
    }
    if (pid1 == 0) {
        close(pipefd[0]);
        execute_single_command(left_cmd, STDIN_FD, pipefd[1], close_stdin_if_batch, 1);
        exit(EXIT_FAILURE);
    }

    pid2 = fork();
    if (pid2 < 0) {
        perror("fork (right pipeline) failed");
        close(pipefd[0]);
        close(pipefd[1]);
        waitpid(pid1, NULL, 0);
        return 1;
    }
    if (pid2 == 0) {
        close(pipefd[1]);
        execute_single_command(right_cmd, pipefd[0], STDOUT_FD, close_stdin_if_batch, 1);
        exit(EXIT_FAILURE);
    }

    close(pipefd[0]);
    close(pipefd[1]);

    waitpid(pid1, &status_left, 0);
    waitpid(pid2, &status_right, 0);

    if (WIFEXITED(status_right))
        final_status = WEXITSTATUS(status_right);
    else {
        fprintf(stderr, "Pipeline right command terminated abnormally.\n");
        final_status = 1;
    }
    return final_status;
}


/* --- Main Function --- */

int main(int argc, char *argv[]) {
    int input_fd = STDIN_FD;
    int interactive_mode = 0;
    int close_stdin_in_batch = 0;

    if (argc > 2) {
        fprintf(stderr, "Usage: mysh [scriptfile]\n");
        exit(EXIT_FAILURE);
    }
    if (argc == 2) {
        input_fd = open(argv[1], O_RDONLY);
        if (input_fd < 0) {
            perror(argv[1]);
            exit(EXIT_FAILURE);
        }
    }

    if (isatty(input_fd) && input_fd == STDIN_FD)
        interactive_mode = 1;
    else {
        interactive_mode = 0;
        close_stdin_in_batch = 1;
    }

    if (interactive_mode) {
        printf("Welcome to my shell!\n");
        fflush(stdout);
    }

    int last_exit_status = 0;
    int first_command = 1;
    char *command_line = NULL;

    while (1) {
        if (interactive_mode) {
            printf("mysh> ");
            fflush(stdout);
        }

        command_line = read_command_line(input_fd);
        if (!command_line)
            break;
        if (strlen(command_line) == 0) {
            free(command_line);
            continue;
        }

        int raw_arg_count = 0;
        char **raw_args = parse_command_dynamic(command_line, &raw_arg_count);
        free(command_line);

        if (raw_arg_count == 0) {
            free(raw_args);
            continue;
        }

        int expanded_arg_count = 0;
        char **expanded_args = expand_wildcards_in_args_dynamic(raw_args, raw_arg_count, &expanded_arg_count);
        for (int i = 0; i <= raw_arg_count; i++) free(raw_args[i]);
        free(raw_args);

        if (expanded_arg_count == 0) {
            free(expanded_args);
            continue;
        }

        int run_command = 1;
        int conditional_offset = 0;
        if (!first_command && expanded_arg_count > 0) {
            if (strcmp(expanded_args[0], "and") == 0) {
                conditional_offset = 1;
                if (last_exit_status != 0)
                    run_command = 0;
            } else if (strcmp(expanded_args[0], "or") == 0) {
                conditional_offset = 1;
                if (last_exit_status == 0)
                    run_command = 0;
            }
        }
        if (!run_command) {
            for (int i = 0; i <= expanded_arg_count; i++) free(expanded_args[i]);
            free(expanded_args);
            first_command = 0;
            continue;
        }

        char **final_args = expanded_args + conditional_offset;
        int final_arg_count = expanded_arg_count - conditional_offset;

        int pipe_index = -1;
        for (int i = 0; i < final_arg_count; i++) {
            if (strcmp(final_args[i], "|") == 0) {
                if (i == 0 || i == final_arg_count - 1 || (i > 0 && (strcmp(final_args[i+1], "and") == 0 || strcmp(final_args[i+1], "or") == 0))) {
                    fprintf(stderr, "mysh: syntax error near pipe\n");
                    last_exit_status = 1;
                    pipe_index = -2;
                    break;
                }
                if (pipe_index != -1) {
                    fprintf(stderr, "mysh: only one pipe supported\n");
                    last_exit_status = 1;
                    pipe_index = -2;
                    break;
                }
                for (int j = 0; j < final_arg_count; ++j) {
                    if (strcmp(final_args[j], "<") == 0 || strcmp(final_args[j], ">") == 0) {
                        fprintf(stderr, "mysh: redirection not supported within pipeline segments\n");
                        last_exit_status = 1;
                        pipe_index = -2;
                        break;
                    }
                }
                if (pipe_index == -2) break;
                pipe_index = i;
            }
        }

        int command_status = 1;
        if (pipe_index == -2)
            command_status = 1;
        else if (pipe_index != -1) {
            CommandSegment left_cmd = {0}, right_cmd = {0};
            int left_capacity = 0, right_capacity = 0;
            for (int i = 0; i < pipe_index; i++) {
                left_cmd.args = append_arg(left_cmd.args, &left_cmd.count, &left_capacity, strdup(final_args[i]));
            }
            left_cmd.args = append_arg(left_cmd.args, &left_cmd.count, &left_capacity, NULL);
            left_cmd.count--;
            for (int i = pipe_index + 1; i < final_arg_count; i++) {
                right_cmd.args = append_arg(right_cmd.args, &right_cmd.count, &right_capacity, strdup(final_args[i]));
            }
            right_cmd.args = append_arg(right_cmd.args, &right_cmd.count, &right_capacity, NULL);
            right_cmd.count--;
            if (left_cmd.count == 0 || right_cmd.count == 0) {
                fprintf(stderr, "mysh: syntax error near empty pipe segment\n");
                command_status = 1;
            } else {
                command_status = execute_pipeline(&left_cmd, &right_cmd, close_stdin_in_batch);
            }
            free_command_segment(&left_cmd);
            free_command_segment(&right_cmd);
        } else {
            CommandSegment cmd = {0};
            int arg_capacity = 0;
            int syntax_error = 0;
            for (int i = 0; i < final_arg_count; i++) {
                if (strcmp(final_args[i], "<") == 0) {
                    i++;
                    if (i >= final_arg_count || strcmp(final_args[i], ">") == 0 || strcmp(final_args[i], "<") == 0 || strcmp(final_args[i], "|") == 0) {
                        fprintf(stderr, "mysh: syntax error near input redirection\n");
                        syntax_error = 1;
                        break;
                    }
                    if (cmd.input_file != NULL) {
                        fprintf(stderr, "mysh: ambiguous input redirect\n");
                        syntax_error = 1;
                        break;
                    }
                    cmd.input_file = strdup(final_args[i]);
                } else if (strcmp(final_args[i], ">") == 0) {
                    i++;
                    if (i >= final_arg_count || strcmp(final_args[i], ">") == 0 || strcmp(final_args[i], "<") == 0 || strcmp(final_args[i], "|") == 0) {
                        fprintf(stderr, "mysh: syntax error near output redirection\n");
                        syntax_error = 1;
                        break;
                    }
                    if (cmd.output_file != NULL) {
                        fprintf(stderr, "mysh: ambiguous output redirect\n");
                        syntax_error = 1;
                        break;
                    }
                    cmd.output_file = strdup(final_args[i]);
                } else {
                    cmd.args = append_arg(cmd.args, &cmd.count, &arg_capacity, strdup(final_args[i]));
                }
            }
            cmd.args = append_arg(cmd.args, &cmd.count, &arg_capacity, NULL);
            cmd.count--;
            if (syntax_error) {
                command_status = 1;
            } else if (cmd.count >= 0) {
                if (cmd.count == -1) {
                    command_status = 0;
                } else if (strcmp(cmd.args[0], "exit") == 0) {
                    free_command_segment(&cmd);
                    for (int i = 0; i <= expanded_arg_count; i++) free(expanded_args[i]);
                    free(expanded_args);
                    if (interactive_mode)
                        printf("Exiting my shell!\n");
                    if (input_fd != STDIN_FD)
                        close(input_fd);
                    exit(EXIT_SUCCESS);
                } else if (strcmp(cmd.args[0], "die") == 0) {
                    for (int i = 1; i < cmd.count; i++) { // use < instead of <= here
                        fprintf(stderr, "%s", cmd.args[i]);
                        if (i != cmd.count - 1) {
                            fprintf(stderr, " ");
                        }
                    }
                    fprintf(stderr, "\n");
                    free_command_segment(&cmd);
                    // free original expanded_args array before exiting
                    for (int i = 0; i <= expanded_arg_count; i++) 
                        free(expanded_args[i]);
                    free(expanded_args);
                    if (input_fd != STDIN_FD)
                        close(input_fd);
                    exit(EXIT_FAILURE);
                } else if (strcmp(cmd.args[0], "cd") == 0) {
                    command_status = execute_single_command(&cmd, STDIN_FD, STDOUT_FD, 0, 0);
                } else {
                    pid_t pid = fork();
                    if (pid < 0) {
                        perror("fork failed");
                        command_status = 1;
                    } else if (pid == 0) {
                        execute_single_command(&cmd, STDIN_FD, STDOUT_FD, close_stdin_in_batch, 1);
                        exit(EXIT_FAILURE);
                    } else {
                        int status;
                        waitpid(pid, &status, 0);
                        if (WIFEXITED(status))
                            command_status = WEXITSTATUS(status);
                        else
                            command_status = 1;
                    }
                }
            } else {
                command_status = 0;
            }
            free_command_segment(&cmd);
        }

        last_exit_status = command_status;
        for (int i = 0; i <= expanded_arg_count; i++) {
            free(expanded_args[i]);
        }
        free(expanded_args);
        first_command = 0;
    }

    if (interactive_mode) {
        printf("Exiting my shell!\n");
        fflush(stdout);
    }

    if (input_fd != STDIN_FD) {
        if (close(input_fd) < 0) {
            perror("Error closing input file");
        }
    }

    return EXIT_SUCCESS;
}
