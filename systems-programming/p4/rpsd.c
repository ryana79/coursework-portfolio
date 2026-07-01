#define _POSIX_C_SOURCE 200809L // for signal handling, poll
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h> // for waitpid
#include <signal.h>   // for signal handling
#include <errno.h>    // for errno
#include <fcntl.h>    // for non-blocking
#include <poll.h>     // for poll()

#include "network.h"

#define MAX_PENDING_CONNECTIONS 10
#define MAX_NAME_LEN 50
#define RPSD_BUF_SIZE 256          // buffer for messages
#define INITIAL_PLAYER_CAPACITY 10 // initial size for dynamic player array
#define GAME_TIMEOUT_MS -1         // no timeout for poll in game (-1 = infinite)

// player states (in parent)
typedef enum
{
    CONNECTED, // initial state after accept
    WAITING,   // sent 'P', waiting for opponent
    PLAYING,   // in an active game managed by a child
} PlayerParentState;

// player move
typedef enum
{
    MOVE_NONE,
    ROCK,
    PAPER,
    SCISSORS
} PlayerMove;

// game states (within child process)
typedef enum
{
    GAME_STARTING,        // initial state, sending B
    WAITING_FOR_MOVES,    // waiting for M from both
    WAITING_FOR_CONTINUE, // waiting for C/Q from both
    GAME_ENDED            // game over, child will exit
} GameState;

// structure to hold player information (in parent)
typedef struct
{
    int fd;                       // client socket descriptor
    char name[MAX_NAME_LEN];      // player's name
    struct sockaddr_in addr;      // client address info
    PlayerParentState state;      // current state of the player in parent view
    int opponent_idx;             // index of the opponent (if PLAYING)
    char read_buf[RPSD_BUF_SIZE]; // buffer for accumulating initial P message
    int read_buf_len;             // current length of data in read_buf
} PlayerInfo;

// global variables (Parent Process)
PlayerInfo *players = NULL;        // dynamically allocated array of players
int player_count = 0;              // current number of players
int player_capacity = 0;           // current allocated capacity of players array
int listen_fd = -1;                // listening socket descriptor
volatile sig_atomic_t running = 1; // for shutdown

// function prototypes
void handle_sigchld(int sig);
void handle_sigint(int sig);
int find_player_by_fd(int fd);                    // find by FD in parent list
int find_active_player_by_name(const char *name); // find WAITING or PLAYING by name
int add_player(int fd, struct sockaddr_in addr);
void remove_player_by_index(int index); // remove by index
int send_message(int fd, const char *message);
PlayerMove parse_move(const char *move_str);
const char *move_to_string(PlayerMove move);
char determine_winner(PlayerMove p1_move, PlayerMove p2_move);                    // returns 'W', 'L', 'D' for player 1
void handle_game(int p1_fd, const char *p1_name, int p2_fd, const char *p2_name); // function run by child process
void cleanup_and_exit(int status);
int process_buffer(int fd, char *buffer, int *buf_len, char *output_msg, size_t output_size); // helper for child game

// signal handlers

void handle_sigchld(int sig)
{
    (void)sig; // mark unused
    // reap all terminated children non-blockingly
    while (waitpid(-1, NULL, WNOHANG) > 0)
        ;
}

void handle_sigint(int sig)
{
    (void)sig; //  mark unused
    printf("\nSIGINT received. Shutting down server...\n");
    running = 0; // signal main loop to exit
    if (listen_fd != -1)
    {
        shutdown(listen_fd, SHUT_RDWR); // try to stop accept
        close(listen_fd);
        listen_fd = -1;
    }
}

// player management (Parent Process)

int find_player_by_fd(int fd)
{
    for (int i = 0; i < player_count; ++i)
    {
        if (players[i].fd == fd)
        {
            return i;
        }
    }
    return -1; // not found
}

int find_active_player_by_name(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return -1;
    for (int i = 0; i < player_count; ++i)
    {
        if ((players[i].state == WAITING || players[i].state == PLAYING) &&
            players[i].name[0] != '\0' &&
            strcmp(players[i].name, name) == 0)
        {
            return i;
        }
    }
    return -1; // not found or not active
}

// add a new player, resizing the array if necessary
int add_player(int fd, struct sockaddr_in addr)
{
    // check if we need to resize the player array
    if (player_count >= player_capacity)
    {
        int new_capacity;
        if (player_capacity == 0)
        {
            new_capacity = INITIAL_PLAYER_CAPACITY;
        }
        else
        {
            new_capacity = player_capacity * 2;
        }
        printf("Resizing player array from %d to %d\n", player_capacity, new_capacity);
        PlayerInfo *temp = realloc(players, new_capacity * sizeof(PlayerInfo));
        if (temp == NULL)
        {
            perror("Failed to reallocate player array");
            // keep the old array, cannot add player
            close(fd);
            return -1;
        }
        players = temp;
        player_capacity = new_capacity;
    }

    // now add the player
    int index = player_count;
    players[index].fd = fd;
    players[index].addr = addr;
    players[index].state = CONNECTED;
    players[index].opponent_idx = -1;
    memset(players[index].name, 0, MAX_NAME_LEN);
    players[index].read_buf_len = 0;
    memset(players[index].read_buf, 0, RPSD_BUF_SIZE);
    player_count++;

    char client_ip[INET_ADDRSTRLEN];
    inet_ntop(AF_INET, &addr.sin_addr, client_ip, sizeof(client_ip));
    printf("Player connected: fd=%d, ip=%s, port=%d (Index: %d, Count: %d, Capacity: %d)\n",
           fd, client_ip, ntohs(addr.sin_port), index, player_count, player_capacity);

    return index;
}

// remove player by index
void remove_player_by_index(int index)
{
    if (index < 0 || index >= player_count)
        return;

    // determine name for logging
    const char *name_to_log;
    if (players[index].name[0] != '\0')
    {
        name_to_log = players[index].name;
    }
    else
    {
        name_to_log = "(no name)";
    }

    printf("Removing player: fd=%d, name='%s' (State: %d, Index: %d)\n",
           players[index].fd, name_to_log, players[index].state, index);

    // only close FD if it's valid and NOT managed by a child
    if (players[index].fd != -1 && players[index].state != PLAYING)
    {
        close(players[index].fd);
    }
    else if (players[index].state == PLAYING)
    {
        printf("Note: Not closing fd %d for PLAYING player in parent.\n", players[index].fd);
    }

    // shift remaining players down
    if (index < player_count - 1)
    {
        memmove(&players[index], &players[index + 1], (player_count - index - 1) * sizeof(PlayerInfo));
    }
    player_count--;

    // update opponent_idx
    for (int i = 0; i < player_count; ++i)
    {
        if (players[i].opponent_idx > index)
        {
            players[i].opponent_idx--;
        }
        else if (players[i].opponent_idx == index)
        {
            players[i].opponent_idx = -1;
        }
    }
    printf("Player removed. Count: %d, Capacity: %d\n", player_count, player_capacity);
}

// network communication

int send_message(int fd, const char *message)
{
    size_t len = strlen(message);
    ssize_t bytes_sent_total = 0;
    while (bytes_sent_total < len)
    {
        ssize_t result = send(fd, message + bytes_sent_total, len - bytes_sent_total, 0);
        if (result < 0)
        {
            if (errno == EINTR)
                continue;
            if (errno == EPIPE || errno == ECONNRESET)
            {
                printf("Send failed: Client fd %d disconnected (%s).\n", fd, strerror(errno));
            }
            else
            {
                perror("Send failed");
            }
            return -1;
        }
        if (result == 0)
        {
            fprintf(stderr, "Send returned 0 unexpectedly for fd %d.\n", fd);
            return -1;
        }
        bytes_sent_total += result;
    }
    return 0;
}

// game logic & protocol helpers

PlayerMove parse_move(const char *move_str)
{
    if (strcmp(move_str, "ROCK") == 0)
        return ROCK;
    if (strcmp(move_str, "PAPER") == 0)
        return PAPER;
    if (strcmp(move_str, "SCISSORS") == 0)
        return SCISSORS;
    return MOVE_NONE;
}

const char *move_to_string(PlayerMove move)
{
    switch (move)
    {
    case ROCK:
        return "ROCK";
    case PAPER:
        return "PAPER";
    case SCISSORS:
        return "SCISSORS";
    default:
        return "";
    }
}

char determine_winner(PlayerMove p1_move, PlayerMove p2_move)
{
    if (p1_move == MOVE_NONE || p2_move == MOVE_NONE)
        return 'F';
    if (p1_move == p2_move)
        return 'D';
    if ((p1_move == ROCK && p2_move == SCISSORS) ||
        (p1_move == SCISSORS && p2_move == PAPER) ||
        (p1_move == PAPER && p2_move == ROCK))
    {
        return 'W';
    }
    return 'L';
}

int process_buffer(int fd, char *buffer, int *buf_len, char *output_msg, size_t output_size)
{
    char *delimiter = strstr(buffer, "||");
    if (delimiter)
    {
        size_t message_len = (delimiter - buffer) + 2;
        if (message_len >= output_size)
        {
            fprintf(stderr, "[Game Child %d] Error: Received message too large for output buffer (fd=%d)\n", getpid(), fd);
            *buf_len = 0;
            memset(buffer, 0, RPSD_BUF_SIZE);
            return -1;
        }
        memcpy(output_msg, buffer, message_len);
        output_msg[message_len] = '\0';

        *buf_len -= message_len;
        memmove(buffer, buffer + message_len, *buf_len);
        memset(buffer + *buf_len, 0, RPSD_BUF_SIZE - *buf_len);
        return 1;
    }
    if (*buf_len >= RPSD_BUF_SIZE - 1)
    {
        fprintf(stderr, "[Game Child %d] Error: Read buffer full, no delimiter found (fd=%d)\n", getpid(), fd);
        *buf_len = 0;
        memset(buffer, 0, RPSD_BUF_SIZE);
        return -1;
    }
    return 0;
}

void handle_game(int p1_fd, const char *p1_name, int p2_fd, const char *p2_name)
{
    printf("[Game Child %d] Starting game between %s (fd %d) and %s (fd %d)\n", getpid(), p1_name, p1_fd, p2_name, p2_fd);

    if (listen_fd != -1)
        close(listen_fd);
    signal(SIGPIPE, SIG_IGN);
    signal(SIGINT, SIG_DFL);
    signal(SIGCHLD, SIG_DFL);

    GameState game_state = GAME_STARTING;
    PlayerMove p1_move = MOVE_NONE;
    PlayerMove p2_move = MOVE_NONE;
    // use -1 to indicate "response not yet received" for C/Q state
    int p1_wants_rematch = -1;
    int p2_wants_rematch = -1;
    int p1_forfeited = 0;
    int p2_forfeited = 0;

    char p1_read_buf[RPSD_BUF_SIZE] = {0};
    int p1_read_len = 0;
    char p2_read_buf[RPSD_BUF_SIZE] = {0};
    int p2_read_len = 0;
    char incoming_msg[RPSD_BUF_SIZE];
    char msg_to_send[RPSD_BUF_SIZE];

    struct pollfd pfds[2];
    pfds[0].fd = p1_fd;
    pfds[0].events = POLLIN;
    pfds[1].fd = p2_fd;
    pfds[1].events = POLLIN;

    int running_game = 1;

    while (running_game)
    {
        if (game_state == GAME_STARTING)
        {
            printf("[Game Child %d] State: GAME_STARTING. Sending BEGIN...\n", getpid());
            p1_move = MOVE_NONE;
            p2_move = MOVE_NONE;
            p1_wants_rematch = -1;
            p2_wants_rematch = -1; // reset C/Q state to "waiting"
            p1_forfeited = 0;
            p2_forfeited = 0;
            p1_read_len = 0;
            memset(p1_read_buf, 0, sizeof(p1_read_buf)); // clear any leftover buffer data
            p2_read_len = 0;
            memset(p2_read_buf, 0, sizeof(p2_read_buf));

            snprintf(msg_to_send, sizeof(msg_to_send), "B|%s||", p2_name);
            if (send_message(p1_fd, msg_to_send) < 0)
            {
                p1_forfeited = 1;
                game_state = GAME_ENDED;
                continue;
            }
            snprintf(msg_to_send, sizeof(msg_to_send), "B|%s||", p1_name);
            if (send_message(p2_fd, msg_to_send) < 0)
            {
                p2_forfeited = 1;
                game_state = GAME_ENDED;
                continue;
            }

            game_state = WAITING_FOR_MOVES;
            printf("[Game Child %d] State: WAITING_FOR_MOVES.\n", getpid());
            continue;
        }

        int ready = poll(pfds, 2, GAME_TIMEOUT_MS);
        if (ready < 0)
        {
            if (errno == EINTR)
                continue;
            perror("[GC] Poll failed");
            game_state = GAME_ENDED;
            continue;
        }
        if (ready == 0)
        {
            continue;
        }

        // check Player 1
        if (pfds[0].revents)
        {
            if (pfds[0].revents & (POLLHUP | POLLERR | POLLNVAL))
            {
                printf("[GC %d] P1 poll error/hup.\n", getpid());
                p1_forfeited = 1;
                game_state = GAME_ENDED;
            }
            else if (pfds[0].revents & POLLIN)
            {
                char temp_buf[RPSD_BUF_SIZE];
                ssize_t bytes_read = recv(p1_fd, temp_buf, sizeof(temp_buf) - 1, 0);
                if (bytes_read > 0)
                {
                    if (p1_read_len + bytes_read >= RPSD_BUF_SIZE)
                    { // check overflow *before* memcpy
                        fprintf(stderr, "[GC %d] Error: P1 buffer overflow.\n", getpid());
                        p1_read_len = 0;
                        memset(p1_read_buf, 0, sizeof(p1_read_buf));
                        p1_forfeited = 1;
                        game_state = GAME_ENDED;
                    }
                    else
                    {
                        memcpy(p1_read_buf + p1_read_len, temp_buf, bytes_read);
                        p1_read_len += bytes_read;
                        p1_read_buf[p1_read_len] = '\0';

                        // process P1 Buffer
                        if (game_state == WAITING_FOR_CONTINUE)
                        {
                            // only process a single C or Q from P1
                            if (p1_read_len > 0 && p1_wants_rematch == -1)
                            {
                                char c = p1_read_buf[0];
                                // consume that byte
                                memmove(p1_read_buf, p1_read_buf + 1, --p1_read_len);
                                p1_read_buf[p1_read_len] = '\0';

                                if (c == 'C')
                                {
                                    p1_wants_rematch = 1;
                                }
                                else if (c == 'Q')
                                {
                                    p1_wants_rematch = 0;
                                    // clear any leftover data just in case
                                    p1_read_len = 0;
                                    p1_read_buf[0] = '\0';
                                    // quit immediately
                                    game_state = GAME_ENDED;
                                }
                                else
                                {
                                    // anything else is a forfeit
                                    p1_forfeited = 1;
                                    game_state = GAME_ENDED;
                                }
                            }
                        }
                        else
                        {
                            // handle || terminated messages (like Moves)
                            int process_status;
                            while ((process_status = process_buffer(p1_fd, p1_read_buf, &p1_read_len, incoming_msg, sizeof(incoming_msg))) == 1)
                            {
                                printf("[GC %d] Received from P1 (fd %d): %s\n", getpid(), p1_fd, incoming_msg);
                                if (game_state == WAITING_FOR_MOVES && incoming_msg[0] == 'M' && incoming_msg[1] == '|')
                                {
                                    char *start = incoming_msg + 2;
                                    char *end = strstr(start, "||");
                                    if (end)
                                    {
                                        *end = '\0';
                                        p1_move = parse_move(start);
                                    }
                                    if (p1_move == MOVE_NONE)
                                    {
                                        fprintf(stderr, "[GC %d] Invalid move P1.\n", getpid());
                                        p1_forfeited = 1;
                                        game_state = GAME_ENDED;
                                        break;
                                    }
                                }
                                else
                                {
                                    // should not receive other message types in WAITING_FOR_MOVES
                                    fprintf(stderr, "[GC %d] Unexpected msg P1 state %d: %s\n", getpid(), game_state, incoming_msg);
                                    p1_forfeited = 1;
                                    game_state = GAME_ENDED;
                                    break;
                                }
                            }
                            if (process_status < 0)
                            {
                                p1_forfeited = 1;
                                game_state = GAME_ENDED;
                            }
                        }
                        // end process P1 Buffer
                    }
                }
                else if (bytes_read == 0)
                {
                    printf("[GC %d] P1 disconnected.\n", getpid());
                    p1_forfeited = 1;
                    game_state = GAME_ENDED;
                }
                else
                {
                    if (errno != EINTR)
                    {
                        perror("[GC] Recv P1 failed");
                        p1_forfeited = 1;
                        game_state = GAME_ENDED;
                    }
                }
            }
        } // end P1 check

        // check Player 2
        if (pfds[1].revents)
        {
            if (pfds[1].revents & (POLLHUP | POLLERR | POLLNVAL))
            {
                printf("[GC %d] P2 poll error/hup.\n", getpid());
                p2_forfeited = 1;
                game_state = GAME_ENDED;
            }
            else if (pfds[1].revents & POLLIN)
            {
                char temp_buf[RPSD_BUF_SIZE];
                ssize_t bytes_read = recv(p2_fd, temp_buf, sizeof(temp_buf) - 1, 0);
                if (bytes_read > 0)
                {
                    if (p2_read_len + bytes_read >= RPSD_BUF_SIZE)
                    { // check overflow *before* memcpy
                        fprintf(stderr, "[GC %d] Error: P2 buffer overflow.\n", getpid());
                        p2_read_len = 0;
                        memset(p2_read_buf, 0, sizeof(p2_read_buf));
                        p2_forfeited = 1;
                        game_state = GAME_ENDED;
                    }
                    else
                    {
                        memcpy(p2_read_buf + p2_read_len, temp_buf, bytes_read);
                        p2_read_len += bytes_read;
                        p2_read_buf[p2_read_len] = '\0';

                        // process P2 Buffer
                        if (game_state == WAITING_FOR_CONTINUE)
                        {
                            // only process a single C or Q, once
                            if (p2_read_len > 0 && p2_wants_rematch == -1)
                            {
                                char c = p2_read_buf[0];
                                // consume the char
                                memmove(p2_read_buf, p2_read_buf + 1, --p2_read_len);
                                p2_read_buf[p2_read_len] = '\0';

                                if (c == 'C')
                                {
                                    p2_wants_rematch = 1;
                                }
                                else if (c == 'Q')
                                {
                                    p2_wants_rematch = 0;
                                    // immediately end the game when they quit
                                    game_state = GAME_ENDED;
                                }
                                else
                                {
                                    // anything else is treated as a forfeit
                                    p2_forfeited = 1;
                                    game_state = GAME_ENDED;
                                }
                            }
                        }
                        else
                        {
                            // handle || terminated messages (like Moves)
                            int process_status;
                            while ((process_status = process_buffer(p2_fd, p2_read_buf, &p2_read_len, incoming_msg, sizeof(incoming_msg))) == 1)
                            {
                                printf("[GC %d] Received from P2 (fd %d): %s\n", getpid(), p2_fd, incoming_msg);
                                if (game_state == WAITING_FOR_MOVES && incoming_msg[0] == 'M' && incoming_msg[1] == '|')
                                {
                                    char *start = incoming_msg + 2;
                                    char *end = strstr(start, "||");
                                    if (end)
                                    {
                                        *end = '\0';
                                        p2_move = parse_move(start);
                                    }
                                    if (p2_move == MOVE_NONE)
                                    {
                                        fprintf(stderr, "[GC %d] Invalid move P2.\n", getpid());
                                        p2_forfeited = 1;
                                        game_state = GAME_ENDED;
                                        break;
                                    }
                                }
                                else
                                {
                                    fprintf(stderr, "[GC %d] Unexpected msg P2 state %d: %s\n", getpid(), game_state, incoming_msg);
                                    p2_forfeited = 1;
                                    game_state = GAME_ENDED;
                                    break;
                                }
                            }
                            if (process_status < 0)
                            {
                                p2_forfeited = 1;
                                game_state = GAME_ENDED;
                            }
                        }
                        // --- end Process P2 Buffer
                    }
                }
                else if (bytes_read == 0)
                {
                    printf("[GC %d] P2 disconnected.\n", getpid());
                    p2_forfeited = 1;
                    game_state = GAME_ENDED;
                }
                else
                {
                    if (errno != EINTR)
                    {
                        perror("[GC] Recv P2 failed");
                        p2_forfeited = 1;
                        game_state = GAME_ENDED;
                    }
                }
            }
        } // end P2 check

        // state transitions and logic
        if (game_state == WAITING_FOR_MOVES)
        {
            // check if both have moved (or one forfeited)
            if ((p1_move != MOVE_NONE && p2_move != MOVE_NONE) || p1_forfeited || p2_forfeited)
            {
                if (p1_forfeited || p2_forfeited)
                {
                    // if forfeit happened during move phase, go straight to end
                    game_state = GAME_ENDED;
                }
                else
                {
                    // both moved successfully, determine winner and send results
                    char p1_result = determine_winner(p1_move, p2_move);
                    char p2_result; // determine
                    if (p1_result == 'W')
                        p2_result = 'L';
                    else if (p1_result == 'L')
                        p2_result = 'W';
                    else
                        p2_result = p1_result; // D or F

                    printf("[GC %d] Result: %s(%c) vs %s(%c)\n", getpid(), p1_name, p1_result, p2_name, p2_result);

                    snprintf(msg_to_send, sizeof(msg_to_send), "R|%c|%s||", p1_result, move_to_string(p2_move));
                    if (send_message(p1_fd, msg_to_send) < 0)
                        p1_forfeited = 1;
                    snprintf(msg_to_send, sizeof(msg_to_send), "R|%c|%s||", p2_result, move_to_string(p1_move));
                    if (send_message(p2_fd, msg_to_send) < 0)
                        p2_forfeited = 1;

                    if (p1_forfeited || p2_forfeited)
                    {
                        game_state = GAME_ENDED; // send failed, game over
                    }
                    else
                    {
                        game_state = WAITING_FOR_CONTINUE;
                        p1_wants_rematch = -1;
                        p2_wants_rematch = -1; // reset for next phase
                        printf("[GC %d] State: WAITING_FOR_CONTINUE.\n", getpid());
                    }
                }
            }
            // else: still waiting for one or both moves
        }
        else if (game_state == WAITING_FOR_CONTINUE)
        {
            // check if both have responded C/Q (or one forfeited)
            if ((p1_wants_rematch != -1 && p2_wants_rematch != -1) || p1_forfeited || p2_forfeited)
            {
                if (p1_wants_rematch == 1 && p2_wants_rematch == 1 && !p1_forfeited && !p2_forfeited)
                {
                    // both continue: Start next round
                    printf("[GC %d] Both continue.\n", getpid());
                    game_state = GAME_STARTING; // Loop back to send B
                }
                else
                {
                    // one or both quit/forfeited
                    printf("[GC %d] Game ending (C/Q/Forfeit decision).\n", getpid());
                    game_state = GAME_ENDED;
                }
            }
            // else: still waiting for one or both C/Q responses
        }

        // final Check and Game End Handling
        if (game_state == GAME_ENDED)
        {
            if (p1_forfeited && !p2_forfeited)
            {
                printf("[GC %d] %s forfeited during game.\n", getpid(), p1_name);
                snprintf(msg_to_send, sizeof(msg_to_send), "R|F|||");
                send_message(p2_fd, msg_to_send);
            }
            else if (p2_forfeited && !p1_forfeited)
            {
                printf("[GC %d] %s forfeited during game.\n", getpid(), p2_name);
                snprintf(msg_to_send, sizeof(msg_to_send), "R|F|||");
                send_message(p1_fd, msg_to_send);
            }
            else if (p1_forfeited && p2_forfeited)
            {
                printf("[GC %d] Both forfeited/disconnected during game.\n", getpid());
            }
            running_game = 0;
        }
    }

    printf("[Game Child %d] Closing connections fds %d and %d.\n", getpid(), p1_fd, p2_fd);
    close(p1_fd);
    close(p2_fd);
    printf("[Game Child %d] Exiting.\n", getpid());
    exit(EXIT_SUCCESS);
}

// main server logic

void cleanup_and_exit(int status)
{
    printf("Server shutting down. Closing sockets...\n");
    running = 0;
    for (int i = 0; i < player_count; ++i)
    {
        // only close FDs that the parent still owns (valid and not playing)
        if (players[i].fd != -1 && players[i].state != PLAYING)
        {
            printf("Closing parent fd %d for player %s\n", players[i].fd, players[i].name);
            close(players[i].fd);
        }
        else
        {
            printf("Skipping close for fd %d (player %s, state %d)\n", players[i].fd, players[i].name, players[i].state);
        }
    }
    if (listen_fd != -1)
        close(listen_fd);
    listen_fd = -1;

    free(players);
    players = NULL;
    player_count = 0;
    player_capacity = 0;

    printf("Shutdown complete.\n");
    exit(status);
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <port>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    printf("Starting Rock-Paper-Scissors server (rpsd) on port %s...\n", argv[1]);

    // initialize player array
    players = malloc(INITIAL_PLAYER_CAPACITY * sizeof(PlayerInfo));
    if (players == NULL)
    {
        perror("Failed to allocate initial player array");
        exit(EXIT_FAILURE);
    }
    player_capacity = INITIAL_PLAYER_CAPACITY;
    player_count = 0;

    // setup Signal Handlers
    signal(SIGCHLD, handle_sigchld);
    signal(SIGPIPE, SIG_IGN);
    signal(SIGINT, handle_sigint);

    listen_fd = open_listener(argv[1], MAX_PENDING_CONNECTIONS);
    if (listen_fd < 0)
    {
        fprintf(stderr, "Failed to open listener on port %s\n", argv[1]);
        free(players); // clean up allocated memory before exiting
        exit(EXIT_FAILURE);
    }
    printf("Server listening on port %s (fd: %d)...\n", argv[1], listen_fd);

    int waiting_player_idx = -1;

    // dynamically allocate pollfd array - needs space for listener + all players
    // start with a reasonable initial size, similar to players array
    int pollfd_capacity = INITIAL_PLAYER_CAPACITY + 1; // +1 for listener
    struct pollfd *pfds = malloc(pollfd_capacity * sizeof(struct pollfd));
    if (!pfds)
    {
        perror("Failed to allocate pollfd array");
        close(listen_fd);
        free(players);
        exit(EXIT_FAILURE);
    }

    while (running)
    {
        int nfds = 0; // number of fds actually being polled this iteration

        // rebuild pollfd array
        // ensure capacity
        if (player_count + 1 > pollfd_capacity)
        {
            int new_poll_capacity = (player_count + 1) * 2; // grow strategy
            printf("Resizing pollfd array from %d to %d\n", pollfd_capacity, new_poll_capacity);
            struct pollfd *temp_pfds = realloc(pfds, new_poll_capacity * sizeof(struct pollfd));
            if (!temp_pfds)
            {
                perror("Failed to reallocate pollfd array");
            }
            else
            {
                pfds = temp_pfds;
                pollfd_capacity = new_poll_capacity;
            }
        }

        // add Listener
        pfds[nfds].fd = listen_fd;
        pfds[nfds].events = POLLIN;
        nfds++;

        // add CONNECTED players
        for (int i = 0; i < player_count; ++i)
        {
            if (players[i].state == CONNECTED)
            {
                // ensure we don't exceed current pollfd capacity even if realloc failed
                if (nfds < pollfd_capacity)
                {
                    pfds[nfds].fd = players[i].fd;
                    pfds[nfds].events = POLLIN;
                    nfds++;
                }
                else
                {
                    fprintf(stderr, "Warning: Exceeded pollfd capacity - cannot monitor player %d\n", players[i].fd);
                }
            }
        }

        int poll_ready = poll(pfds, nfds, 1000); // poll with 1-second timeout to check 'running' flag

        if (!running)
            break; // check flag after poll

        if (poll_ready < 0)
        {
            if (errno == EINTR)
                continue;
            perror("Main poll failed");
            break; // exit loop on main poll error
        }

        if (poll_ready == 0)
        {
            continue; // timeout, loop again
        }

        // 1. check Listener (always at index 0)
        if (pfds[0].revents & POLLIN)
        {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);
            int client_fd = accept(listen_fd, (struct sockaddr *)&client_addr, &client_len);

            if (client_fd < 0)
            {
                if (errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK)
                { // EAGAIN/EWOULDBLOCK might happen if listener becomes non-blocking
                    perror("Accept failed");
                }
            }
            else
            {
                // add player, state starts as CONNECTED. It will be polled next iteration
                int new_player_idx = add_player(client_fd, client_addr);
                if (new_player_idx < 0)
                {
                    // add_player already closed fd and logged error
                }
            }
            poll_ready--; // decrement count of ready fds
        }

        // 2. check CONNECTED players (start from index 1 in pfds)
        for (int pfd_idx = 1; pfd_idx < nfds && poll_ready > 0; ++pfd_idx)
        {
            if (pfds[pfd_idx].revents)
            {
                poll_ready--; // processed one ready fd

                int current_fd = pfds[pfd_idx].fd;
                // find the corresponding player index
                int player_idx = -1;
                for (int k = 0; k < player_count; ++k)
                {
                    if (players[k].fd == current_fd && players[k].state == CONNECTED)
                    {
                        player_idx = k;
                        break;
                    }
                }

                if (player_idx == -1)
                {
                    // should not happen if pfds array is built correctly
                    fprintf(stderr, "Error: Polled fd %d not found in CONNECTED players list!\n", current_fd);
                    close(current_fd); // close the unknown fd
                    continue;
                }

                // handle HUP/ERR first
                if (pfds[pfd_idx].revents & (POLLHUP | POLLERR | POLLNVAL))
                {
                    printf("Player fd %d (idx %d) poll error/hup. Removing.\n", current_fd, player_idx);
                    remove_player_by_index(player_idx);
                }
                // handle data ready
                else if (pfds[pfd_idx].revents & POLLIN)
                {
                    char temp_buf[RPSD_BUF_SIZE];
                    ssize_t bytes_read = recv(current_fd, temp_buf, sizeof(temp_buf) - 1, 0);

                    if (bytes_read > 0)
                    {
                        // check for buffer overflow BEFORE appending
                        if (players[player_idx].read_buf_len + bytes_read >= RPSD_BUF_SIZE)
                        {
                            fprintf(stderr, "Error: Initial read buffer overflow fd %d. Removing.\n", current_fd);
                            remove_player_by_index(player_idx);
                            continue; // move to next pfd
                        }

                        // append data
                        memcpy(players[player_idx].read_buf + players[player_idx].read_buf_len, temp_buf, bytes_read);
                        players[player_idx].read_buf_len += bytes_read;
                        players[player_idx].read_buf[players[player_idx].read_buf_len] = '\0'; // Keep null-terminated

                        // check if the initial 'P|Name||' message is complete
                        char *delimiter = strstr(players[player_idx].read_buf, "||");
                        if (delimiter)
                        {
                            size_t message_len = (delimiter - players[player_idx].read_buf) + 2;
                            char initial_msg[RPSD_BUF_SIZE]; // temp buffer to hold extracted message

                            // check if message fits (should always if RPSD_BUF_SIZE check passed)
                            if (message_len >= sizeof(initial_msg))
                            {
                                fprintf(stderr, "Error: Extracted message too long (should not happen) fd %d. Removing.\n", current_fd);
                                remove_player_by_index(player_idx);
                                continue;
                            }

                            // copy message out
                            memcpy(initial_msg, players[player_idx].read_buf, message_len);
                            initial_msg[message_len] = '\0';

                            // consume message from player's buffer
                            players[player_idx].read_buf_len -= message_len;
                            memmove(players[player_idx].read_buf, players[player_idx].read_buf + message_len, players[player_idx].read_buf_len);
                            memset(players[player_idx].read_buf + players[player_idx].read_buf_len, 0, RPSD_BUF_SIZE - players[player_idx].read_buf_len);

                            // process the Complete Initial Message
                            printf("Processing initial message from fd %d: %s\n", current_fd, initial_msg);
                            if (initial_msg[0] == 'P' && initial_msg[1] == '|')
                            {
                                char *name_start = initial_msg + 2;
                                char *name_end = strstr(name_start, "||"); // should be at the end
                                if (name_end && (name_end - name_start) < MAX_NAME_LEN - 1)
                                {
                                    *name_end = '\0'; // null terminate name part
                                    if (strlen(name_start) == 0)
                                    {
                                        fprintf(stderr, "Player fd %d provided empty name. Removing.\n", current_fd);
                                        remove_player_by_index(player_idx);
                                    }
                                    else
                                    {
                                        // store name
                                        strncpy(players[player_idx].name, name_start, MAX_NAME_LEN - 1);
                                        players[player_idx].name[MAX_NAME_LEN - 1] = '\0';
                                        printf("Received PLAY from fd %d: Name='%s'\n", current_fd, players[player_idx].name);

                                        // check for duplicates
                                        int duplicate_idx = find_active_player_by_name(players[player_idx].name);
                                        if (duplicate_idx != -1)
                                        {
                                            fprintf(stderr, "Duplicate login: '%s' already active (fd %d). Removing new connection %d.\n",
                                                    players[player_idx].name, players[duplicate_idx].fd, current_fd);
                                            char msg_to_send[50];
                                            snprintf(msg_to_send, sizeof(msg_to_send), "R|L|Logged in||");
                                            send_message(current_fd, msg_to_send);
                                            remove_player_by_index(player_idx);
                                        }
                                        else
                                        {
                                            // send Wait message
                                            char msg_to_send[10];
                                            snprintf(msg_to_send, sizeof(msg_to_send), "W|1||");
                                            if (send_message(current_fd, msg_to_send) < 0)
                                            {
                                                // failed to send W, remove player
                                                remove_player_by_index(player_idx);
                                            }
                                            else
                                            {
                                                // success! Player is now waiting
                                                players[player_idx].state = WAITING;
                                                printf("Player %s (fd %d) state changed to WAITING.\n", players[player_idx].name, current_fd);

                                                // attempt pairing ,ogic
                                                if (waiting_player_idx != -1)
                                                {
                                                    if (waiting_player_idx < player_count && players[waiting_player_idx].state == WAITING)
                                                    {
                                                        int opponent_idx = waiting_player_idx;
                                                        // current player is player_idx
                                                        printf("Pairing %s (idx %d) with %s (idx %d)\n",
                                                               players[player_idx].name, player_idx, // corrected index
                                                               players[opponent_idx].name, opponent_idx);

                                                        pid_t pid = fork();
                                                        if (pid < 0)
                                                        { // fork failed handling
                                                            perror("Fork failed");
                                                            send_message(players[player_idx].fd, "Server Error||");
                                                            remove_player_by_index(player_idx);
                                                            int opp_fd = players[opponent_idx].fd;
                                                            int opp_cur_idx = find_player_by_fd(opp_fd);
                                                            if (opp_cur_idx != -1)
                                                            {
                                                                send_message(players[opp_cur_idx].fd, "Server Error||");
                                                                remove_player_by_index(opp_cur_idx);
                                                            }
                                                            waiting_player_idx = -1;
                                                        }
                                                        else if (pid == 0)
                                                        { // child
                                                            // close listener fd in child IF NEEDED (handle_game does this)
                                                            handle_game(players[player_idx].fd, players[player_idx].name,
                                                                        players[opponent_idx].fd, players[opponent_idx].name);
                                                            exit(EXIT_FAILURE);
                                                        }
                                                        else
                                                        { // parent
                                                            printf("Parent %d: Forked child %d for game (%s vs %s).\n", getpid(), pid,
                                                                   players[player_idx].name, players[opponent_idx].name);

                                                            // parent marks players as PLAYING in its list
                                                            players[player_idx].state = PLAYING;
                                                            players[opponent_idx].state = PLAYING;
                                                            players[player_idx].opponent_idx = opponent_idx;
                                                            players[opponent_idx].opponent_idx = player_idx;

                                                            // parent closes its copies of the FDs
                                                            printf("Parent closing its fd %d (for %s)\n", players[player_idx].fd, players[player_idx].name);
                                                            close(players[player_idx].fd);
                                                            printf("Parent closing its fd %d (for %s)\n", players[opponent_idx].fd, players[opponent_idx].name);
                                                            close(players[opponent_idx].fd);

                                                            players[player_idx].fd = -1;   // mark parent's copy as invalid
                                                            players[opponent_idx].fd = -1; // mark parent's copy as invalid

                                                            waiting_player_idx = -1; // reset waiting player
                                                        }
                                                    }
                                                    else
                                                    { // waiting player disappeared
                                                        printf("Waiting player (idx %d) unavailable. %s now waiting.\n",
                                                               waiting_player_idx, players[player_idx].name);
                                                        waiting_player_idx = player_idx; // this player becomes the new waiting player
                                                    }
                                                }
                                                else
                                                { // no one was waiting
                                                    printf("Player %s (fd %d) is now waiting.\n", players[player_idx].name, current_fd);
                                                    waiting_player_idx = player_idx;
                                                }
                                            }
                                        }
                                    }
                                }
                                else
                                { // malformed P message or name too long
                                    fprintf(stderr, "Malformed P msg/name too long fd %d. Removing.\n", current_fd);
                                    remove_player_by_index(player_idx);
                                }
                            }
                            else
                            { // message was not P|...||
                                fprintf(stderr, "Expected P first fd %d, got %s. Removing.\n", current_fd, initial_msg);
                                remove_player_by_index(player_idx);
                            }
                        }
                        // else: delimiter not found yet, wait for more data
                    }
                    else if (bytes_read == 0)
                    {
                        // client disconnected before sending P
                        printf("Player fd %d (idx %d) disconnected before sending P. Removing.\n", current_fd, player_idx);
                        remove_player_by_index(player_idx);
                    }
                    else
                    { // bytes_read < 0
                        if (errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK)
                        {
                            perror("Recv failed for initial message");
                            remove_player_by_index(player_idx);
                        }
                        // EINTR/EAGAIN just means try again later
                    }
                }
            }
        }

        // check if the designated waiting player has disconnected OR finished game
        if (waiting_player_idx != -1)
        {
            if (waiting_player_idx >= player_count || players[waiting_player_idx].state != WAITING)
            {
                printf("Waiting player (idx %d) gone or state changed.\n", waiting_player_idx);
                waiting_player_idx = -1;
            }
        }
    }

    // free the pollfd array
    free(pfds);

    cleanup_and_exit(EXIT_SUCCESS);
    return 0;
}