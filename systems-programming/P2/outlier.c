#include <stdio.h>
#include <fcntl.h>    // for open()
#include <unistd.h>   // for read() and close()
#include <sys/stat.h> // for stat()
#include <dirent.h>   // for directory operations
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>

#define BUFFER_SIZE 1024
#define INITIAL_CAPACITY 16

// struct to store a word and its count
typedef struct
{
    char *word;
    int count;
} WordCount;

// struct to store data for one file
typedef struct
{
    char *filename;   // file name or path
    WordCount *words; // array of words in the file
    int unique_words; // number of unique words
    int capacity;     // capacity of words array
    int total_words;  // total valid words in file
} FileInfo;

// global list of files and global word counts
FileInfo *fileList = NULL;
int numFiles = 0;
int fileListCapacity = 0;

WordCount *globalWords = NULL;
int numGlobalWords = 0;
int globalCapacity = 0;
int globalTotalWords = 0;

// function prototypes
void addWordToFile(FileInfo *file, const char *word);
void addWordGlobal(const char *word);
WordCount *findGlobalWord(const char *word);
void processFile(const char *path);
void traverseDir(const char *path);

// add a word to a file's word list (or update its count)
void addWordToFile(FileInfo *file, const char *word)
{
    // check if word is already present
    for (int i = 0; i < file->unique_words; i++)
    {
        if (strcmp(file->words[i].word, word) == 0)
        {
            file->words[i].count++;
            return;
        }
    }
    // expand the array if needed
    if (file->unique_words >= file->capacity)
    {
        if (file->capacity == 0)
        {
            file->capacity = INITIAL_CAPACITY;
        }
        else
        {
            file->capacity = file->capacity * 2;
        }
        file->words = realloc(file->words, file->capacity * sizeof(WordCount));
        if (!file->words)
        {
            perror("realloc");
            exit(EXIT_FAILURE);
        }
    }
    // add new word
    file->words[file->unique_words].word = strdup(word);
    if (!file->words[file->unique_words].word)
    {
        perror("strdup");
        exit(EXIT_FAILURE);
    }
    file->words[file->unique_words].count = 1;
    file->unique_words++;
}

// add a word to the global word list (or update its count)
void addWordGlobal(const char *word)
{
    for (int i = 0; i < numGlobalWords; i++)
    {
        if (strcmp(globalWords[i].word, word) == 0)
        {
            globalWords[i].count++;
            return;
        }
    }
    if (numGlobalWords >= globalCapacity)
    {
        if (globalCapacity == 0)
        {
            globalCapacity = INITIAL_CAPACITY;
        }
        else
        {
            globalCapacity = globalCapacity * 2;
        }
        globalWords = realloc(globalWords, globalCapacity * sizeof(WordCount));
        if (!globalWords)
        {
            perror("realloc");
            exit(EXIT_FAILURE);
        }
    }
    globalWords[numGlobalWords].word = strdup(word);
    if (!globalWords[numGlobalWords].word)
    {
        perror("strdup");
        exit(EXIT_FAILURE);
    }
    globalWords[numGlobalWords].count = 1;
    numGlobalWords++;
}

// find a word in the global word list
WordCount *findGlobalWord(const char *word)
{
    for (int i = 0; i < numGlobalWords; i++)
    {
        if (strcmp(globalWords[i].word, word) == 0)
        {
            return &globalWords[i];
        }
    }
    return NULL;
}

// process a single file using open() and read()
void processFile(const char *path)
{
    int fd = open(path, O_RDONLY);
    if (fd == -1)
    {
        perror(path);
        return;
    }

    // create a FileInfo object for this file
    FileInfo file;
    file.filename = strdup(path);
    if (!file.filename)
    {
        perror("strdup");
        exit(EXIT_FAILURE);
    }
    file.words = NULL;
    file.unique_words = 0;
    file.capacity = 0;
    file.total_words = 0;

    char buffer[BUFFER_SIZE];
    ssize_t bytesRead;
    // use a dynamic wordBuffer instead of a fixed-size array
    char *wordBuffer = NULL;
    int wordCapacity = 0;
    int wordLen = 0;
    int inWord = 0;
    int hasLetter = 0;

    // read the file in chunks
    while ((bytesRead = read(fd, buffer, BUFFER_SIZE)) > 0)
    {
        for (int i = 0; i < bytesRead; i++)
        {
            char c = buffer[i];
            if (!isspace((unsigned char)c))
            {
                // if not in a word, check if character is allowed to start a word
                if (!inWord)
                {
                    if (c == '(' || c == '[' || c == '{' || c == '"' || c == '\'')
                    {
                        continue; // skip invalid starting character
                    }
                    else
                    {
                        inWord = 1;
                        wordLen = 0;
                        if (isalpha((unsigned char)c))
                            hasLetter = 1;
                        else
                            hasLetter = 0;
                        // ensure space for the character
                        if (wordLen + 1 >= wordCapacity)
                        {
                            if (wordCapacity == 0)
                            {
                                wordCapacity = 32;
                            }
                            else
                            {
                                wordCapacity = wordCapacity * 2;
                            }
                            char *newBuffer = realloc(wordBuffer, wordCapacity * sizeof(char));
                            if (!newBuffer)
                            {
                                perror("realloc");
                                exit(EXIT_FAILURE);
                            }
                            wordBuffer = newBuffer;
                        }
                        wordBuffer[wordLen++] = c;
                    }
                }
                else
                {
                    // append character to current word
                    if (wordLen + 1 >= wordCapacity)
                    {
                        if (wordCapacity == 0)
                        {
                            wordCapacity = 32;
                        }
                        else
                        {
                            wordCapacity = wordCapacity * 2;
                        }
                        char *newBuffer = realloc(wordBuffer, wordCapacity * sizeof(char));
                        if (!newBuffer)
                        {
                            perror("realloc");
                            exit(EXIT_FAILURE);
                        }
                        wordBuffer = newBuffer;
                    }
                    wordBuffer[wordLen++] = c;
                    if (isalpha((unsigned char)c))
                        hasLetter = 1;
                }
            }
            else
            {
                // on whitespace, finish the current word if one is in progress
                if (inWord)
                {
                    // remove trailing punctuation: ) ] } " ' , . ! ?
                    while (wordLen > 0 &&
                           (wordBuffer[wordLen - 1] == ')' || wordBuffer[wordLen - 1] == ']' ||
                            wordBuffer[wordLen - 1] == '}' || wordBuffer[wordLen - 1] == '"' ||
                            wordBuffer[wordLen - 1] == '\'' || wordBuffer[wordLen - 1] == ',' ||
                            wordBuffer[wordLen - 1] == '.' || wordBuffer[wordLen - 1] == '!' ||
                            wordBuffer[wordLen - 1] == '?'))
                    {
                        wordLen--;
                    }
                    // Null-terminate the word
                    if (wordLen + 1 >= wordCapacity)
                    {
                        if (wordCapacity == 0)
                        {
                            wordCapacity = 32;
                        }
                        else
                        {
                            wordCapacity = wordCapacity * 2;
                        }
                        char *newBuffer = realloc(wordBuffer, wordCapacity * sizeof(char));
                        if (!newBuffer)
                        {
                            perror("realloc");
                            exit(EXIT_FAILURE);
                        }
                        wordBuffer = newBuffer;
                    }
                    wordBuffer[wordLen] = '\0';
                    // if the word is valid, add it to file and global counts
                    if (wordLen > 0 && hasLetter)
                    {
                        for (int j = 0; j < wordLen; j++)
                        {
                            wordBuffer[j] = tolower((unsigned char)wordBuffer[j]);
                        }
                        addWordToFile(&file, wordBuffer);
                        addWordGlobal(wordBuffer);
                        file.total_words++;
                        globalTotalWords++;
                    }
                    inWord = 0;
                    wordLen = 0;
                    hasLetter = 0;
                }
            }
        }
    }
    if (bytesRead == -1)
    {
        perror("read");
    }
    // if the file did not end with whitespace, process the last word
    if (inWord)
    {
        while (wordLen > 0 &&
               (wordBuffer[wordLen - 1] == ')' || wordBuffer[wordLen - 1] == ']' ||
                wordBuffer[wordLen - 1] == '}' || wordBuffer[wordLen - 1] == '"' ||
                wordBuffer[wordLen - 1] == '\'' || wordBuffer[wordLen - 1] == ',' ||
                wordBuffer[wordLen - 1] == '.' || wordBuffer[wordLen - 1] == '!' ||
                wordBuffer[wordLen - 1] == '?'))
        {
            wordLen--;
        }
        if (wordLen + 1 >= wordCapacity)
        {
            if (wordCapacity == 0)
            {
                wordCapacity = 32;
            }
            else
            {
                wordCapacity = wordCapacity * 2;
            }
            char *newBuffer = realloc(wordBuffer, wordCapacity * sizeof(char));
            if (!newBuffer)
            {
                perror("realloc");
                exit(EXIT_FAILURE);
            }
            wordBuffer = newBuffer;
        }
        wordBuffer[wordLen] = '\0';
        if (wordLen > 0 && hasLetter)
        {
            for (int j = 0; j < wordLen; j++)
            {
                wordBuffer[j] = tolower((unsigned char)wordBuffer[j]);
            }
            addWordToFile(&file, wordBuffer);
            addWordGlobal(wordBuffer);
            file.total_words++;
            globalTotalWords++;
        }
    }
    free(wordBuffer);
    close(fd);

    // add the processed file to the global file list
    if (numFiles >= fileListCapacity)
    {
        if (fileListCapacity == 0)
        {
            fileListCapacity = INITIAL_CAPACITY;
        }
        else
        {
            fileListCapacity = fileListCapacity * 2;
        }
        fileList = realloc(fileList, fileListCapacity * sizeof(FileInfo));
        if (!fileList)
        {
            perror("realloc");
            exit(EXIT_FAILURE);
        }
    }
    fileList[numFiles++] = file;
}

// recursively traverse a directory and process ".txt" files
void traverseDir(const char *path)
{
    DIR *dir = opendir(path);
    if (!dir)
    {
        perror(path);
        return;
    }
    struct dirent *entry;
    struct stat statBuf;
    char fullPath[1024];

    while ((entry = readdir(dir)) != NULL)
    {
        // skip hidden files and directories (names starting with '.')
        if (entry->d_name[0] == '.')
            continue;
        snprintf(fullPath, sizeof(fullPath), "%s/%s", path, entry->d_name);
        if (stat(fullPath, &statBuf) == -1)
        {
            perror("stat");
            continue;
        }
        if (S_ISDIR(statBuf.st_mode))
        {
            // recursively traverse subdirectories
            traverseDir(fullPath);
        }
        else if (S_ISREG(statBuf.st_mode))
        {
            // process only files ending with ".txt"
            int len = strlen(entry->d_name);
            if (len >= 4 && strcmp(entry->d_name + len - 4, ".txt") == 0)
            {
                processFile(fullPath);
            }
        }
    }
    closedir(dir);
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <file_or_directory>...\n", argv[0]);
        return EXIT_FAILURE;
    }

    struct stat statBuf;
    // process each command-line argument
    for (int i = 1; i < argc; i++)
    {
        if (stat(argv[i], &statBuf) == -1)
        {
            perror(argv[i]);
            continue;
        }
        if (S_ISDIR(statBuf.st_mode))
        {
            traverseDir(argv[i]);
        }
        else if (S_ISREG(statBuf.st_mode))
        {
            // process file even if it doesn't end with ".txt"
            processFile(argv[i]);
        }
    }

    // for each file, find and print the word with the highest relative frequency increase
    for (int i = 0; i < numFiles; i++)
    {
        FileInfo *file = &fileList[i];
        if (file->total_words == 0)
        {
            printf("%s: \n", file->filename);
            continue;
        }
        double bestRatio = 0.0;
        char *bestWord = NULL;
        // check each unique word in file
        for (int j = 0; j < file->unique_words; j++)
        {
            double fileFreq = (double)file->words[j].count / file->total_words;
            WordCount *globalWord = findGlobalWord(file->words[j].word);
            if (globalWord == NULL)
                continue;
            double globalFreq = (double)globalWord->count / globalTotalWords;
            double ratio = fileFreq / globalFreq;
            // update best word if the ratio is higher, or use lexicographic order for ties
            if (ratio > bestRatio ||
                (ratio == bestRatio && (bestWord == NULL || strcmp(file->words[j].word, bestWord) < 0)))
            {
                bestRatio = ratio;
                bestWord = file->words[j].word;
            }
        }
        if (bestWord != NULL)
        {
            printf("%s: %s\n", file->filename, bestWord);
        }
        else
        {
            printf("%s: \n", file->filename);
        }
    }

    // free memory for file data and global word list
    for (int i = 0; i < numFiles; i++)
    {
        FileInfo *file = &fileList[i];
        free(file->filename);
        for (int j = 0; j < file->unique_words; j++)
        {
            free(file->words[j].word);
        }
        free(file->words);
    }
    free(fileList);
    for (int i = 0; i < numGlobalWords; i++)
    {
        free(globalWords[i].word);
    }
    free(globalWords);

    return EXIT_SUCCESS;
}
