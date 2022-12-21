#include "scc.h"

FILE* output_file;
char* input_file;
int enable_warnings;

static void
usage()
{
  printf("scc <input.c> [-o out]\n");
  exit(1);
}

static void
parse_options(int argc, char** argv)
{
  enable_warnings = 1;

  int i = 1;
  if (argc < 2) {
    usage();
  }

  while (i < argc) {
    if (strcmp("-h", argv[i]) == 0 || strcmp("--help", argv[i]) == 0) {
      usage();
    }

    // gcc/clang: -w disables all warnings
    if (strcmp("-w", argv[i]) == 0) {
      enable_warnings = 0;
      i++;
    }

    if (strcmp("-o", argv[i]) == 0) {
      if (output_file) {
        printf("error: multiple output files declared\n");
        exit(1);
      }

      i++;
      if (i >= argc) {
        printf("error: argument to -o required\n");
        exit(1);
      }
      output_file = fopen(argv[i++], "w");
      if (!output_file) {
        perror("error: failed to open output file");
        exit(1);
      }

      continue;
    }

    if (!input_file) {
      input_file = argv[i++];
    } else {
      printf("error: multiple input files declared\n");
      exit(1);
    }
  }
}

/**
 * SCC is not (yet) a full C compiler in that it does not provide a
 * preprocessor.
 *
 * Until it does, we rely on another C compiler to preprocess C source that we
 * can feed as an input to SCC. We do this by forking off a child process that
 * invokes a C compiler and feeds its stdout to our tokenizer.
 */
static FILE*
setup_preprocessor(char* filename)
{
  int pipefd[2];
  if (pipe(pipefd) != 0) {
    perror("failed to open pipe");
    exit(1);
  }

  // We open the source file in the parent process to issue an error earlier if
  // the file doesn't exist.
  FILE* f = fopen(filename, "r");
  if (!f) {
    perror("failed to open source file");
    exit(1);
  }

  int child_pid = fork();
  if (child_pid == -1) {
    perror("failed to fork");
    exit(1);
  }

  if (child_pid == 0) {
    // The child side of the fork. The child writes preprocessed C source to the
    // pipe.
    close(pipefd[0]);
    dup2(pipefd[1], 1 /* stdout */);
    close(pipefd[1]);
    char* args[] = {
      "/usr/bin/gcc", "-E", "-P", "-D__SCC__=1", filename, NULL
    };
    execv("/usr/bin/gcc", args); // does not return
    exit(0);
  } else {
    // The parent side of the fork. The parent sets the tokenizer's FILE to be
    // the read end of the pipe coming from the C preprocessor.
    close(pipefd[1]);
    fclose(f);
    return fdopen(pipefd[0], "r");
  }
}

int
main(int argc, char** argv)
{
  parse_options(argc, argv);
  if (!input_file) {
    printf("error: no input filu\n");
    exit(1);
  }

  if (!output_file) {
    output_file = stdout;
  }

  FILE* stream = setup_preprocessor(input_file);
  load_file(argv[1], stream);
  token* tok = tokenize();
  symbol* sym = parse(&tok);
  codegen(sym);
}