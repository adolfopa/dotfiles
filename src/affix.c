#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

typedef enum { PREFIX, INFIX, SUFFIX } affix_t;

/*
 * Parse command line arguments, storing the chosen delimiter, file
 * name and affix type into `delim', `fname' and `affix_type`.
 *
 * This function will exit on any error. On successfull return,
 * callers may assume both `delim', `fname' and `affix_type` have
 * valid (non-NULL) values.
 */
static void
parse_cmdline(int argc, char **argv, char *delim, char **fname,
              affix_t *affix_type)
{
     assert(argc > 0);
     assert(argv != NULL);
     assert(delim != NULL);
     assert(fname != NULL);

     if (argc == 1)
          exit(EX_USAGE);

     *delim = '.';
     *affix_type = PREFIX;

     int ch;

     while ((ch = getopt(argc, argv, "d:ips")) != -1) {
          switch (ch) {
          case 'd':
               *delim = *optarg;
               break;
          case 'i':
               *affix_type = INFIX;
               break;
          case 'p':
               *affix_type = PREFIX;
               break;
          case 's':
               *affix_type = SUFFIX;
               break;
          case '?':
               exit(EX_USAGE);
          }
     }

     if (optind == argc)
          exit(EX_USAGE);

     *fname = argv[optind];

     assert(*fname != NULL);
}

int
main(int argc, char **argv)
{
     char delim;
     char *fname;
     affix_t affix_type;

     parse_cmdline(argc, argv, &delim, &fname, &affix_type);

     assert(fname != NULL);
     assert(affix_type == PREFIX
            || affix_type == INFIX
            || affix_type == SUFFIX);

     char *p = NULL;

     if (affix_type == PREFIX || affix_type == INFIX) {
          p = strrchr(fname, delim);

          if (p != NULL)
               *p = '\0';
     }

     if (affix_type == SUFFIX || affix_type == INFIX) {
          p = strchr(fname, delim);

          if (p != NULL)
               fname = ++p;
     }

     if (puts(fname) == EOF) {
          exit(EX_IOERR);
     }

     return EXIT_SUCCESS;
}
