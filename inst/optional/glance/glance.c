// mock-up for raster/vector GIS File association with glance()

// build command: 'gcc -o glance glance.c'

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc,char **argv) {
   int verbose=0;
   int i;
   char cmd[2048];
   char *pch;
   int res;
   strcpy(cmd,"Rscript -e ursa::glance()");
   for (i=1;i<argc;i++) {
      pch=strchr(argv[i],' ');
      if (verbose)
         fprintf(stdout,"%d: %s\n",i,argv[i]);
      strcat(cmd," ");
      if (pch!=NULL)
        strcat(cmd,"\"");
      strcat(cmd,argv[i]);
      if (pch!=NULL)
        strcat(cmd,"\"");
   }
   if (verbose)
      fprintf(stdout,"%d: %s\n",i,argv[i]);
   res=system(cmd);
   return res;
}

