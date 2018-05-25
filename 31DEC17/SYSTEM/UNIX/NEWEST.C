/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

main(argc, argv)
int argc;
char *argv[];
{
   char prgnam[256], pathname[256], newest[256], include[80],
      logical[80], incfile[256], incs[1024], incdir[10][256];
   int argn, ntime;
   register int i, j, k;
   struct stat stbuf;
   ntime = 0;

   sprintf(prgnam,"%-10s",argv[0]);
   if (argc < 2)
      fprintf(stderr,"Usage: %s pathnames\n",prgnam);
   else {
      for (argn = 1; argn < argc; argn++) {
         sprintf(pathname,"%s",argv[argn]);
         if ((i = sindex(pathname,":")) != -1) {
                                        /* Prefixed with logical.     */
                                        /* Probably an INCLUDE file.  */
                                        /* Separate logical and       */
                                        /* filename.                  */
            for (j = 0; j < i; j++)
               logical[j] = pathname[j];
            logical[j] = '\0';
            for (j = i + 1, k = 0; pathname[j] != '\0'; j++, k++)
               incfile[k] = pathname[j];
            incfile[k] = '\0';
/*
            printf("logical = %s  incfile = %s\n",
               logical,incfile);
*/
                                        /* Translate logical (should  */
                                        /* be a search path of blank  */
                                        /* delimited directories).    */
            sprintf(incs,"%s",getenv(logical));
/*
            printf("incs = %s\n",incs);
*/
                                        /* Cycle through search path  */
                                        /* looking for 'incfile'.     */
            for (i = 0; i < 10; i++)
               sprintf(incdir[i],"");
            sscanf(incs,"%s%s%s%s%s%s%s%s%s%s",
               incdir[0],incdir[1],incdir[2],incdir[3],incdir[4],
               incdir[5],incdir[6],incdir[7],incdir[8],incdir[9]);
/*
            for (i = 0; i < 10 && incdir[i][0] != '\0'; i++)
               printf("%s: incdir[%d] = :%s:\n",prgnam,i,incdir[i]);
*/
            for (i = 0; i < 10; i++) {
               if (incdir[i][0] == '\0') {
                                        /* Out of directories to     */
                                        /* search.                   */
                  fprintf(stderr,"%s: can't find %s in %s",
                     prgnam,incfile,incdir[0]);
                  for (j = 1; j < i; j++)
                     fprintf(stderr," or %s",incdir[j]);
                  fprintf(stderr,"\n");
                  exit(1);
               }
               else {
                                        /* Form null terminated       */
                                        /* pathname of file.          */
                  sprintf(pathname,"%s/%s",incdir[i],incfile);
/*
                  printf("%s: pathname = %s\n",prgnam,pathname);
*/
                  if (access(pathname,0) == 0) {
                                        /* Found it.                  */
/*
                     printf("%s: found %s\n",prgnam,pathname);
*/
                     break;
                  }
               }
            }
         }
                                        /* Get last time modified.    */
         if (stat(pathname, &stbuf) == -1)
/*
            perror(prgnam);
*/
            continue;
         else {
/*
            printf("%s: pathname = :%s:  stbuf.st_mtime = %d\n",
               prgnam,pathname,stbuf.st_mtime);
*/
            if (stbuf.st_mtime > ntime) {
               sprintf(newest,"%s",pathname);
               ntime = stbuf.st_mtime;
            }
         }
      }
      printf("%s\n",newest);
   }
}
sindex(s1, s2)
char s1[], s2[];
{
   register int i, j, k;

   for (i = 0; s1[i] != '\0'; i++) {
      for (j = i, k = 0; s2[k] != '\0' && s1[j] == s2[k]; j++, k++);
      if (s2[k] == '\0')
                                        /* Found.                     */
         return(i);
   }
                                        /* Not found.                 */
   return(-1);

}
