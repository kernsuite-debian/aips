/* FillSlide */
/*-----------------------------------------------------------------*/
/*! Scales an encapsolated postscript file to fill a 35 mm slide   */
/*# SYS UNIX                                                       */
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
/*-----------------------------------------------------------------*/
/* FillSlide scales and orients encapsolated postscript to fill    */
/* the 2 to 3 size ratio 35 mm slide                               */
/*                                                                 */
/*   Inputs:                                                       */
/*      black/white/grey flag                                      */
/*      Piped            postscript file                           */
/*   Output:                                                       */
/*      Standard output postscript file                            */
/* FillSlide assumes "xgrab" encapsolated postscript (PS) output   */
/* The PS is rotated to fill a 35 mm slide image with a zoomed and */
/* centered rendition of a postscript file.                        */
/* The slide is assumed to have an aspect ratio of 2 to 3 and that */
/* the maximum width is 800 units.                                 */
/* A critical assumption is that the PS file contains a line with  */
/* three elements, 1) x size, 2) y size and 3) the word "scale"    */
/* when this line is found, the PS is modified else the PS file is */
/* passed unchanged.                                               */
/* # Last editted by glen langston on 1992 Sept 21                 */
/*-----------------------------------------------------------------*/

#include <stdio.h>
#ifdef __STDC__
#include <string.h>
#else
#include <strings.h>
#endif

main(argc, argv)
int argc;
char *argv[];
{
   char line[132], first[132], second[132], scale[132];
   register int i;
   int x, y, xmax=533, ymax=800, once=0, in_ok=1, black=0;
   float xyfact, fillfact=0.92;

                                        /* Check number of arguents   */
   if (argc < 1 || argc > 2) in_ok=0;
                                        /* Check bacground greyscale  */
   if (argc == 2) {
      sscanf (argv[1],"%d",&black); 
      if (strcmp( argv[1], "white") == 0) black = 0;
      if (strcmp( argv[1], "grey")  == 0) black = 50;
      if (strcmp( argv[1], "black") == 0) black = 100;
      if (black < 0 || black > 100) in_ok = 0;
      }
   fprintf(stderr,"Background color is %d %% black\n",black);

   if (in_ok == 0) {
      printf("Usage: cat <yourfile> | FILLSLIDE.EXE 100 \n");
      printf("       Where the background color ranged from 0 - white\n");
      printf("                                         to 100 - black\n");
      exit(1);
   }
                                        /* While not end of file      */
   while (gets(line,132,stdin) != NULL) {
      sscanf(line,"%s%s%s", first, second, scale);
                                        /* If the first scale line    */
      if (strcmp(scale,"scale") == 0 && once == 0) {
         sscanf(line,"%d %d %s", &x, &y, scale);
         printf("%% begin inserted rescaling: %s\n", line);
         printf("/xmax  %4d def         /ymax  %4d def\n",xmax, ymax);
         printf("/xsize %4d def         /ysize %4d def\n",x, y);
                                        /* if coloring background     */
         if (black != 0) {
            printf("gsave newpath 0 0 moveto ");
            printf("0 ymax rlineto xmax 0 rlineto\n");
            printf("0 ymax -1 mul rlineto xmax -1 mul 0 rlineto \n");
                                        /* if completely black        */
            if (black == 100) 
               printf("closepath fill grestore \n");
            else
               printf("closepath %f setgray fill grestore \n", 
                  (float)black/100.);
            }
                                        /* portrait or landscape?     */
         if ( x < y) {
                                        /* higher than wide, use port.*/
            if ((float)x/(float)xmax < (float)y/(float)ymax)
               xyfact = (float)y/(float)ymax;
            else
               xyfact = (float)x/(float)xmax;
            printf("/xyfact %f def\n", xyfact);
            printf("/xscal xsize %f mul xyfact div def\n",fillfact);
            printf("/yscal ysize %f mul xyfact div def\n",fillfact);
            printf("/xtran xmax xscal sub 2 div def\n");
            printf("/ytran ymax yscal sub 2 div def\n");
            printf("xtran ytran translate xscal yscal scale\n");
            }
         else  {
                                        /*wider than high: landscape  */
            if ((float)y/(float)xmax < (float)x/(float)ymax)
               xyfact = (float)x/(float)ymax;
            else
               xyfact = (float)y/(float)xmax;
            printf("/xyfact %f def\n", xyfact);
            printf("/xscal ysize %f mul xyfact div def\n",fillfact);
            printf("/yscal xsize %f mul xyfact div def\n",fillfact);
            printf("/xtran xmax xmax xscal sub 2 div sub def\n");
            printf("/ytran ymax yscal sub 2 div def\n");
            printf("xtran ytran translate xscal yscal scale 90 rotate\n");
            }
         printf("%%%% end inserted rescaling\n");
                                        /* flag rescaling done        */
         once=1;
         }
      else
                                        /* Else just pass normal lines*/
         printf("%s\n",line);
   }
   exit(0);
}

