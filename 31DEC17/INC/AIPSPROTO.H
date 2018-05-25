/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998, 2000                                    */
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
/*  define prototypes for AIPS functions                             */
/*  includes only those called by another C routine to avoid long    */
/*  lists                                                            */

#if defined(__STDC__)                     /* only semi-ANSI C        */

                                          /* AIPS non-Z Functions    */
   void acount_ ( int * ) ;
   void zdie_ ( ) ;
                                          /* C AIPS Functions        */

#ifndef Z_abors__
   void zabors_ ( int ) ;
#endif
#ifndef Z_crlog__
   void zcrlog_ ( int *, char*, int *, char *, int * ) ;
#endif
#ifndef Z_daopn__
   void zdaopn_ ( int *, char *, int *, int *, int * ) ;
#endif
#ifndef Z_delay__
   void zdelay_ ( float *, int * ) ;
#endif
#ifndef Z_lock__
   void zlock_  ( int *, char *, int *, int *, int * );
#endif
#ifndef Z_m70m2__
   void zm70m2_ ( int *, int * ) ;
#endif
#ifndef Z_mktmp__
   void zmktmp_ ( int *, char *, int * ) ;
#endif
#ifndef Z_msgwr__
   void zmsgwr_ ( char *, int * ) ;
#endif
#ifndef Z_onlin__
   int olopen_  ( int *, char *, int ) ;
   int olread_  ( int *, char *, int ) ;
   int olclose_ ( int * ) ;
#endif
#ifndef Z_path__
   void zpath_  ( int *, char *, int *, char *, int *, int * ) ;
#endif
#ifndef Z_ignal__
   void (*zignal_ ( int, void (*)(int) ))( int ) ;
#endif
#ifndef Z_staip__
   void zstaip_ ( ) ;
#endif
#ifndef Z_trlo2__
   void ztrlo2_ ( int *, char *, int *, char *, int *, int * ) ;
#endif
#ifndef Z_winc__
   void zwinc_ ( ) ;
#endif
#ifndef Z_winch__
   void zwinch_ ( int ) ;
#endif

#else                                     /* non-ANSI functions only */

                                          /* AIPS non-Z Functions    */
   void acount_ (/* int * */) ;
   void zdie_ ( ) ;
                                          /* C AIPS Functions        */
#ifndef Z_abors__
   void zabors_ (/* int */) ;
#endif
#ifndef Z_crlog__
   void zcrlog_ (/* int *, char*, int *, char *, int * */) ;
#endif
#ifndef Z_daopn__
   void zdaopn_ (/* int *, char *, int *, int *, int * */) ;
#endif
#ifndef Z_delay__
   void zdelay_ (/* float *, int * */) ;
#endif
#ifndef Z_lock__
   void zlock_  (/* int *, char *, int *, int *, int * */);
#endif
#ifndef Z_m70m2__
   void zm70m2_ (/* int *, int * */) ;
#endif
#ifndef Z_mktmp__
   void zmktmp_ (/* int *, char *, int * */) ;
#endif
#ifndef Z_msgwr__
   void zmsgwr_ (/* char *, int * */) ;
#endif
#ifndef Z_onlin__
   int olopen_  (/* int *, char *, int */) ;
   int olread_  (/* int *, char *, int */) ;
   int olclose_ (/* int * */) ;
#endif
#ifndef Z_path__
   void zpath_  (/* int *, char *, int *, char *, int *, int * */) ;
#endif
#ifndef Z_ignal__
   void (*zignal_ (/* int, void (*)(int) */))(/* int */) ;
#endif
#ifndef Z_staip__
   void zstaip_ ( ) ;
#endif
#ifndef Z_trlo2__
   void ztrlo2_ (/* int *, char *, int *, char *, int *, int * */) ;
#endif
#ifndef Z_winc__
   void zwinc_ ( ) ;
#endif
#ifndef Z_winch__
   void zwinch_ (/* int */) ;
#endif


#endif                           /* __STDC__        */


#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXAPATH 256                    /* Maximum path name length.  */

#define MAXEHEX 36                      /* Maximum number of EHEX     */
                                        /* (base 36) values: [0-9A-Z] */
