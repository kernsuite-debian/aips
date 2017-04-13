      SUBROUTINE QMENT (APBASE, DOGRDS, IMG, DEF, RES, STP, NELEM)
C-----------------------------------------------------------------------
C! FPS AP version: Gradients and their dot products for MEM tasks.
C# AP-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C Q-Routine for Maximum entropy programs. Calculates gradients
C and their dot products.
C Input :
C   APBASE   I        Base address in PS-AP, all others relative
C   DOGRDS   I        Do gradients ? If > 0 then yes else no.
C   IMG      I        Pointer in PS-AP to MEM image line
C   DEF      I        Pointer in PS-AP to default image line
C   RES      I        Pointer in PS-AP to residual image line
C   STP      I        Pointer in PS-AP to step image line
C   NELEM    I        Number of elements in a line
C Input via AP :
C   Location        Variable
C      0            - ALPHA
C      1            - BETA
C      2              GGC
C      3              1.0
C Input/Output via AP :
C   Location        Variable
C      4             GRDHSQ
C      5             GRDESQ
C      6             GRDFSQ
C      7             GRDHE
C      8             GRDHF
C      9             GRDEF
C     10             FLUX
C     11              RJ1
C    STP        step in mem image
C    All locations are relative to APBASE. Location 12 is used for
C scratch. Locations 13-16 are free for future use.
C Programmer : T.J. Cornwell                   January 1986
C-----------------------------------------------------------------------
      INTEGER   APBASE, DOGRDS, IMG,  DEF,  RES,  STP,  NELEM
      INTEGER*2 IAPBAS, IDOGRD, IIMG, IDEF, IRES, ISTP, INELEM
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (APBASE.LT.32768) IAPBAS = APBASE
      IF (APBASE.GE.32768) IAPBAS = APBASE - 65536
      IF (DOGRDS.LT.32768) IDOGRD = DOGRDS
      IF (DOGRDS.GE.32768) IDOGRD = DOGRDS - 65536
      IF (IMG.LT.32768) IIMG = IMG
      IF (IMG.GE.32768) IIMG = IMG - 65536
      IF (DEF.LT.32768) IDEF = DEF
      IF (DEF.GE.32768) IDEF = DEF - 65536
      IF (RES.LT.32768) IRES = RES
      IF (RES.GE.32768) IRES = RES - 65536
      IF (STP.LT.32768) ISTP = STP
      IF (STP.GE.32768) ISTP = STP - 65536
      IF (NELEM.LT.32768) INELEM = NELEM
      IF (NELEM.GE.32768) INELEM = NELEM - 65536
C                                        Call FPS routine.
      CALL MENT (IAPBAS, IDOGRD, IIMG, IDEF, IRES, ISTP, INELEM)
C
 999  RETURN
      END
