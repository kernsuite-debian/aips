      SUBROUTINE GRDSET (FFTIM, SCRGRD, SCRWRK, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Creates scratch files and sets up for GRDSUB
C# UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2006, 2008-2009, 2012
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
C   GRDSET sets up for GRDSUB, mostly creates scratch files (type SC)
C   Also fills in LUNS array in /CFILES/ = 16,17,18,19,20
C   Input:
C      FFTIM    L        Doing image or CC?
C   Output:
C      SCRGRD   I        /CFILES/ file number for grid file.
C      SCRWRK   I        /CFILES/ file number for work file
C      BUFFER   I(512)   Work buffer.
C      IRET     I        Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      LOGICAL   FFTIM
      INTEGER   SCRGRD, SCRWRK, BUFFER(512), IRET
C
      INTEGER   NX, NY, NP(5), NCVSIZ, IFIELD, SIZE, SIZE2, MX, MY,
     *   MXOFF, MYOFF, II
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
C-----------------------------------------------------------------------
C                                       Setup.
      NCVSIZ = 10
C                                       Find largest input file.
      NX = 1
      NY = 1
      DO 10 IFIELD = 1,MFIELD
         CALL CATIO ('READ', CCDISK(IFIELD), CCCNO(IFIELD), BUFFER,
     *      'REST', BUFFER(257), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, IFIELD
            GO TO 990
            END IF
         IF (FFTIM) THEN
            CALL IMGSIZ (BUFFER, BUFFER, MX, MY, MXOFF, MYOFF, IRET)
         ELSE
            MX = BUFFER(KINAX)
            MY = BUFFER(KINAX+1)
            END IF
         NX = MAX (NX, MX)
         NY = MAX (NY, MY)
 10      CONTINUE
      CALL POWER2 (NX, II)
      IF (II.LT.NX) NX = 2 * II
      CALL POWER2 (NY, II)
      IF (II.LT.NY) NY = 2 * II
C                                       Grid. file.
C                                       Increase size if possible for
C                                       interpolation
      IF (NX.LE.8192) NX = 2 * NX
      IF (NY.LE.8192) NY = 2 * NY
C                                       try two estimates of size:
      NP(1) = 2 * NY + 2 * NCVSIZ
      NP(2) = NX / 2 + 1 + 2 * NCVSIZ
      CALL MAPSIZ (2, NP, SIZE2)
C
      NP(1) = MIN (NX, NY) + 2
      NP(2) = MAX (NX, NY) + 2
C                                       Add extra rows for bandwidth
C                                       synthesis.
      NP(1) = NP(1) + 2 * NCVSIZ
C                                       Determine file size
      CALL MAPSIZ (2, NP, SIZE)
      SIZE = MAX (SIZE, SIZE2)
C                                       Make GRID (SCRGRD) file.
      CALL SCREAT (SIZE, BUFFER, IRET)
      SCRGRD = NSCR
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'TOO LITTLE DISK SPACE FOR SCRATCH FILE'
         ELSE
            WRITE (MSGTXT,1011) IRET
            END IF
         GO TO 990
         END IF
C                                       WORK file (SCRWRK)
 40   CALL SCREAT (SIZE, BUFFER, IRET)
      SCRWRK = NSCR
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'TOO LITTLE DISK SPACE FOR SCRATCH FILE'
         ELSE
            WRITE (MSGTXT,1011) IRET
            END IF
         GO TO 990
         END IF
C                                       Fill LUNS array in /CFILES/
 50   LUNS(1) = 16
      LUNS(2) = 17
      LUNS(3) = 18
      LUNS(4) = 19
      LUNS(5) = 20
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRDSET: ERROR',I5,' READING CATBLK FIELD',I3)
 1011 FORMAT ('GRDSET: ERROR',I5,' CREATING SCRATCH FILE')
      END
