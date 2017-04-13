      SUBROUTINE SETGRD (SCRGRD, SCRWRK, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Sets up for gridding uv data.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2012
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
C   SETGRD sets up for the gridding routines, mostly creates scratch
C   files.
C   Inputs from common:
C      MFIELD   I        The number of fields which are going to to be
C                        imaged (excluding any beam). MUST be filled in
C      DO3DIM   L        If true, doing MFIELD beams, else 1 beam
C      FLDSZ    I(2,*)   Dimension of map in RA, Dec (cells) of each
C                        field.  MUST be completely filled in
C                        if files currently do not exist or zeroed
C                        or filled in if they do.
C      NXBEM    I(*)     X dimension of Beam(s)
C      NYBEM    I(*)     Y dimension (cells) of beam(s)
C      CCDISK   I(*)     Disk numbers of the output images.
C                        Must be filled in or zeroed.
C      CCCNO    I(*)     Catalog slot numbers of output images.
C                        Must be filled in or zeroed.
C   Input/Output:
C      SCRGRD   I        /CFILES/ file number for grid file.
C      SCRWRK   I        /CFILES/ file number for work file
C   Output:
C      BUFFER   I(512)   Work buffer.
C      IRET     I        Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   SCRGRD, SCRWRK, BUFFER(512), IRET
C
      INTEGER   MX, MY, NP(5), NCVSIZ, IFIELD, SIZE, II
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
C-----------------------------------------------------------------------
C                                       Setup.
      NCVSIZ = 10
C                                       Find largest input file.
      MX = 1
      MY = 1
      DO 10 IFIELD = 1,MFIELD
         IRET = 3
         IF ((CCDISK(IFIELD).GT.0) .AND. (CCCNO(IFIELD).GT.0))
     *      CALL CATIO ('READ', CCDISK(IFIELD), CCCNO(IFIELD), BUFFER,
     *         'REST', BUFFER(257), IRET)
         IF (IRET.NE.3) THEN
            MX = MAX (MX, BUFFER(KINAX))
            MY = MAX (MY, BUFFER(KINAX+1))
C                                       If not there use FLDSIZ
         ELSE
            MX = MAX (MX, FLDSZ(1,IFIELD))
            MY = MAX (MY, FLDSZ(1,IFIELD))
            IRET = 0
            END IF
         IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
            WRITE (MSGTXT,1000) IRET, IFIELD
            GO TO 990
            END IF
         IF ((IFIELD.EQ.1) .OR. (DO3DIM)) THEN
            MX = MAX (MX, NXBEM(IFIELD))
            MY = MAX (MY, NYBEM(IFIELD))
            END IF
 10      CONTINUE
      CALL POWER2 (MX, II)
      IF (II.LT.MX) MX = 2 * II
      CALL POWER2 (MY, II)
      IF (II.LT.MY) MY = 2 * II
      IRET = 0
C                                       Grid. file.
C                                       Increase size if possible for
C                                       interpolation
      IF (MX.LE.8192) MX = 2 * MX
      IF (MY.LE.8192) MY = 2 * MY
      NP(1) = MIN (MX, MY) + 2
      NP(2) = MAX (MX, MY)
C                                       Add extra rows for bandwidth
C                                       synthesis.
      NP(1) = NP(1) + 2 * NCVSIZ
C                                       Determine file size
      CALL MAPSIZ (2, NP, SIZE)
C                                       Don't create if it already exists
C                                       Make GRID (SCRGRD) file.
      IF (SCRGRD.LE.0) THEN
         CALL SCREAT (SIZE, BUFFER, IRET)
         SCRGRD = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1010)
            IF (IRET.GT.1) WRITE (MSGTXT,1011)
            GO TO 990
            END IF
         END IF
C                                       WORK file (SCRWRK)
      IF (SCRWRK.LE.0) THEN
         CALL SCREAT (SIZE, BUFFER, IRET)
         SCRWRK = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1010)
            IF (IRET.GT.1) WRITE (MSGTXT,1011)
            GO TO 990
            END IF
         END IF
C                                       Fill LUNS array in /CFILES/
      LUNS(1) = 16
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
 1000 FORMAT ('SETGRD: ERROR',I5,' READING CATBLK FIELD',I3)
 1010 FORMAT ('TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1011 FORMAT ('TROUBLE CREATING SCRATCH FILE')
      END
