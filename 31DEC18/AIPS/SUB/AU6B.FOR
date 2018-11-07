      SUBROUTINE AU6B (BRANCH)
C-----------------------------------------------------------------------
C! verb to display image value at pixel indicated by TV cursor (CURVAL)
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2014
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
C   AU6B performs interactive displays of map image values:
C   BRANCH = 1  CURVALUE  display X, Y, B; get B from map file
C   Another possibility, using the TV memory, appears here but is now
C   commented out.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, CDUM*1
      INTEGER   MLUN, IG1, POTERR, IERR, BUFFER(3072), ICHAN, I, ZAND
      REAL      RBUFF(3072), RDUM
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DTVC.INC'
      COMMON /AIPSCR/ BUFFER
      EQUIVALENCE (RBUFF, BUFFER)
      DATA PRGNAM /'AU6B '/
      DATA MLUN /17/
C-----------------------------------------------------------------------
      IF (BRANCH.NE.1) GO TO 999
C                                       Open TV
      CALL TVOPEN (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Branch to OP
 50   GO TO (100), BRANCH
C-----------------------------------------------------------------------
C                                       CURVALUE (from disk file)
C                                       display value under cursor
C-----------------------------------------------------------------------
C                                       no image yet
 100  CALL ADVERB ('GRCHAN', 'I', 1, 0, IG1, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IG1.LT.1) .OR. (IG1.GT.NGRAPH)) IG1 = MIN (2, NGRAPH)
      CALL ADVERB ('NCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
C                                       is this channel on?
      IF (ICHAN.GT.0) THEN
         I = 2 ** (ICHAN-1)
         IF ((ZAND(TVLIMG(1),I).EQ.0) .AND. (ZAND(TVLIMG(2),I).EQ.0)
     *      .AND. (ZAND(TVLIMG(3),I).EQ.0) .AND.
     *      (ZAND(TVLIMG(4),I).EQ.0)) ICHAN = 0
         END IF
      CALL TVALUE (MLUN, IG1, ICHAN, BUFFER, RBUFF, POTERR, IERR)
C
 975  CALL YHOLD ('OFFF', IERR)
      CALL TVCLOS (BUFFER, IERR)
C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
