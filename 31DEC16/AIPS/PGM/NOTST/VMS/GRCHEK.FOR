      PROGRAM GRCHEK
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
C     Program to read a gripe file and check its format, sequencing,
C     etc.
C     DCW, 28Sept84
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     NGRIPE, LGRIPE, L5, L6, LIN, NFIRST, NCOUNT, K
      CHARACTER   GRIPE*5000, NEWLIN*3, INFILE*30, GRARGS(9)*5000
      DATA        NEWLIN /'|||'/
C-----------------------------------------------------------------------
C                                       initialize:
      LGRIPE = -1
      NCOUNT = 0
      L5 = 5
      L6 = 6
 5    CONTINUE
         LIN = 1
         WRITE (L6, 1005) 'INFILE'
         READ (L5, 1007, ERR=5) INFILE
         IF (INFILE.EQ.' ') GO TO 7
            CALL PADTEX (L6, INFILE)
            OPEN (UNIT=LIN, FILE=INFILE, STATUS='OLD', ERR=5)
            GO TO 10
 7       CONTINUE
            OPEN (UNIT=LIN, FILE='GRINFIL', STATUS='OLD', ERR=5)
C                                       big loop:
 10   CONTINUE
         CALL GETGRI (L6, LIN, NEWLIN, NGRIPE, GRIPE, GRARGS)
         IF (GRIPE.EQ.' ') GO TO 900
         NCOUNT = NCOUNT + 1
         IF (LGRIPE.NE.-1) GO TO 20
            NFIRST = NGRIPE
            GO TO 30
 20      CONTINUE
            K = (NGRIPE - (LGRIPE + 1))
            IF (K.EQ.1) WRITE (L6, 1020) (LGRIPE + 1)
            IF (K.GT.1) WRITE (L6, 1025) (LGRIPE + 1), (NGRIPE - 1)
            IF (K.LT.0) WRITE (L6, 1027) NGRIPE, LGRIPE
 30      CONTINUE
         LGRIPE = NGRIPE
         GO TO 10
 900  CONTINUE
      CLOSE (UNIT=LIN)
      WRITE (L6, 1900) NCOUNT, NFIRST, LGRIPE
C-----------------------------------------------------------------------
 1005 FORMAT (' GRCHEK: enter name for file ', A6, ': ', $)
 1007 FORMAT (A30)
 1020 FORMAT (' GRCHEK: gripe ', 5X, '   ', I5, '  is missing.')
 1025 FORMAT (' GRCHEK: gripes', I5, ' to', I5, ' are missing.')
 1027 FORMAT (' GRCHEK: out of order! gripe', I5, ' follows gripe', I5)
 1900 FORMAT (' GRCHEK:', I5, ' gripes. first=', I5, ', last=', I5)
      END
