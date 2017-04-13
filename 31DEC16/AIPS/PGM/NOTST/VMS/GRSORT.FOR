      PROGRAM GRSORT
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
C     Program to sort a gripe file. GETGRI checks format, sequencing,
C     etc. GRSORT suppresses identical redundant copies of gripes.
C     Optionally, it also suppresses non-identical copies.
C     DCW, 01Oct84
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     MGRIPE, LGRIPE
      PARAMETER   (MGRIPE=500, LGRIPE=5000)
      INTEGER     NGRIPE, L5, L6, LIN, LOUT, K, J, IGRIPE,
     *            JGRIPS(MGRIPE), NOUT, ILAST
      INTEGER     N10
      LOGICAL     LCRUSH
      CHARACTER   GRIPES(MGRIPE)*(LGRIPE), GRIPE*(LGRIPE), NEWLIN*3,
     *            INFILE*30, OUTFIL*30, GRARGS(9)*(LGRIPE)
      DATA        LIN, LOUT, L5, L6, N10 /1, 2, 5, 6, 10/
      DATA        NEWLIN /'|||'/
C-----------------------------------------------------------------------
C                                       initialize:
      NGRIPE = 0
 10   CONTINUE
         WRITE (L6, 1010) 'INFILE'
         READ (L5, 1012, ERR=10) INFILE
         IF (INFILE.EQ.' ') GO TO 15
            CALL PADTEX (L6, INFILE)
            OPEN (UNIT=LIN, FILE=INFILE, STATUS='OLD', ERR=10)
            GO TO 20
 15      CONTINUE
            OPEN (UNIT=LIN, FILE='GRINFIL', STATUS='OLD', ERR=10)
 20   CONTINUE
         WRITE (L6, 1010 ) 'OUTFIL'
         READ (L5, 1012, ERR=20) OUTFIL
         IF (OUTFIL.EQ.' ') GO TO 25
            CALL PADTEX (L6, OUTFIL)
            OPEN (UNIT=LOUT, FILE=OUTFIL, STATUS='NEW',
     *            CARRIAGECONTROL='LIST', ERR=20)
            GO TO 30
 25      CONTINUE
            OPEN (UNIT=LOUT, FILE='GROUTFIL', STATUS='NEW',
     *            CARRIAGECONTROL='LIST', ERR=20)
 30   CONTINUE
         WRITE (L6, 1030)
         READ (L5, 1035, ERR=30) LCRUSH
C                                       read and sort:
 50   CONTINUE
         CALL GETGRI (L6, LIN, NEWLIN, IGRIPE, GRIPE, GRARGS)
         IF (GRIPE.EQ.' ') GO TO 900
         IF (NGRIPE.GT.0) GO TO 60
C                                       insert the first gripe:
            J = 1
            GO TO 90
 60      CONTINUE
         IF (IGRIPE.LT.JGRIPS(NGRIPE)) GO TO 70
C                                       new gripe goes at end:
            J = NGRIPE + 1
            GO TO 90
 70      CONTINUE
C                                       else, find interpolation spot:
         J = NGRIPE + 1
 80      CONTINUE
            J = J - 1
            IF (J.LE.0) STOP 'GRSORT: J.LE.0 !!'
            GRIPES(J+1) = GRIPES(J)
            JGRIPS(J+1) = JGRIPS(J)
            IF (J.LE.1) GO TO 90
            IF (IGRIPE.LT.JGRIPS(J-1)) GO TO 80
C                                       store the gripe:
 90      CONTINUE
         GRIPES(J) = GRIPE
         JGRIPS(J) = IGRIPE
         NGRIPE = NGRIPE + 1
         IF (NGRIPE.GE.MGRIPE) STOP 'GRSORT: NGRIPE.GE.MGRIPE!'
         IF (MOD (NGRIPE, N10).EQ.1) WRITE (L6, 1090) NGRIPE, IGRIPE, J
         GO TO 50
C                                       write out the sorted gripes:
 900  CONTINUE
      IF (MOD (NGRIPE, N10).NE.1) WRITE (L6, 1090) NGRIPE, IGRIPE, J
      CLOSE (UNIT=LIN)
      WRITE (L6, 1900) NGRIPE
      NOUT = 0
      WRITE (LOUT, 1903)
      DO 930 J = 1, NGRIPE
         IF (J.EQ.1) GO TO 920
         IF (JGRIPS(J).NE.JGRIPS(J-1)) GO TO 920
            IF (.NOT.LCRUSH.AND.(GRIPES(J).NE.GRIPES(J-1))) GO TO 910
               WRITE (L6, 1907) JGRIPS(J)
               GO TO 930
 910        CONTINUE
               WRITE (L6, 1910) JGRIPS(J)
 920     CONTINUE
         CALL WRIGRI (L6, LOUT, NEWLIN, GRIPES(J))
         NOUT = NOUT + 1
         ILAST = JGRIPS(J)
 930     CONTINUE
      WRITE (LOUT, 1930)
      CLOSE (UNIT=LOUT)
      WRITE (L6, 1935) NOUT, JGRIPS(1), ILAST
C-----------------------------------------------------------------------
 1010 FORMAT (' GRSORT: enter name for file ', A6, ': ', $)
 1012 FORMAT (A30)
 1030 FORMAT (' GRSORT: Be careful! This option can destroy data:', /,
     *        '         suppress ALL redundant gripes? enter T or F:',
     *                                                               $)
 1035 FORMAT (L1)
 1090 FORMAT (' GRSORT: NGRIPE=', I5, ', IGRIPE=', I5, ', J=', I5)
 1900 FORMAT (' GRSORT:', I5, ' gripes read.')
 1903 FORMAT ('\input aipsgrip:gripemac5.tex', /,
     *        '\def\finalrun{F}', /)
 1907 FORMAT (' GRSORT: duplicate copy of gripe', I5, ' suppressed.')
 1910 FORMAT (' GRSORT: another version of gripe', I5, ' copied.')
 1930 FORMAT ('\end')
 1935 FORMAT (' GRSORT:', I5, ' gripes written. first=', I5,
     *                                                   ', last=', I5)
      END
