      PROGRAM GETTBC
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
C   Program to read a gripe file and separate \tbc gripes into a
C   separate file.
C   DCW, 30Sept84
C-----------------------------------------------------------------------
      INTEGER     NGRIPE, LIN, LOUT, LTBC, L5, L6, NIN, NOUT, NTBC,
     *   NFIRST, NLAST, KSPLIT, ITRIM
      CHARACTER   GRIPE*5000, NEWLIN*3, INFIL*30, OUTFIL*30, TBCFIL*30,
     *   SPLIT*20, STAT*6, GRARGS(9)*5000, FNLOUT*1, FNLTBC*1, FNLIN*1
      DATA        LIN, LOUT, LTBC, L5, L6 /1, 2, 3, 5, 6/
      DATA        NEWLIN /'|||'/
C-----------------------------------------------------------------------
C                                       initialize: infile
 10   CONTINUE
         WRITE (L6,1010) ' INFIL'
         READ (L5,1015) INFIL
         IF (INFIL.EQ.' ') GO TO 15
            CALL PADTEX (L6, INFIL)
            OPEN (UNIT=LIN, FILE=INFIL, STATUS='OLD', ERR=10)
            GO TO 20
 15      CONTINUE
            OPEN (UNIT=LIN, FILE='GRINFIL', STATUS='OLD', ERR=10)
 20   CONTINUE
         WRITE (L6, 1010) 'OUTFIL'
         READ (L5, 1015) OUTFIL
         IF (OUTFIL.EQ.' ') GO TO 25
            CALL PADTEX (L6, OUTFIL)
            OPEN (UNIT=LOUT, FILE=OUTFIL, STATUS='NEW',
     *         CARRIAGECONTROL='LIST', ERR=20)
            GO TO 30
 25      CONTINUE
            OPEN (UNIT=LOUT, FILE='GROUTFIL', STATUS='NEW',
     *         CARRIAGECONTROL='LIST', ERR=20)
 30   CONTINUE
         WRITE (L6, 1010) 'TBCFIL'
         READ (L5, 1015) TBCFIL
         IF (TBCFIL.EQ.' ') GO TO 35
            CALL PADTEX (L6, TBCFIL)
            OPEN (UNIT=LTBC, FILE=TBCFIL, STATUS='NEW',
     *         CARRIAGECONTROL='LIST', ERR=30)
            GO TO 40
 35      CONTINUE
            OPEN (UNIT=LTBC, FILE='GRTBCFIL', STATUS='NEW',
     *         CARRIAGECONTROL='LIST', ERR=30)
 40   CONTINUE
         WRITE (L6,1040) 'OUTFIL'
         FNLOUT = 'F'
         READ (L5,1015,ERR=40) FNLIN
         IF (FNLIN.EQ.'T') FNLOUT = 'T'
 42   CONTINUE
         WRITE (L6,1040) 'TBCFIL'
         FNLTBC = 'F'
         READ (L5,1015,ERR=42) FNLIN
         IF (FNLIN.EQ.'T') FNLTBC = 'T'
 45   CONTINUE
         WRITE (L6,1045)
         READ (L5,1015,ERR=45) SPLIT
         IF (SPLIT.EQ.' ') GO TO 45
         KSPLIT = ITRIM (SPLIT)
      NIN = 0
      NOUT = 0
      NTBC = 0
C                                       big loop:
 50   CONTINUE
         CALL GETGRI (L6, LIN, NEWLIN, NGRIPE, GRIPE, GRARGS)
         IF (GRIPE.EQ.' ') GO TO 900
         NIN = NIN + 1
         IF (NIN.EQ.1) NFIRST = NGRIPE
C                                       test for "splitting" string:
         IF (INDEX (GRIPE, SPLIT(1:KSPLIT)).NE.0) GO TO 60
            IF (NOUT.EQ.0) WRITE (LOUT,1050) FNLOUT
            CALL WRIGRI (L6, LOUT, NEWLIN, GRIPE)
            NOUT = NOUT + 1
            GO TO 70
 60      CONTINUE
            IF (NTBC.EQ.0) WRITE (LTBC,1050) FNLTBC
            CALL WRIGRI (L6, LTBC, NEWLIN, GRIPE)
            NTBC = NTBC + 1
 70      CONTINUE
         NLAST = NGRIPE
         GO TO 50
C                                       that's all, folks:
 900  CONTINUE
      WRITE (L6, 1900) NIN, NFIRST, NLAST, NOUT, NTBC, SPLIT(1:KSPLIT)
      CLOSE (UNIT=LIN)
C                                       close LOUT:
      IF (NOUT.GT.0) GO TO 910
         STAT = 'DELETE'
         GO TO 920
 910  CONTINUE
         WRITE (LOUT, 1920)
         STAT = 'KEEP'
 920  CLOSE (UNIT=LOUT, STATUS=STAT)
C                                       close LTBC:
      IF (NTBC.GT.0) GO TO 930
         STAT = 'DELETE'
         GO TO 940
 930  CONTINUE
         WRITE (LTBC, 1920)
         STAT = 'KEEP'
 940  CONTINUE
      CLOSE (UNIT=LTBC, STATUS=STAT)
C-----------------------------------------------------------------------
 1010 FORMAT (' GETTBC: enter name for file ', A6, ': ', $)
 1015 FORMAT (A30)
 1040 FORMAT (' GETTBC: enter \finalrun for file ', A6, ': ', $)
 1045 FORMAT (' GETTBC: enter the "splitting" string (e.g., \tbc): ', $)
 1050 FORMAT ('\input aipsgrip:gripemac5.tex', /,
     *        '\def\finalrun{',A,'}', /)
 1900 FORMAT (' GETTBC:', I5, ' gripes in (', I4, ' to', I5,
     *                                       '), separated into', /,
     *        '        ', I5, ' regular and', I5, ' containing "',
     *                                       A, '".')
 1920 FORMAT ('\end')
      END
