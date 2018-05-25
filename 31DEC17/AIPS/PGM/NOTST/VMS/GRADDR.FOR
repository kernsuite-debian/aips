      PROGRAM GRADDR
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
C     Program to read a gripe file and produce a list of the
C     gripers to aid in readying gripes for distribution.
C     "Spawned" from PROGRAM GETTBC (DCW, 30Sep84)
C
C     - NDW, 9Mar86 (with assistance from EWG & GAF)
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     NGRIPE, LIN, LOUT, L5, L6, NIN, NOUT, K,
     *   NFIRST, NLAST, NSPLIT, ASPLIT, ITRIM
      CHARACTER   GRIPE*5000, NEWLIN*3, INFIL*30, OUTFIL*30,
     *   SPLIT*20, STAT*6, GRARGS(9)*5000, FNLOUT*1, FNLTBC*1,
     *   FNLIN*1, NAMES(300)*132, ADDRS(300)*264
      DATA        LIN, LOUT, L5, L6 /1, 2, 5, 6/
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
 30   NIN = 0
      NOUT = 0
C                                       big loop:
 50   CONTINUE
         CALL GETGRI (L6, LIN, NEWLIN, NGRIPE, GRIPE, GRARGS)
         IF (GRIPE.EQ.' ') GO TO 900
         NIN = NIN + 1
         IF (NIN.EQ.1) NFIRST = NGRIPE
C                                       print names & addresses:
 60         CONTINUE
            NSPLIT=ITRIM(GRARGS(3))
            ASPLIT=ITRIM(GRARGS(4))
            DO 65 K=1,NOUT
               IF((NAMES(K) .EQ. GRARGS(3)(1:NSPLIT))
     *           .AND. (ADDRS(K) .EQ. GRARGS(4)(1:ASPLIT)))
     *           GO TO 70
 65            CONTINUE
            WRITE (LOUT, '(A)') GRARGS(3)(1:NSPLIT)
            WRITE (LOUT, '(A)') GRARGS(4)(1:ASPLIT)
            WRITE (LOUT,1020)
            NOUT = NOUT + 1
            NAMES(NOUT)=GRARGS(3)(1:NSPLIT)
            ADDRS(NOUT)=GRARGS(4)(1:ASPLIT)
 70      CONTINUE
         NLAST = NGRIPE
         GO TO 50
C                                       that's all, folks:
 900  CONTINUE
      WRITE (L6, 1900) NIN, NFIRST, NLAST
      CLOSE (UNIT=LIN)
C                                       close LOUT:
 920  CLOSE (UNIT=LOUT)
C-----------------------------------------------------------------------
 1010 FORMAT (' GRADDR: enter name for file ', A6, ': ', $)
 1015 FORMAT (A30)
 1020 FORMAT (' ')
 1900 FORMAT (' GRADDR:', I5, ' gripes in (', I4, ' to', I5, ')')
      END
