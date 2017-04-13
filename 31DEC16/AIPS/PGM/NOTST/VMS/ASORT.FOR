      PROGRAM ASORT
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
C     Utility to sort and re-format \a, \c and \h# fields of ADR files.
C     Only does contiguous groups of lines of same type.
C     DCW, NRAO-CV, 24Apr88,27Apr88.
C-----------------------------------------------------------------------
C                                       Constants:
      INTEGER     ILDIM, NLSMAX, CMAX, L1, L2, L5, L6, FILDIM, OLMAX,
     *            BARLEN
      PARAMETER   (ILDIM=80, NLSMAX=100, CMAX=3, L1=1, L2=2, L5=5, L6=6,
     *            FILDIM=40, OLMAX=72, BARLEN=(OLMAX-3))
      CHARACTER   CS*3, DIGITS*10
      PARAMETER   (CS = 'ahc', DIGITS = '0123456789')
C                                       Local variables:
      INTEGER     I, J, ITRIM, NLS, NS, KI, KO
      CHARACTER   IL*(ILDIM), TIL*(ILDIM), C*3, LC*3,
     *            S((ILDIM/2))*(ILDIM), LS(NLSMAX)*(OLMAX), T*(ILDIM),
     *            INP*(FILDIM), F1*(FILDIM), F2*(FILDIM), LASTN*(ILDIM)
      LOGICAL     NOTBAR
C
C-----------------------------------------------------------------------
C                                       get input file:
  10  CONTINUE
      WRITE (L6, 1010)
      INP = ' '
      READ (L5, 1015, ERR=10) INP
      IF (INP.EQ.' ') INP = 'AIPSWHO:WHOGETS'
      F1 = INP(:ITRIM(INP)) // '.ADR'
      OPEN (UNIT=L1, FILE=F1, STATUS='OLD', ERR=17, READONLY)
         WRITE (L6, 1016) 1, F1
         GO TO 20
  17  CONTINUE
         WRITE (L6, 1017) F1
         GO TO 10
  20  CONTINUE
C                                       get output file:
  30  CONTINUE
      WRITE (L6, 1030)
      READ (L5, 1015, ERR=30) INP
      IF (INP.EQ.' ') THEN
         INP = F1
         I = INDEX (INP, ':')
         IF (I.GT.0) THEN
            T = INP(I+1:)
            INP = T
         ENDIF
         I = INDEX (INP, ']')
         IF (I.GT.0) THEN
            T = INP(I+1:)
            INP = T
         ENDIF
         I = INDEX (INP, '.ADR')
         T = INP(:I-1)
         INP = T
      ENDIF
      F2 = INP(:ITRIM(INP)) // '.ADR'
      OPEN (UNIT=L2, FILE=F2, STATUS='NEW', ERR=37,
     *      CARRIAGECONTROL='LIST')
         WRITE (L6, 1016) 2, F2
         GO TO 40
  37  CONTINUE
         WRITE (L6, 1017) F2
         GO TO 30
  40  CONTINUE
C
      LC = ' '
      NLS = 0
      KI = 0
      KO = 0
      LASTN = ' '
C                                       main scan loop:
 110  READ (L1, '(A)', END=910) IL
         KI = KI + 1
         IF (IL(OLMAX+1:).NE.' ')
     *      WRITE (L6, *) '[', LASTN(:ITRIM(LASTN)),
     *         '] Long-line: [', IL(:ITRIM(IL)), ']', ITRIM (IL)
         IF (IL(1:1).NE.'\') GO TO 310
C                                       parse line into strings:
         T = IL(2:)
         TIL = T
         NS = 0
 120     CONTINUE
            IF (TIL.EQ.' ') GO TO 150
 130        IF (TIL(1:1).NE.' ') GO TO 140
               T = TIL(2:)
               TIL = T
               GO TO 130
 140        I = INDEX (TIL, ' ')
            NS = NS + 1
            S(NS) = TIL(:I-1)
            IF (S(NS)(OLMAX+1:).NE.' ') WRITE (L6, *) '[',
     *         LASTN(:ITRIM(LASTN)), '] Long S(NS): [',
     *         S(NS)(:ITRIM(S(NS))), ']', ITRIM (S(NS))
            T = TIL(I+1:)
            TIL = T
            GO TO 120
 150     CONTINUE
C                                       is C in sort-set?
         C = S(1)
         IF (INDEX (CS, C(1:1)).EQ.0) GO TO 310
C
C                                       we will sort the line:
         IF (C(1:1).EQ.'h') THEN
            IF (C(2:2).EQ.' ') C(2:2) = '1'
            IF ((INDEX (DIGITS, C(2:2)).EQ.0).OR.(C(3:3).NE.' '))
     *         WRITE (L6,*) ' Encountered [\', C, '] !?!'
         ELSE
            IF (C(2:2).NE.' ')
     *            WRITE (L6,*) ' Encountered [\', C, '] !?!'
         ENDIF
C                                       move list down:
         NS = NS - 1
         IF (NS.GT.0) THEN
            DO 160 I = 1, NS
               S(I) = S(I+1)
 160           CONTINUE
         ENDIF
C                                       append S to LS:
C                                       comment-lines *not* appended:
         IF ((C.NE.LC).OR.(LC.EQ.'c'))
     *      CALL FLUSH (L2, L6, OLMAX, LASTN, LC, NLS, LS, KO)
         LC = C
         DO 170 I = 1, NS
            NLS = NLS + 1
            LS(NLS) = S(I)
 170        CONTINUE
C                                       \c case is special:
         IF (LC.EQ.'c') THEN
C                                       objective is to
C                                       normalize comment bars:
            IF ((NLS.EQ.1).AND.(LS(1)(1:1).EQ.'-')) THEN
               NOTBAR = .FALSE.
               DO 180 I = 1, OLMAX
                  NOTBAR = NOTBAR .OR.
     *               ((LS(1)(I:I).NE.'-') .AND. (LS(1)(I:).NE.' '))
 180              CONTINUE
               IF (.NOT.NOTBAR) THEN
C                                       make bar extend to margin:
                  LS(1)(BARLEN+1:) = ' '
                  J = INDEX (LS(1), ' ')
                  IF (J.LT.BARLEN) THEN
                     DO 190 I = J, BARLEN
                        LS(1)(I:I) = '-'
 190                    CONTINUE
                  ENDIF
               ELSE
                  WRITE (L6, *) 'Peculiar bar: [', LS(1)(:ITRIM(LS(1))),
     *               ']'
               ENDIF
            ENDIF
         ENDIF
C
         GO TO 110
C                                       IL not in CS:
 310     CALL FLUSH (L2, L6, OLMAX, LASTN, LC, NLS, LS, KO)
C                                       update LASTN (after FLUSH!):
         IF ((LASTN.NE.' ').AND.(IL.EQ.' ')) LASTN = ' '
         IF ((LASTN.EQ.' ').AND.(IL.NE.' ')) LASTN = IL
C                                       write IL:
         WRITE (L2, '(A)') IL(:ITRIM(IL))
         KO = KO + 1
         IF (MOD (KO, 2000).EQ.0) WRITE (L6, *) 'KO =', KO
         GO TO 110
C                                       That's all folks:
 910  CONTINUE
      CLOSE (UNIT=L1)
      WRITE (L6, *) 'final KI = ', KI
      CALL FLUSH (L2, L6, OLMAX, LASTN, LC, NLS, LS, KO)
      WRITE (L6, *) 'final KO = ', KO
      CLOSE (UNIT=L2)
C-----------------------------------------------------------------------
 1010 FORMAT (' Enter input WHOGETS-file  (w/o .ADR):', $)
 1015 FORMAT (A72)
 1016 FORMAT (' L',I1,'=', A)
 1017 FORMAT (' Cannot open file ', A)
 1030 FORMAT (' Enter output WHOGETS-file (w/o .ADR):', $)
      END
      SUBROUTINE FLUSH (L2, L6, OLMAX, LASTN, LC, NLS, LS, KO)
C-----------------------------------------------------------------------
C     Sort LS() and write it out.
C-----------------------------------------------------------------------
      INTEGER     L2, L6, OLMAX, NLS, KO
      CHARACTER   LASTN*(*), LC*(*), LS(*)*(*)
C
      INTEGER     I, J, ITRIM, OLDIM, NOL1
      PARAMETER   (OLDIM=84)
      CHARACTER   OL*(OLDIM), OL1*4, T*(OLDIM)
      LOGICAL     SWAP
C-----------------------------------------------------------------------
      IF (OLDIM.LT.OLMAX) STOP 'FLUSH: OLDIM.LT.OLMAX !'
      IF (LC.EQ.' ') GO TO 910
C                                       \c-lines not sorted:
      IF (LC.EQ.'c') GO TO 135
C                                       sort fields in LS():
      IF (NLS.GT.1) THEN
         J = 0
 110     SWAP = .FALSE.
            DO 130 I = 2, NLS
               IF (LS(I-1).GT.LS(I)) THEN
                  T = LS(I-1)
                  LS(I-1) = LS(I)
                  LS(I) = T
                  SWAP = .TRUE.
               END IF
 130           CONTINUE
         J = J + 1
         IF (J.GT.NLS) STOP 'FLUSH: J.GT.NLS !'
         IF (SWAP) GO TO 110
      END IF
C                                       check for equal fields:
      IF (NLS.GT.1) THEN
         DO 132 I = 2, NLS
            J = INDEX (LS(I), '=')
            IF (J.GT.0) THEN
               IF (LS(I-1)(:J).EQ.LS(I)(:J))
     *         WRITE (L6,*) '[', LASTN(:ITRIM(LASTN)), '] Duplicate: [',
     *            LS(I)(:ITRIM(LS(I))), ']'
            ENDIF
 132        CONTINUE
      ENDIF
C                                       write output lines:
 135  CONTINUE
      OL1 = '\' // LC
      NOL1 = ITRIM (OL1)
      OL = ' '
      IF (NLS.EQ.0) THEN
         WRITE (L2, '(A)') OL1(:NOL1)
         KO = KO + 1
C        WRITE (L6, *) OL1(:NOL1)
      ENDIF
      DO 140 I = 1, NLS
         J = ITRIM (OL)
         IF (J.GT.0) J = J + 1
         IF ((NOL1+J+(1+ITRIM(LS(I)))).GT.OLMAX) THEN
            WRITE (L2, '(A)') OL1(:NOL1) // OL(:ITRIM(OL))
            KO = KO + 1
            OL = ' '
         ENDIF
         OL(ITRIM (OL)+2:) = LS(I)
 140     CONTINUE
      IF (OL.NE.' ') THEN
         WRITE (L2, '(A)') OL1(:NOL1) // OL(:ITRIM(OL))
         KO = KO + 1
      ENDIF
C                                       clear LS():
      NLS = 0
      LC = ' '
C
 910  CONTINUE
      RETURN
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C   DCW, NRAO-CV, 30Nov82.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      ITRIM = LEN (STRING) + 1
 10   CONTINUE
         ITRIM = ITRIM - 1
         IF (ITRIM.LT.1) GO TO 999
         IF (STRING(ITRIM:ITRIM).EQ.' ') GO TO 10
 999  RETURN
      END
