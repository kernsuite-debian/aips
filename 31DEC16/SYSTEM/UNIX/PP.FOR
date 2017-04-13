      PROGRAM PP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2004, 2012
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
C   Program to preprocess AIPS standard Fortran files into files for the
C   local Fortran compiler.  It assumes that the files are assigned to
C   the logical names PPINFIL and PPOUTFIL.  At present, PP does:
C         (1) Locate INCLUDE statements and insert the text
C         (2) Substitue REAL for HOLLERITH declarations
C         (3) Carry and insert LOCAL INCLUDE sections from the
C             beginning of the file to the places specified
C         (4) Allow a 2nd level of include
C         (5) Ignore includes if already included once in this routine
C   PP is deliberately "dumb", requiring the INCLUDE, SUBROUTINE, and
C   various FUNCTION statements to begin in column 7.
C   Version for UNIX
C-----------------------------------------------------------------------
      INTEGER   MXNLIN, MXINCN, MXINCL
      PARAMETER (MXNLIN = 3000)
      PARAMETER (MXINCN = 30)
      PARAMETER (MXINCL = 300)
C
      CHARACTER INCBUF(MXNLIN)*72, INCNAM(MXINCN)*20, ALRINC(MXINCL)*20,
     *   STDINC(10)*20, STDUPL(5)*20, LINT*10
      INTEGER   ILN(MXNLIN), ILB(MXINCN), ILE(MXINCN), ILT(MXINCN),
     *   NALR, NLI, NSTD, NSTDUP, NLIN, ADDRSZ
      COMMON /PPCHAR/ INCBUF, INCNAM, ALRINC, STDINC, STDUPL, LINT
      COMMON /PPARMS/ ILN, ILB, ILE, ILT, NALR, NLI, NSTD, NSTDUP, NLIN
C
      CHARACTER INLINE*72, OUTLIN*72, INFIL*20, TFIL*20, MSG*80,
     *   LOGNAM*8, ARCH*8
      INTEGER   I1, I2, IL, INDEX, ITRIM, IRET, I, IERR, J1, J2, ILEN,
     *   LLEN, XLEN, XLNB
      LOGICAL   KEEP, ALLOW
C-----------------------------------------------------------------------
C                                       special data initialization
C                                       allow these multiple times
      NSTDUP = 2
      STDUPL(1) = 'ZVD.INC'
      STDUPL(2) = 'ZVND.INC'
C                                       suck these to memory asap
      NSTD = 5
      STDINC(1) = 'DMSG.INC'
      STDINC(2) = 'DDCH.INC'
      STDINC(3) = 'DFIL.INC'
      STDINC(4) = 'DUVH.INC'
      STDINC(5) = 'DHDR.INC'
      IRET = 8
      DO 5 I = 1,MXINCN
         ILB(I) = 0
         ILE(I) = 0
         ILT(I) = 0
 5       CONTINUE
      NLI = 0
      NLIN = 0
      NALR = 0
      WRITE (6,1010) 'TEST VERSION OF PP'
C                                       how big is an address variable
      CALL ZADRSZ (ADDRSZ)
C                                       get $ARCH
      LOGNAM = 'ARCH:'
      LLEN = LEN (LOGNAM)
      XLEN = LEN (ARCH)
      CALL ZTRLOG (LLEN, LOGNAM, XLEN, ARCH, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         ARCH = ' '
         END IF
      IF ((ARCH(:2).EQ.'HP') .OR. (ADDRSZ.LE.4)) THEN
         LINT = 'INTEGER'
      ELSE
         LINT = 'INTEGER*8'
         END IF
C                                       Open the input file
      CALL PPOPEN (15, 'PPINFIL:', 'READ', IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Open the output file
      CALL PPOPEN (16, 'PPOUTFIL:', 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 910
C                                       main file read loop
 10   CONTINUE
         READ (15,1010,END=800,ERR=920) INLINE
         ILEN = ITRIM (INLINE)
         IF (ILEN.EQ.0) GO TO 10
C                                       Substitute for HOLLERITH
C                                       Allow for lowercase too (ATNF)
         IF (INLINE(7:16).EQ.'hollerith ') INLINE(7:16) = 'REAL     '
         IF (INLINE(7:16).EQ.'HOLLERITH ') INLINE(7:16) = 'REAL     '
         IF (INLINE(7:16).EQ.'longint   ') INLINE(7:16) = LINT
         IF (INLINE(7:16).EQ.'LONGINT   ') INLINE(7:16) = LINT
C                                       Write all but INCLUDEs
C                                       ATNF only: uppercase includes.
C        IF (INLINE(7:13).EQ.'include') CALL UPCASE (INLINE)
C                                       Check for start of subroutine
         IF (INLINE(:6).EQ.' ') THEN
            IF (INLINE(7:16).EQ.'BLOCK DATA') NALR = 0
            IF (INLINE(7:16).EQ.'SUBROUTINE') NALR = 0
            IF (INLINE(7:14).EQ.'FUNCTION') NALR = 0
            IF (INLINE(7:19).EQ.'REAL FUNCTION') NALR = 0
            IF (INLINE(7:22).EQ.'INTEGER FUNCTION') NALR = 0
            IF (INLINE(7:22).EQ.'LOGICAL FUNCTION') NALR = 0
            IF (INLINE(7:31).EQ.'DOUBLE PRECISION FUNCTION') NALR = 0
            END IF
C                                       Simple write out of line
         IF ((INLINE(7:13).NE.'INCLUDE') .OR. (INLINE(:1).EQ.'C')
     *      .OR. (INLINE(7:17).EQ.'INCLUDE ''($')) THEN
            WRITE (16,1010,ERR=930) INLINE(:ILEN)
            GO TO 10
            END IF
C                                       Include: dig out name
 15      OUTLIN = INLINE(14:72)
         I1 = INDEX (OUTLIN, '''')
         I1 = I1 + 1
         I2 = INDEX (OUTLIN(I1:72), '''')
C                                       problem with INCL statement
         IF (I2.LE.0) THEN
            MSG = 'WARNING - PROBLEM PARSING INCLUDE CARD:'
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            MSG = INLINE(:ILEN)
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            WRITE (16,1010,ERR=930) INLINE(:ILEN)
            GO TO 10
            END IF
         I2 = I2 + I1 - 2
         INFIL = OUTLIN(I1:I2)
C                                       LOCAL INCL declaration?
 20      IF (INLINE(1:6).EQ.'LOCAL ') THEN
C                                       No logical names allowed
            IF (INDEX(INFIL,':').GT.0) THEN
               MSG = 'LOGICAL NOT ALLOWED IN LOCAL INCLUDE NAME:'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               GO TO 965
               END IF
C                                       No repeat declarations
            DO 25 I = 1,NLI
               IF (INFIL.EQ.INCNAM(I)) THEN
                  MSG = 'LOCAL INCLUDE NAME ALREADY DEFINED:'
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  MSG = INFIL
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  GO TO 965
                  END IF
 25            CONTINUE
            IF (NLI.EQ.MXINCN) THEN
               WRITE (6,1025) MXINCN
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               GO TO 965
               END IF
            NLI = NLI + 1
            ILB(NLI) = NLIN + 1
            ILT(NLI) = 1
            INCNAM(NLI) = INFIL
C                                       read Local Inc loop
 30         CONTINUE
               READ (15,1010,END=800,ERR=920) INLINE
               ILEN = ITRIM (INLINE)
               IF ((ILEN.GT.0) .AND. (INLINE(:2).NE.'C;')) THEN
C                                       end of local include?
                  IF (INLINE(1:6).EQ.'LOCAL ' .OR.
     *               INLINE(1:6).EQ.'local ') THEN
                     ILE(NLI) = NLIN
                     IF ((INLINE(7:13).EQ.'INCLUDE') .AND.
     *                  (INLINE(1:1).NE.'C') .AND.
     *                  (INLINE(7:17).NE.'INCLUDE ''($')) GO TO 15
                     GO TO 10
                     END IF
                  IF (NLIN.EQ.MXNLIN) THEN
                     WRITE (6,1030) MXNLIN
                     MSG = INFIL
                     WRITE (6,1000) MSG(1:ITRIM(MSG))
                     GO TO 965
                     END IF
                  NLIN = NLIN + 1
                  IF ((INLINE(7:16).EQ.'hollerith ') .OR.
     *               (INLINE(7:16).EQ.'HOLLERITH '))
     *               INLINE(7:16) = 'REAL '
                  IF ((INLINE(7:16).EQ.'longint   ') .OR.
     *               (INLINE(7:16).EQ.'LONGINT   '))
     *               INLINE(7:16) = LINT
                  INCBUF(NLIN) = INLINE
                  ILN(NLIN) = ILEN
                  END IF
               GO TO 30
C                                       Include something
         ELSE
            I1 = INDEX (INFIL, ':')
            I1 = I1 + 1
            TFIL = INFIL(I1:20)
C                                       Already included?
            IF (NALR.GT.0) THEN
               DO 35 I = 1,NALR
                  IF (TFIL.EQ.ALRINC(I)) THEN
                     INLINE(:1) = 'C'
                     INLINE(7:13) = 'SKIPPED'
                     WRITE (16,1010,ERR=930) INLINE(1:ILEN)
                     GO TO 10
                     END IF
 35               CONTINUE
               END IF
C                                       Duplication allowed?
            ALLOW = .FALSE.
            DO 36 I = 1,NSTDUP
               IF (TFIL.EQ.STDUPL(I)) ALLOW = .TRUE.
 36            CONTINUE
            IF (.NOT.ALLOW) THEN
               IF (NALR.GE.MXINCL) THEN
                  WRITE (6,1035) MXINCL
                  MSG = INFIL
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  GO TO 965
                  END IF
               NALR = NALR + 1
               ALRINC(NALR) = TFIL
               END IF
C                                       is it in local list?
            I2 = 0
            DO 40 I = 1,NLI
               IF (TFIL.EQ.INCNAM(I)) I2 = I
 40            CONTINUE
C                                       Yes - dump local from memory
            IF (I2.GT.0) THEN
C                                       warn if logical name
               IF ((I1.NE.1) .AND. (ILT(I2).NE.2)) THEN
                  MSG = 'WARNING - SUBSTITUTED LOCAL INCLUDE FOR:'
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  MSG = INFIL
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  END IF
C                                       write INCL line as Comment
               INLINE(1:1) = 'C'
               WRITE (16,1010,ERR=930) INLINE(1:ILEN)
               J1 = ILB(I2)
               J2 = ILE(I2)
               DO 50 I = J1,J2
                  IL = ILN(I)
                  CALL OUT2 (IL, INCBUF(I), IERR)
                  IF (IERR.NE.0) GO TO 930
 50               CONTINUE
C                                       No - get external file
            ELSE
               IF (INFIL(1:5).NE.'INCS:') THEN
                  MSG = 'WARNING - NON-STANDARD INCLUDE USED:'
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  MSG = INFIL
                  WRITE (6,1000) MSG(1:ITRIM(MSG))
                  END IF
C                                       write INCL line as Comment
               INLINE(1:1) = 'C'
               WRITE (16,1010,ERR=930) INLINE(1:ILEN)
C                                       open INCL file
               CALL PPOPEN (17, INFIL, 'READ', IERR)
               IF (IERR.NE.0) GO TO 940
               KEEP = .FALSE.
               DO 55 I = 1,NSTD
                  IF (STDINC(I).EQ.TFIL) KEEP = .TRUE.
 55               CONTINUE
               IF (NLI.GE.MXINCN-1) KEEP = .FALSE.
               IF (NLIN.GE.MXNLIN-150) KEEP = .FALSE.
               IF (KEEP) THEN
                  NLI = NLI + 1
                  ILB(NLI) = NLIN + 1
                  ILT(NLI) = 2
                  INCNAM(NLI) = TFIL
                  END IF
C                                       read INCL file loop
 60            CONTINUE
                  READ (17,1010,END=65,ERR=950) OUTLIN
                  ILEN = ITRIM (OUTLIN)
                  IF ((ILEN.GT.0) .AND. (OUTLIN(:2).NE.'C;')) THEN
                     IF ((OUTLIN(7:16).EQ.'hollerith ') .OR.
     *                  (OUTLIN(7:16).EQ.'HOLLERITH '))
     *                   OUTLIN(7:16) = 'REAL'
                     IF ((OUTLIN(7:16).EQ.'longint   ') .OR.
     *                  (OUTLIN(7:16).EQ.'LONGINT   '))
     *                   OUTLIN(7:16) = LINT
                     IF (KEEP) THEN
                        NLIN = NLIN + 1
                        INCBUF(NLIN) = OUTLIN
                        ILN(NLIN) = ILEN
                        END IF
                     CALL OUT2 (ILEN, OUTLIN, IERR)
                     IF (IERR.NE.0) GO TO 955
                     END IF
                  GO TO 60
 65            CLOSE (UNIT=17)
               IF (KEEP) ILE(NLI) = NLIN
               END IF
C                                       return to read loop
            GO TO 10
            END IF
C                                       write the line
C                                       normal EOF
 800  IRET = 1
      GO TO 965
C                                       Errors:
C                                       open input
 900  MSG = 'CAN''T OPEN THE INPUT FILE'
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 999
C                                       open output
 910  MSG = 'CAN''T OPEN THE OUTPUT FILE'
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 970
C                                       read input
 920  MSG = 'CAN''T READ THE INPUT FILE'
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 965
C                                       write output
 930  MSG = 'CAN''T WRITE THE OUTPUT FILE'
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 965
C                                       open incl
 940  MSG = 'CAN''T OPEN INCL FILE ' // INFIL
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 965
C                                       Read incl
 950  MSG = 'CAN''T READ INCL FILE ' // INFIL
      WRITE (6,1000) MSG(1:ITRIM(MSG))
      GO TO 960
C                                       write output in incl section
 955  MSG = 'CAN''T WRITE INCLUDE TO THE OUTPUT FILE'
      WRITE (6,1000) MSG(1:ITRIM(MSG))
C                                       closes
 960  CLOSE (UNIT=17,STATUS='KEEP')
 965  CLOSE (UNIT=16,STATUS='KEEP')
 970  CLOSE (UNIT=15,STATUS='KEEP')
      GO TO 999
C
 999  IF (IRET.GT.1) CALL ZEXIT (IRET)
      STOP
C-----------------------------------------------------------------------
 1000 FORMAT (' PP(    ): ',A)
 1010 FORMAT (A)
 1025 FORMAT (' PP LOCAL INCLUDE NAME LIST TOO LONG (>',I2,') AT:')
 1030 FORMAT (' PP LOCAL INCLUDE LIST TOO LONG (>',I5,
     *   ' LINES) AT NAME:')
 1035 FORMAT (' PP TOO MANY (>',I4,') INCLUDES AT NAME:')
      END
      SUBROUTINE OUT2 (ILEN, OUTLIN, IERR)
C-----------------------------------------------------------------------
C   writes out the specified line except if it's an INCLUDE.  In that
C   case it does the full (2nd-level) include.  This is only called when
C   we are already writing out an INCLUDE at first level.
C-----------------------------------------------------------------------
      INTEGER   ILEN, IERR
      CHARACTER OUTLIN*(*)
C
      INTEGER   MXNLIN, MXINCN, MXINCL
      PARAMETER (MXNLIN = 3000)
      PARAMETER (MXINCN = 30)
      PARAMETER (MXINCL = 300)
C
      CHARACTER INCBUF(MXNLIN)*72, INCNAM(MXINCN)*20, ALRINC(MXINCL)*20,
     *   STDINC(10)*20, STDUPL(5)*20, LINT*10
      INTEGER   ILN(MXNLIN), ILB(MXINCN), ILE(MXINCN), ILT(MXINCN),
     *   NALR, NLI, NSTD, NSTDUP, NLIN
      COMMON /PPCHAR/ INCBUF, INCNAM, ALRINC, STDINC, STDUPL, LINT
      COMMON /PPARMS/ ILN, ILB, ILE, ILT, NALR, NLI, NSTD, NSTDUP, NLIN
C
      CHARACTER INLINE*72, INFIL*20, TFIL*20, TMPLIN*72, MSG*72
      INTEGER   I1, I2, IL, INDEX, ITRIM, I, J1, J2
      LOGICAL   KEEP, ALLOW
C-----------------------------------------------------------------------
      IERR = 0
C                                       simple output
      IF ((OUTLIN(7:13).NE.'INCLUDE') .OR. (OUTLIN(:1).EQ.'C')
     *      .OR. (INLINE(7:17).EQ.'INCLUDE ''($')) THEN
         IF (ILEN.GT.0) WRITE (16,1010,ERR=930) OUTLIN(:ILEN)
C                                       a 2nd-level include
      ELSE
         INLINE = OUTLIN
C                                       Not local INCLUDE declaration
         IF (INLINE(:5).EQ.'LOCAL') THEN
            MSG = 'LOCAL INCLUDE DECLARATION NOT ALLOWED IN AN INCLUDE'
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            GO TO 990
            END IF
C                                       get include name
         TMPLIN = INLINE(14:72)
         I1 = INDEX (TMPLIN, '''')
         I1 = I1 + 1
         I2 = INDEX (TMPLIN(I1:72), '''')
         IF (I2.LE.0) THEN
            MSG = 'PROBLEM PARSING INCLUDE CARD:'
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            MSG = INLINE(1:ILEN)
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            GO TO 990
            END IF
         I2 = I2 + I1 - 2
         INFIL = TMPLIN(I1:I2)
         I1 = INDEX (INFIL, ':')
         I1 = I1 + 1
         TFIL = INFIL(I1:20)
C                                       Already included?
         IF (NALR.GT.0) THEN
            DO 10 I = 1,NALR
               IF (TFIL.EQ.ALRINC(I)) THEN
                  INLINE(:1) = 'C'
                  INLINE(7:13) = 'SKIPPED'
                  WRITE (16,1010,ERR=930) INLINE(1:ILEN)
                  GO TO 999
                  END IF
 10            CONTINUE
            END IF
C                                       Duplication allowed?
         ALLOW = .FALSE.
         DO 15 I = 1,NSTDUP
            IF (TFIL.EQ.STDUPL(I)) ALLOW = .TRUE.
 15         CONTINUE
         IF (.NOT.ALLOW) THEN
            IF (NALR.GE.MXINCL) THEN
               MSG = 'MAX NUMBER OF INCLUDES EXCEEDED AT'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INLINE(1:ILEN)
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               IERR = 2
               GO TO 999
               END IF
            NALR = NALR + 1
            ALRINC(NALR) = TFIL
            END IF
C                                       is it in local list?
         I2 = 0
         DO 20 I = 1,NLI
            IF (TFIL.EQ.INCNAM(I)) I2 = I
 20         CONTINUE
C                                       Yes - dump local from memory
         IF (I2.GT.0) THEN
C                                       warn if logical name
            IF ((I1.NE.1) .AND. (ILT(I2).NE.2)) THEN
               MSG = 'SUBSTITUTED LOCAL INCLUDE FOR:'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               END IF
C                                       write INCL line as Comment
            INLINE(1:1) = 'C'
            WRITE (16,1010,ERR=930) INLINE(1:ILEN)
            J1 = ILB(I2)
            J2 = ILE(I2)
            DO 30 I = J1,J2
               IL = ILN(I)
               CALL OUT3 (IL, INCBUF(I), IERR)
               IF (IERR.NE.0) GO TO 930
 30            CONTINUE
C                                       No - get external file
         ELSE
            IF (INFIL(1:5).NE.'INCS:') THEN
               MSG = 'WARNING - NON-STANDARD INCLUDE USED:'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               END IF
C                                       write INCL line as Comment
            INLINE(1:1) = 'C'
            WRITE (16,1010,ERR=930) INLINE(1:ILEN)
C                                       open INCL file
            CALL PPOPEN (18, INFIL, 'READ', IERR)
            IF (IERR.NE.0) THEN
               MSG = 'CAN''T OPEN INCLUDE FILE ' // INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               GO TO 999
               END IF
            KEEP = .FALSE.
            DO 35 I = 1,NSTD
               IF (STDINC(I).EQ.TFIL) KEEP = .TRUE.
 35            CONTINUE
            IF (NLI.GE.MXINCN-1) KEEP = .FALSE.
            IF (NLIN.GE.MXNLIN-150) KEEP = .FALSE.
            IF (KEEP) THEN
               NLI = NLI + 1
               ILB(NLI) = NLIN + 1
               ILT(NLI) = 2
               INCNAM(NLI) = TFIL
               END IF
C                                       read INCL file loop
 40         CONTINUE
               READ (18,1010,END=50,ERR=950) INLINE
               ILEN = ITRIM (INLINE)
               IF ((ILEN.GT.0) .AND. (INLINE(:2).NE.'C;')) THEN
                  IF ((INLINE(7:16).EQ.'hollerith ') .OR.
     *               (INLINE(7:16).EQ.'HOLLERITH '))
     *               INLINE(7:16) = 'REAL'
                  IF ((INLINE(7:16).EQ.'longint   ') .OR.
     *               (INLINE(7:16).EQ.'LONGINT   '))
     *               INLINE(7:16) = LINT
                  IF (KEEP) THEN
                     NLIN = NLIN + 1
                     INCBUF(NLIN) = INLINE
                     ILN(NLIN) = ILEN
                     END IF
                  CALL OUT3 (ILEN, INLINE, IERR)
                  IF (IERR.NE.0) GO TO 955
                  END IF
               GO TO 40
 50         CLOSE (UNIT=18)
            IF (KEEP) ILE(NLI) = NLIN
            END IF
         END IF
      GO TO 999
C                                       errors
 930  IERR = 1
      MSG = 'ERROR WRITING INCLUDED LINE TO OUTPUT'
      GO TO 995
 950  IERR = 3
      MSG = 'ERROR READING 2ND-LEVEL INCLUDE FILE'
      CLOSE (UNIT=18)
      GO TO 995
 955  MSG = 'ERROR WRITING 2ND-LEVEL INCLUDE LINE TO OUTPUT'
      CLOSE (UNIT=18)
      GO TO 995
C
 990  MSG = 'CONTINUING ANYWAY'
 995  WRITE (6,1000) MSG(1:ITRIM(MSG))
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' PP(OUT2): ',A)
 1010 FORMAT (A)
      END
      SUBROUTINE OUT3 (ILEN, OUTLIN, IERR)
C-----------------------------------------------------------------------
C   writes out the specified line except if it's an INCLUDE.  In that
C   case it does the full (3rd-level) include.  This is only called when
C   we are already writing out an INCLUDE at second level.
C-----------------------------------------------------------------------
      INTEGER   ILEN, IERR
      CHARACTER OUTLIN*(*)
C
      INTEGER   MXNLIN, MXINCN, MXINCL
      PARAMETER (MXNLIN = 3000)
      PARAMETER (MXINCN = 30)
      PARAMETER (MXINCL = 300)
C
      CHARACTER INCBUF(MXNLIN)*72, INCNAM(MXINCN)*20, ALRINC(MXINCL)*20,
     *   STDINC(10)*20, STDUPL(5)*20, LINT*10
      INTEGER   ILN(MXNLIN), ILB(MXINCN), ILE(MXINCN), ILT(MXINCN),
     *   NALR, NLI, NSTD, NSTDUP, NLIN
      COMMON /PPCHAR/ INCBUF, INCNAM, ALRINC, STDINC, STDUPL, LINT
      COMMON /PPARMS/ ILN, ILB, ILE, ILT, NALR, NLI, NSTD, NSTDUP, NLIN
C
      CHARACTER INLINE*72, INFIL*20, TFIL*20, TMPLIN*72, MSG*72
      INTEGER   I1, I2, IL, INDEX, ITRIM, I, J1, J2
      LOGICAL   KEEP, ALLOW
C-----------------------------------------------------------------------
      IERR = 0
C                                       simple output
      IF ((OUTLIN(7:13).NE.'INCLUDE') .OR. (OUTLIN(:1).EQ.'C')
     *      .OR. (INLINE(7:17).EQ.'INCLUDE ''($')) THEN
         IF (ILEN.GT.0) WRITE (16,1010,ERR=930) OUTLIN(:ILEN)
C                                       a 2nd-level include
      ELSE
         INLINE = OUTLIN
C                                       Not local INCLUDE declaration
         IF (INLINE(:5).EQ.'LOCAL') THEN
            MSG = 'LOCAL INCLUDE DECLARATION NOT ALLOWED IN AN INCLUDE'
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            GO TO 990
            END IF
C                                       get include name
         TMPLIN = INLINE(14:72)
         I1 = INDEX (TMPLIN, '''')
         I1 = I1 + 1
         I2 = INDEX (TMPLIN(I1:72), '''')
         IF (I2.LE.0) THEN
            MSG = 'PROBLEM PARSING INCLUDE CARD:'
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            MSG = INLINE(1:ILEN)
            WRITE (6,1000) MSG(1:ITRIM(MSG))
            GO TO 990
            END IF
         I2 = I2 + I1 - 2
         INFIL = TMPLIN(I1:I2)
            I1 = INDEX (INFIL, ':')
            I1 = I1 + 1
            TFIL = INFIL(I1:20)
C                                       Already included?
         IF (NALR.GT.0) THEN
            DO 10 I = 1,NALR
               IF (TFIL.EQ.ALRINC(I)) THEN
                  INLINE(:1) = 'C'
                  INLINE(7:13) = 'SKIPPED'
                  WRITE (16,1010,ERR=930) INLINE(1:ILEN)
                  GO TO 999
                  END IF
 10            CONTINUE
            END IF
C                                       Duplication allowed?
         ALLOW = .FALSE.
         DO 15 I = 1,NSTDUP
            IF (TFIL.EQ.STDUPL(I)) ALLOW = .TRUE.
 15         CONTINUE
         IF (.NOT.ALLOW) THEN
            IF (NALR.GE.MXINCL) THEN
               MSG = 'MAX NUMBER OF INCLUDES EXCEEDED AT'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INLINE(1:ILEN)
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               IERR = 2
               GO TO 999
               END IF
            NALR = NALR + 1
            ALRINC(NALR) = TFIL
            END IF
C                                       is it in local list?
         I2 = 0
         DO 20 I = 1,NLI
            IF (TFIL.EQ.INCNAM(I)) I2 = I
 20         CONTINUE
C                                       Yes - dump local from memory
         IF (I2.GT.0) THEN
C                                       warn if logical name
            IF ((I1.NE.1) .AND. (ILT(I2).NE.2)) THEN
               MSG = 'SUBSTITUTED LOCAL INCLUDE FOR:'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               END IF
C                                       write INCL line as Comment
            INLINE(1:1) = 'C'
            WRITE (16,1010,ERR=930) INLINE(1:ILEN)
            J1 = ILB(I2)
            J2 = ILE(I2)
            DO 30 I = J1,J2
               IL = ILN(I)
               WRITE (16,1010,ERR=930) INCBUF(I)(1:IL)
 30            CONTINUE
C                                       No - get external file
         ELSE
            IF (INFIL(1:5).NE.'INCS:') THEN
               MSG = 'WARNING - NON-STANDARD INCLUDE USED:'
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               MSG = INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               END IF
C                                       write INCL line as Comment
            INLINE(1:1) = 'C'
            WRITE (16,1010,ERR=930) INLINE(1:ILEN)
C                                       open INCL file
            CALL PPOPEN (19, INFIL, 'READ', IERR)
            IF (IERR.NE.0) THEN
               MSG = 'CAN''T OPEN INCLUDE FILE ' // INFIL
               WRITE (6,1000) MSG(1:ITRIM(MSG))
               GO TO 999
               END IF
            KEEP = .FALSE.
            DO 35 I = 1,NSTD
               IF (STDINC(I).EQ.TFIL) KEEP = .TRUE.
 35            CONTINUE
            IF (NLI.GE.MXINCN-1) KEEP = .FALSE.
            IF (NLIN.GE.MXNLIN-150) KEEP = .FALSE.
            IF (KEEP) THEN
               NLI = NLI + 1
               ILB(NLI) = NLIN + 1
               ILT(NLI) = 2
               INCNAM(NLI) = TFIL
               END IF
C                                       read INCL file loop
 40         CONTINUE
               READ (19,1010,END=50,ERR=950) INLINE
               ILEN = ITRIM (INLINE)
               IF ((ILEN.GT.0) .AND. (INLINE(:2).NE.'C;')) THEN
                  IF ((INLINE(7:16).EQ.'hollerith ') .OR.
     *               (INLINE(7:16).EQ.'HOLLERITH '))
     *               INLINE(7:16) = 'REAL'
                  IF ((INLINE(7:16).EQ.'longint   ') .OR.
     *               (INLINE(7:16).EQ.'LONGINT   '))
     *               INLINE(7:16) = LINT
                  IF (KEEP) THEN
                     NLIN = NLIN + 1
                     INCBUF(NLIN) = INLINE
                     ILN(NLIN) = ILEN
                     END IF
                  WRITE (16,1010,ERR=955) INLINE(1:ILEN)
                  END IF
               GO TO 40
 50         CLOSE (UNIT=19)
            IF (KEEP) ILE(NLI) = NLIN
            END IF
         END IF
      GO TO 999
C                                       errors
 930  IERR = 1
      MSG = 'ERROR WRITING INCLUDED LINE TO OUTPUT'
      GO TO 995
 950  IERR = 3
      MSG = 'ERROR READING 3rd-LEVEL INCLUDE FILE'
      CLOSE (UNIT=19)
      GO TO 995
 955  IERR = 1
      MSG = 'ERROR WRITING 3rd-LEVEL INCLUDE LINE TO OUTPUT'
      CLOSE (UNIT=19)
      GO TO 995
C
 990  MSG = 'CONTINUING ANYWAY'
 995  WRITE (6,1000) MSG(1:ITRIM(MSG))
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' PP(OUT3): ',A)
 1010 FORMAT (A)
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C-----------------------------------------------------------------------
      ITRIM = LEN (STRING) + 1
C                                       look backwards for non-blank
 10   CONTINUE
         ITRIM = ITRIM - 1
         IF (ITRIM.LT.1) GO TO 999
         IF (STRING(ITRIM:ITRIM).EQ.' ') GO TO 10
C
 999  RETURN
      END
      SUBROUTINE PPOPEN (LUN, FNAME, OPER, IERR)
C-----------------------------------------------------------------------
C   Opens files for pre-processor program PP
C   Inputs:
C      LUN    I       Desired logical unit number
C      FNAME  C*(*)   File name (logical:fname)
C      OPER   C*4     READ or WRIT.
C   Output:
C      IERR   I       Error code: 0 => okay
C                        2 => input error
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
      CHARACTER FNAME*(*), OPER*4
C
      CHARACTER DIREC*128, PATH*256, FILE*128
      INTEGER   ITRIM, I, J, IL, JERR
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) GO TO 999
      IF ((LUN.LT.10) .OR. (LUN.GT.50)) GO TO 999
      IL = ITRIM (FNAME)
      I = INDEX (FNAME, ' ')
      IF (I.LE.0) I = IL + 1
      IF ((IL.LE.0) .OR. (I.LE.IL)) GO TO 999
C                                       Build file name
      J = INDEX (FNAME(1:IL), ':')
C                                       No logical
      IF (J.LE.1) THEN
         PATH = FNAME(J+1:IL)
C                                       Translate logical
      ELSE
         DIREC = FNAME(1:J-1)
         FILE = ' '
         IF (J+1.LE.IL) FILE = FNAME(J+1:IL)
         I = ITRIM (DIREC)
         IL = ITRIM (FILE)
         IF (DIREC(1:I).EQ.'INCS') THEN
            CALL ZPPINC (IL, FILE, 256, PATH, JERR)
         ELSE
            CALL ZPPTRA (I, DIREC, IL, FILE, 256, PATH, JERR)
            END IF
         J = ITRIM (PATH)
         IF ((JERR.NE.0) .OR. (J.LE.0)) THEN
            WRITE (6,1000) DIREC(1:I)
            IERR = 1
            GO TO 999
            END IF
         END IF
C                                       Do open
      CALL ZPPOPN (LUN, PATH, OPER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' PPOPEN: CAN''T TRANSLATE LOGICAL = ',A)
      END
      SUBROUTINE ZPPINC (IL, INCFIL, IM, XLATED, IERR)
C-----------------------------------------------------------------------
C   ZPPINC finds the area containing the include file INCFIL and returns
C   the directory area to that file.  It must foillow the search tree
C   concept and take the lowest occurence for the local machine.
C   Inputs:
C      IL       I       Length of INCFIL
C      INCFIL   C*(*)   Include file name (e.g. DDCH.INC)
C      IM       I       Maximum length of output strinG
C   Output:
C      XLATED   C*IM    Translated file name
C      IERR     I       Error code: 1 => file not found
C                          3 directory tree error
C   UNIX version
C-----------------------------------------------------------------------
      INTEGER   IL, IM, IERR
      CHARACTER INCFIL*(*), XLATED*(*)
C
      INTEGER   NDIREC, I, J, K, LLEN, XLEN, XLNB, ITRIM
      LOGICAL   EXISTS
      CHARACTER LOGNAM*48, TEMP*128, DIREC(8)*128, PATH*128
C-----------------------------------------------------------------------
      IERR = 0
      LOGNAM = 'INCS:'
      LLEN = LEN (LOGNAM)
      XLEN = LEN (XLATED)
      CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         NDIREC = 0
 10      I = INDEX (XLATED, ' ')
         IF (I.GT.1) THEN
            NDIREC = NDIREC + 1
            DIREC(NDIREC) = XLATED (:I)
            TEMP = XLATED(I+1:)
            XLATED = TEMP
            GO TO 10
            END IF
         IERR = 1
         DO 20 I = 1, NDIREC
            EXISTS = .FALSE.
            J = ITRIM (DIREC(I))
            PATH = DIREC(I)(1:J) // '/' // INCFIL
            K = ITRIM (PATH)
            INQUIRE (FILE=PATH(1:K), EXIST=EXISTS)
            IF (EXISTS) THEN
               XLATED = PATH(1:K)
               IERR = 0
               GO TO 999
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZPPTRA (ID, DIREC, IF, FILE, IM, XLATED, IERR)
C-----------------------------------------------------------------------
C   ZPPTRA finds the translation of the logical symbol DIREC and returns
C   it as the full directory name.
C   Inputs:
C      ID       I       Length of DIREC
C      DIREC    C*(*)   logical name
C      IF       I       Length of FILE
C      FILE     C*(*)   File name in DIREC
C      IM       I       Maximum length of output string
C   Output:
C      XLATED   C*IM    Translated file name - full path file name
C      IERR     I       Error code: 1 => file not found
C                          3 directory tree error
C   UNIX version
C-----------------------------------------------------------------------
      INTEGER   ID, IF, IM, IERR
      CHARACTER DIREC*(*), FILE*(*), XLATED*(*)
C
      INTEGER   I, LLEN, XLEN, XLNB, ITRIM
      CHARACTER LOGNAM*48
C-----------------------------------------------------------------------
      IERR = 0
      I = ITRIM (DIREC(1:ID))
      LOGNAM = DIREC(1:I) // ':'
      LLEN = LEN (LOGNAM)
      XLEN = LEN (XLATED)
      CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF ((IERR.EQ.0) .AND. (IF.GT.0)) XLATED(XLNB+1:) = '/' //
     *   FILE(:IF)
C
 999  RETURN
      END
      SUBROUTINE ZPPOPN (LUN, PATH, OPER, IERR)
C-----------------------------------------------------------------------
C   ZPPOPN opens the requested file
C   Inputs:
C      LUN    I       Logical unit number
C      PATH   C*(*)   Full file name
C      OPER   C*4     READ or WRIT
C   Output:
C      IERR     I       Error code: 1 => file not found
C                          3 directory tree error
C   UNIX version
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
      CHARACTER PATH*(*), OPER*4
C
      INTEGER   I, ITRIM
C-----------------------------------------------------------------------
      I = ITRIM (PATH)
C                                       Read
      IF (OPER.EQ.'READ') THEN
         IERR = 1
         OPEN (UNIT=LUN, FILE=PATH(1:I), STATUS='OLD', ERR=900)
      ELSE IF (OPER.EQ.'WRIT') THEN
         IERR = 3
         OPEN (UNIT=LUN, FILE=PATH(1:I), ERR=900)
         REWIND (LUN)
      ELSE
         IERR = 2
         END IF
      IF (IERR.NE.2) IERR = 0
      GO TO 999
C                                       Error
 900  WRITE (6,*) ' ZPPOPN: ERROR ON PATH = ',PATH(1:I)
C
 999  RETURN
      END
      SUBROUTINE ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
C-----------------------------------------------------------------------
C  Translate a logical name (i.e., environment variable).
C  Inputs:
C     LLEN     I          Length of LOGNAM (1-relative)
C     LOGNAM   C*(LLEN)   Logical name
C     XLEN     I          Length of XLATED (1-relative)
C  Output:
C     XLATED   C*(XLEN)   Translation (blank filled)
C     XLNB     I          Position of last non-blank in XLATED
C                         (1-relative)
C     IERR     I          Error return code: 0 => no error
C                             1 => error
C  Generic version - calls ZTRLO2 with HOLLERITHs
C-----------------------------------------------------------------------
      INTEGER   LLEN, XLEN, XLNB, IERR
      CHARACTER LOGNAM*(*), XLATED*(*)
C
      REAL      HOGNAM(64), HLATED(64), HBLANK
C-----------------------------------------------------------------------
       CALL CHR2H (4, '    ', 1, HBLANK)
       CALL RFILL (64, HBLANK, HOGNAM)
       CALL RFILL (64, HBLANK, HLATED)
       XLATED = ' '
       CALL CHR2H (LLEN, LOGNAM, 1, HOGNAM)
       CALL ZTRLO2 (LLEN, HOGNAM, XLEN, HLATED, XLNB, IERR)
       IF (XLNB.GT.0) CALL H2CHR (XLNB, 1, HLATED, XLATED)
C
 999  RETURN
      END
      SUBROUTINE RFILL (NREAL, CONST, AOUT)
C-----------------------------------------------------------------------
C   RFILL fills a real array with a constant
C   Inputs:
C      NREAL   I          Number of real variables to fill
C      CONST   R          Input constant
C   Output:
C      AOUT    R(NREAL)   Output array
C-----------------------------------------------------------------------
      REAL      CONST, AOUT(*)
      INTEGER   NREAL
C
      INTEGER   I
C-----------------------------------------------------------------------
      IF (NREAL.GT.0) THEN
         DO 10 I = 1,NREAL
            AOUT(I) = CONST
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CHR2H (NCH, ISTR, OUTPNT, OSTR)
C-----------------------------------------------------------------------
C   Convert a Fortran CHARACTER variable to an AIPS HOLLERITH string.
C   IF NCH > LEN (ISTR) then blank fill the rest.
C   Inputs:
C      NCH     I      Number of characters
C      ISTR    C*(*)  Input CHARACTER string
C      OUTPNT  I      Start position in output string
C   Output:
C      OSTR    H(*)   Output AIPS string
C-----------------------------------------------------------------------
      INTEGER   NCH, OUTPNT
      CHARACTER ISTR*(*)
      REAL      OSTR(*)
C
      INTEGER   NA, I, J1, J2, IT, NLEN
C-----------------------------------------------------------------------
      NLEN = LEN (ISTR)
      IF (NCH.LE.0) GO TO 999
         NA = MAX (1, OUTPNT)
         DO 10 I = 1,NCH
            J1 = (NA-1) / 4 + 1
            J2 = MOD (NA-1, 4) + 1
            IF (I.LE.NLEN) THEN
               IT = ICHAR (ISTR(I:I))
            ELSE
               IT = ICHAR (' ')
               END IF
            CALL ZPUTCH (IT, OSTR(J1), J2)
            NA = NA + 1
 10         CONTINUE
C
 999  RETURN
      END
      SUBROUTINE H2CHR (NCH, INPNT, ISTR, OSTR)
C-----------------------------------------------------------------------
C   Convert an AIPS HOLLERITH string to a Fortran CHARACTER variable.
C   Blank fills the full ISTR variable.
C   Inputs:
C      NCH    I       Number of characters
C      INPNT  I       Start position in input string
C      ISTR   H(*)    Input AIPS string
C   Output:
C      OSTR   C*(*)   Output CHARACTER string
C-----------------------------------------------------------------------
      INTEGER   NCH, INPNT
      REAL       ISTR(*)
      CHARACTER OSTR*(*)
C
      INTEGER   NA, I, J1, J2, IT, NLEN
C-----------------------------------------------------------------------
      NLEN = LEN (OSTR)
      OSTR = ' '
      IF (NCH.LE.0) GO TO 999
         NA = MAX (1, INPNT)
         NLEN = MIN (NLEN, NCH)
         DO 10 I = 1,NLEN
            J1 = (NA-1) / 4 + 1
            J2 = MOD (NA-1, 4) + 1
            CALL ZGETCH (IT, ISTR(J1), J2)
            OSTR(I:I) = CHAR (IT)
            NA = NA + 1
 10         CONTINUE
C
 999  RETURN
      END
