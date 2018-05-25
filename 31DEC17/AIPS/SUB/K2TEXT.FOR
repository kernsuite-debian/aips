      SUBROUTINE K2TEXT (LUN, RUNFIL, NAME)
C-----------------------------------------------------------------------
C! puts K array in text form to a file
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999-2000, 2002, 2012
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
C   Creates the specified text file, copies the text of the procedures
C   and formats all adverb values to it, then reopenS it for read
C   Inputs:
C      LUN      I       LUN to use
C      RUNFIL   L       T -> to make immediate run file (open quiet,
C                            leave open)
C                       F -> create with message, leave closed
C      NAME     C*(*)   File name
C-----------------------------------------------------------------------
      INTEGER   LUN
      CHARACTER NAME*(*)
      LOGICAL   RUNFIL
C
      INTEGER   FIND, IERR, POTERR, LPROT, L, LL, MM, IL, IT, ILINK, I,
     *   START, NSIZE, NODIM, I1B, I1E, I2B, I2E, I3B, I3E, I4B, I4E,
     *   I1, I2, I3, I4, TAG, JTRIM, NCH
      LOGICAL   EQUAL
      CHARACTER LINE*80, SCR*80, OPCODE*4
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCON.INC'
C-----------------------------------------------------------------------
C                                       create and open the text file
      POTERR = 55
      OPCODE = 'WRIT'
      IF (RUNFIL) OPCODE = 'QWRT'
      CALL ZTXOPN (OPCODE, LUN, FIND, NAME, .FALSE., IERR)
C                                       destroy previous
      IF ((IERR.EQ.5) .AND. (RUNFIL)) THEN
         MSGTXT = 'DELETE PRE-EXISTING COMPRESS SCRATCH FILE'
         CALL MSGWRT (6)
         CALL ZTXZAP (LUN, NAME, IERR)
         IF (IERR.EQ.0) CALL ZTXOPN (OPCODE, LUN, FIND, NAME, .FALSE.,
     *      IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      POTERR = 63
      LPROT = K(7)
      LINE = '*'
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(:1), IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Go through K for procs
      L = 1
 100  L = K(L)
      IF (L.GT.0) THEN
         LL = K(L+1)/16
         MM = K(L+1) - 16*LL
         CALL H2CHR (LL, 1, CH(L+4), LINE)
C                                       only post POPSGN procs
C                                       Not NEWPARMS - is in restore 0
         IF ((L.GE.LPROT) .AND. (MM.EQ.3) .AND. (LINE.NE.'NEWPARMS'))
     *      THEN
            ILINK = K(L+2)
 110        IL = ILINK
               ILINK = K(IL)
               IT = K(IL+1)
               IF (IT.GT.0) THEN
                  LL = IT + 1
                  CALL H2CHR (LISTF(IT), 1, LISTF(LL), LINE)
                  CALL ZTXIO ('WRIT', LUN, FIND, LINE(:LISTF(IT)), IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               IF (ILINK.NE.0) GO TO 110
            END IF
         GO TO 100
         END IF
      LINE = '*'
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(:1), IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Re-loop for adverb values
      L = 1
 120  L = K(L)
      IF (L.GT.0) THEN
         LL = K(L+1)/16
         MM = K(L+1) - 16*LL
         CALL H2CHR (LL, 1, CH(L+4), LINE)
         TAG = K(L+2)
C                                       scalar number
         IF (MM.EQ.1) THEN
            LINE (LL+2:) = '='
            IL = LL + 4
            WRITE (LINE(IL:),1120) C(TAG)
            IL = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUN, FIND, LINE(:IL), IERR)
            IF (IERR.NE.0) GO TO 970
C                                       array number
         ELSE IF (MM.EQ.2) THEN
            START = K(L+3)
            NSIZE = K(START)
            EQUAL = .TRUE.
            DO 125 I = 1,NSIZE
               IF (C(TAG).NE.C(TAG+I-1)) EQUAL = .FALSE.
 125           CONTINUE
            LINE (LL+2:) = '= 0.0'
            IL = LL + 4
            IF (EQUAL) WRITE (LINE(IL:),1120) C(TAG)
            IL = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUN, FIND, LINE(:IL), IERR)
            IF (IERR.NE.0) GO TO 970
            IF (.NOT.EQUAL) THEN
               NODIM = K(START+1)
               I1B = K(START+2)
               I1E = K(START+3) - I1B + 1
               IF (NODIM.GE.2) THEN
                  I2B = K(START+4)
                  I2E = K(START+5) - I2B + 1
               ELSE
                  I2B = 1
                  I2E = 1
                  END IF
               IF (NODIM.GE.3) THEN
                  I3B = K(START+6)
                  I3E = K(START+7) - I3B + 1
               ELSE
                  I3B = 1
                  I3E = 1
                  END IF
               IF (NODIM.GE.4) THEN
                  I4B = K(START+8)
                  I4E = K(START+9) - I4B + 1
               ELSE
                  I4B = 1
                  I4E = 1
                  END IF
               DO 130 I4 = I4B,I4E
               DO 130 I3 = I3B,I3E
               DO 130 I2 = I2B,I2E
               DO 130 I1 = I1B,I1E
                  IF (C(TAG).NE.0.0) THEN
                     IL = LL + 1
                     WRITE (LINE(IL:),1125) '(', I1
                     IL = IL + 5
                     IF (NODIM.GE.2) THEN
                        WRITE (LINE(IL:),1125) ',', I2
                        IL = IL + 5
                        END IF
                     IF (NODIM.GE.3) THEN
                        WRITE (LINE(IL:),1125) ',', I3
                        IL = IL + 5
                        END IF
                     IF (NODIM.GE.4) THEN
                        WRITE (LINE(IL:),1125) ',', I4
                        IL = IL + 5
                        END IF
                     LINE (IL:) = ') = '
                     IL = IL + 4
                     WRITE (LINE(IL:),1120) C(TAG)
                     IL = JTRIM (LINE)
                     CALL ZTXIO ('WRIT', LUN, FIND, LINE(:IL), IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  TAG = TAG + 1
 130              CONTINUE
               END IF
C                                       character strings
         ELSE IF (MM.EQ.7) THEN
            START = K(L+3)
            NSIZE = K(START)
            NODIM = K(START+1)
            NCH = K(START+3)
            IL = LL + 5
            IF (NODIM.EQ.1) THEN
               LINE (LL+2:) = '= '''
               CALL H2CHR (NCH, 1, CH(TAG), LINE(IL:))
               IL = JTRIM(LINE)
C                                       let lower case survive
C              IL - IL + 1
C              LINE(IL:IL) = ''''
            ELSE
               LINE (LL+2:) = '= '' '''
               IL = IL + 1
               END IF
            CALL ZTXIO ('WRIT', LUN, FIND, LINE(:IL), IERR)
            IF (IERR.NE.0) GO TO 970
            IF (NODIM.GT.1) THEN
               I2B = K(START+4)
               I2E = K(START+5) - I2B + 1
               IF (NODIM.GE.3) THEN
                  I3B = K(START+6)
                  I3E = K(START+7) - I3B + 1
               ELSE
                  I3B = 1
                  I3E = 1
                  END IF
               IF (NODIM.GE.4) THEN
                  I4B = K(START+8)
                  I4E = K(START+9) - I4B + 1
               ELSE
                  I4B = 1
                  I4E = 1
                  END IF
               DO 140 I4 = I4B,I4E
               DO 140 I3 = I3B,I3E
               DO 140 I2 = I2B,I2E
                  CALL H2CHR (NCH, 1, CH(TAG), SCR)
                  IF (SCR.NE.' ') THEN
                     IL = LL + 1
                     WRITE (LINE(IL:),1125) '(', I2
                     IL = IL + 5
                     IF (NODIM.GE.3) THEN
                        WRITE (LINE(IL:),1125) ',', I3
                        IL = IL + 5
                        END IF
                     IF (NODIM.GE.4) THEN
                        WRITE (LINE(IL:),1125) ',', I4
                        IL = IL + 5
                        END IF
                     LINE (IL:) = ') = ''' // SCR
                     IL = JTRIM (LINE)
C                                       let lower case survive
C                    IL = IL + 1
C                    LINE(IL:IL) = ''''
                     CALL ZTXIO ('WRIT', LUN, FIND, LINE(:IL), IERR)
                     IF (IERR.NE.0) GO TO 970
                     END IF
                  TAG = TAG + NCH/4
 140              CONTINUE
               END IF
            END IF
         GO TO 120
         END IF
C                                       success: reopen for read
 900  POTERR = 56
      CALL ZTXCLS (LUN, FIND, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (RUNFIL) THEN
         POTERR = 55
         CALL ZTXOPN ('QRED', LUN, FIND, NAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      GO TO 999
C                                       destroy on failure
 970  CALL ZTXCLS (LUN, FIND, IERR)
      CALL ZTXZAP (LUN, NAME, IERR)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = 'K2TEXT'
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT (1PE14.7)
 1125 FORMAT (A1,I4)
      END


