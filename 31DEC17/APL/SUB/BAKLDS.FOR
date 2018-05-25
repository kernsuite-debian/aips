      SUBROUTINE BAKLDS (OPER, IERR)
C-----------------------------------------------------------------------
C! manage catalog entries for BAKLD as it reads the tape made by BAKTP
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000
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
C   Machine-independent code to call from ZBKLD2
C   Input:
C      OPER   C*4   'UPDT' updates the image header with actual output
C                          image names.
C                   'CREA' create new entry
C   Output:
C      IERR   I      Error return: 0 => okay
C                          1 => couldnt create,no room
C                          2 => no create, duplicate name
C                          3 => no room in catalog
C                          4 => i/o problem on catalog
C                          5 => Other Create errors
C                          6 => no catalog file
C   Common: (in/out)
C      /MAPHDR/ CATBLK  Catalog block
C   Subroutine to create a map file using the parameters in a CATBLK.
C   The file will be cataloged and marked with WRITE status.  The image
C   name parameters incl. physical type must be filled in.  The OUTSEQ
C   default is applied (0 => highest matching+1).  The name must be
C   unique ignoring the physical type.  The extension file areas of
C   the CATBLK are cleared and the "DATE-MAP" string is filled in.
C   This subroutine is a clone of MCREAT.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   IERR
C
      INTEGER   WBUFF(256), SNO, IER, ITEMP, KERR
      CHARACTER LTYPE*2, STAT*4, PTYPE*2
      LOGICAL   IAMOK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 7
C                                       update the CB file info:
      IF (OPER.EQ.'UPDT') THEN
         CALL CATIO ('READ', IVOL, CNO, CATBLK, 'REST', WBUFF, IERR)
         CALL CHR2H (12, OUTN, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, OUTCL, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = OUTS
         CATBLK(KIIMU) = USER
         CALL CATIO ('UPDT', IVOL, CNO, CATBLK, 'CLWR', WBUFF, IERR)
         IERR = 0
C                                       Creat the output file
      ELSE IF (OPER.EQ.'CREA') THEN
C                                       crack the file name stuff:
         INN = BAKREC(35:)
         INCL = BAKREC(48:)
         READ (BAKREC,1000) INS
         CALL H2CHR (12, 1, NAME, OUTN)
         CALL H2CHR (6, 1, CLASS, OUTCL)
         OUTS = SEQ4 + 0.01
         CALL MAKOUT (INN, INCL, INS, INCL, OUTN, OUTCL, OUTS)
C                                       Generate directory entry:
         CALL FILL (256, 0, CATBLK)
         PTYPE = BAKREC(32:33)
         IF (PTYPE.EQ.' ') PTYPE = 'MA'
         CALL CHR2H (2, PTYPE, KHPTYO, CATH(KHPTY))
         CALL CHR2H (12, OUTN, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, OUTCL, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = OUTS
C                                       Subroutine inserted
C                                       BKLDIR (IVOL, CNO, WBUFF, IERR)
         CALL CATCLR (CATBLK)
C                                      Check for default seqno
         KERR = 0
C                                       get highest sequence #
 5       IF (OUTS.LE.0) THEN
            SNO = 0
            ITEMP = 0
            CNO = 1
            LTYPE = ' '
            CALL CATDIR ('SRCH', ITEMP, CNO, OUTN, OUTCL, SNO, LTYPE,
     *         CATBLK(KIIMU), STAT, WBUFF, IER)
            IF ((IER.GT.0) .AND. (IER.NE.5)) GO TO 25
            OUTS = SNO + 1
            IF (IER.EQ.5) OUTS = 1
            END IF
 10      SNO = OUTS
         IERR = 0
C                                      Is user name unique?
         CNO = 1
         LTYPE = ' '
         ITEMP = 0
         CALL CATDIR ('SRNN', ITEMP, CNO, OUTN, OUTCL, SNO, LTYPE,
     *      CATBLK(KIIMU), STAT, WBUFF, IER)
C                                      Error or found the name
         IF (IER.NE.5) THEN
            IF (IER.EQ.0) THEN
               IERR = 2
               WRITE (MSGTXT,1010) OUTN, OUTCL, SNO
               CALL MSGWRT (8)
               KERR = KERR + 1
               IF (KERR.LT.5) GO TO 5
               GO TO 980
            ELSE
               IF (IER.NE.1) GO TO 25
               IERR = 0
               END IF
            END IF
C                                       Allowed?
 20      IF (.NOT.IAMOK(IVOL,PTYPE)) THEN
            IERR = 1
            WRITE (MSGTXT,1020) OUTN, OUTCL, SNO, IVOL
            GO TO 980
            END IF
C                                      create catalog slot
         CNO = 1
         CATBLK(KIIMS) = SNO
         OUTS = SNO
         CALL CATDIR ('OPEN', IVOL, CNO, OUTN, OUTCL, SNO, PTYPE,
     *      CATBLK(KIIMU), STAT, WBUFF, IER)
         IF (IER.EQ.0) GO TO 30
C                                      No catalog file
            IF (IER.NE.1) GO TO 22
               IERR = 6
               GO TO 975
C                                      No room in catalog
 22         IF (IER.NE.5) GO TO 25
               WRITE (MSGTXT,1022) IVOL
               IERR = 3
               GO TO 980
C                                      I/O error in catalog
 25         CONTINUE
               WRITE (MSGTXT,1025) IER
               IERR = 4
               GO TO 980
C                                       We do not create the file here
 30      WRITE (MSGTXT,1030) OUTN, OUTCL, SNO, PTYPE, IVOL, CNO
         CALL MSGWRT (2)
         GO TO 999
C                                      Errors:
 975     WRITE (MSGTXT,1975)
C
 980     CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (54X,I4)
 1010 FORMAT ('BKLDIR: DUPLICATE NAME ',A12,'.',A6,'.',I4,
     *   ' - WILL RENUMBER')
 1020 FORMAT ('BLKDIR: ',A12,'.',A6,'.',I4,' PROHIBITED ON DISK',I3)
 1022 FORMAT ('BKLDIR: CATALOG FULL ON VOL ',I1)
 1025 FORMAT ('BKLDIR: CATDIR IO ERROR=',I4)
 1030 FORMAT ('Create ',A12,'.',A6,'.',I4,' (',A2,')  on disk',I2,
     *   ' cno',I5)
 1975 FORMAT ('UNABLE TO CREATE SLOT ON SPECIFIED VOLUME')
      END
