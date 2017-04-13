      SUBROUTINE UVCREA (IVOL, CNO, WBUFF, IERR)
C-----------------------------------------------------------------------
C! create and catalog a uv data base file
C# UV-util Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004
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
C   Subroutine to create a uv file using the parameters in a CATBLK.
C   The file will be cataloged and marked with WRITE status.  The image
C   name parameters must be filled in except that the physical type is
C   converted to 'UV'.  The OUTSEQ default is applied (0 => highest
C   matching+1).  The name must be unique ignoring the physical type.
C   The extension file areas of the CATBLK are cleared and the
C   "DATE-MAP" string is filled in.
C   In/Outs:
C      IVOL      I        Volume # on which to put file.  0 => any
C                         on output is volume used (IERR = 0)
C   Outputs:
C      WBUFF     I(256)   Working buffer
C      CNO       I        Catalog slot number
C      IERR      I        Error code; 0 => o.k.
C                                     1 => couldnt create,no room
C                                     2 => no create, duplicate name
C                                     3 => no room in catalog
C                                     4 => i/o problem on catalog
C                                     5 => Other Create errors
C                                     6 => No catalog file on disk
C                                     7 => no create, duplicate name on
C                                          another disk (IVOL > 0)
C   COMMON: /MAPHDR/ catalog block used a lot, final seq # on output
C-----------------------------------------------------------------------
      INTEGER   IVOL, CNO, WBUFF(256), IERR
C
      CHARACTER STAT*4, CTEXT*8, PNAME*48, NAME*12, CLASS*6,
     *   TYPTMP*2
      INTEGER   IER, SEQNO, SNO, J, I, ID(3), ITEMP, LREC, ASIZE, SIZE
      LOGICAL   T, LVOL, IAMOK
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Expand name and class.
      SEQNO = CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      LVOL = IVOL.LE.0
      IF (LVOL) IVOL = NVOL
C                                       Check for default seqno
      IF (SEQNO.GT.0) GO TO 10
         SNO = 0
         ITEMP = 0
         CNO = 1
         TYPTMP = ' '
         CALL CATDIR ('SRCH', ITEMP, CNO, NAME, CLASS, SNO, TYPTMP,
     *      CATBLK(KIIMU), STAT, WBUFF, IER)
         IF ((IER.GT.0) .AND. (IER.NE.5)) GO TO 25
         SEQNO = SNO + 1
         IF (IER.EQ.5) SEQNO = 1
C                                       Is user name unique?
 10   IERR = 0
      IF (DSKLOK(IVOL).EQ.1) THEN
         IF (LVOL) THEN
            IVOL = IVOL - 1
            IF (IVOL.GT.0) GO TO 10
            END IF
         IERR = 5
         MSGTXT = 'UVCREA: CANNOT CREATE ON READ-ONLY FILE SYSTEM'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      SNO = SEQNO
      CNO = 1
      TYPTMP = ' '
      ITEMP = 0
      CALL CATDIR ('SRNN', ITEMP, CNO, NAME, CLASS, SNO, TYPTMP,
     *   CATBLK(KIIMU), STAT, WBUFF, IER)
      IF (IER.EQ.5) GO TO 20
      IF (IER.NE.0) GO TO 15
         IF ((IVOL.LE.0) .OR. (IVOL.EQ.ITEMP)) THEN
            IERR = 2
            WRITE (MSGTXT,1010) NAME, CLASS, SNO
            CALL MSGWRT (7)
            GO TO 999
         ELSE
            IERR = 7
            WRITE (MSGTXT,1011) NAME, CLASS, SNO, ITEMP
            END IF
         GO TO 980
C                                       missing catalog
 15   IF (IER.NE.1) GO TO 25
         IERR = 6
         IF (LVOL) GO TO 975
         IERR = 0
C                                       Allowed?
 20   IF (.NOT.IAMOK(IVOL,'UV')) THEN
         IERR = 1
         IF ((LVOL) .AND. (IVOL.GT.1)) GO TO 975
         WRITE (MSGTXT,1020) NAME, CLASS, SNO, IVOL
         GO TO 980
         END IF
C                                       create catalog slot
      CNO = 1
      CATBLK(KIIMS) = SNO
      TYPTMP = 'UV'
      CALL CATDIR ('OPEN', IVOL, CNO, NAME, CLASS, SNO, TYPTMP,
     *   CATBLK(KIIMU), STAT, WBUFF, IER)
      IF (IER.EQ.0) GO TO 30
C                                       missing catalog
         IF (IER.NE.1) GO TO 22
            IERR = 6
            GO TO 975
C                                       No room in catalog
 22      IF (IER.NE.5) GO TO 25
            WRITE (MSGTXT,1022) IVOL
            IERR = 3
            GO TO 980
C                                       I/O error in catalog
 25      CONTINUE
            WRITE (MSGTXT,1025) IER
            IERR = 4
            GO TO 980
C                                       Clear extension file area
 30   CALL CATCLR (CATBLK)
C                                       Update DATE-MAP area
      CALL ZDATE (ID)
      WRITE (CTEXT,1030) ID(1), ID(2), ID(3)
      CALL CHR2H (8, CTEXT, 1, CATH(KHDMP))
C                                       Put header into catalog
      CALL CATIO ('UPDT', IVOL, CNO, CATBLK, 'REST', WBUFF, IER)
      IF (IER.EQ.0) GO TO 40
         IERR = 4
         WRITE (MSGTXT,1031) IER
         CALL MSGWRT (8)
         GO TO 970
C                                       Generate physical name
 40   CALL ZPHFIL ('UV', IVOL, CNO, 1, PNAME, IER)
C                                       Figure out how big
      LREC = 1
      J = CATBLK(KIDIM)
      DO 45 I = 1,J
         LREC = LREC * CATBLK(KINAX+I-1)
 45      CONTINUE
      LREC = LREC + CATBLK(KIPCN)
      CALL UVSIZE (LREC, CATBLK(KIGCN), SIZE)
C                                      Create the file
      CALL ZCREAT (IVOL, PNAME, SIZE, T, ASIZE, IER)
      IF (IER.EQ.0) THEN
         WRITE (MSGTXT,1040) NAME, CLASS, SNO, IVOL, CNO
         CALL MSGWRT (2)
         GO TO 999
      ELSE
         IERR = 1
         IF ((IER.NE.3) .AND. (IER.NE.5)) IERR = 5
         IF ((.NOT.LVOL) .OR. (IERR.NE.1) .OR. (IVOL.LE.1)) THEN
            WRITE (MSGTXT,1050) IER, NAME, CLASS, SNO
            IF (IER.EQ.3) WRITE (MSGTXT,1051) NAME, CLASS, SNO
            IF (IER.EQ.5) WRITE (MSGTXT,1020) NAME, CLASS, SNO, IVOL
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Errors:
C                                       Clear write status.
 970  TYPTMP = 'UV'
      CALL CATDIR ('CSTA', IVOL, CNO, NAME, CLASS, SNO, TYPTMP,
     *   CATBLK(KIIMU), 'CLWR', WBUFF, IER)
C                                       Clear catalog block
      TYPTMP = 'UV'
      CALL CATDIR ('CLOS', IVOL, CNO, NAME, CLASS, SNO, TYPTMP,
     *   CATBLK(KIIMU), STAT, WBUFF, IER)
 975  IF ((.NOT.LVOL) .OR. ((IERR.NE.1) .AND. (IERR.NE.6))) GO TO 999
         IVOL = IVOL - 1
         IF (IVOL.GT.0) GO TO 10
         GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVCREA: DUPLICATE NAME ',A12,'.',A6,'.',I4)
 1011 FORMAT ('MCREAT: DUPLICATE NAME ',A12,'.',A6,'.',I4,
     *   ' BUT ON DISK',I3)
 1020 FORMAT ('UVCREA: ',A12,'.',A6,'.',I4,' PROHIBITED ON DISK',I3)
 1022 FORMAT ('UVCREA: CATALOG FULL ON VOL ',I1)
 1025 FORMAT ('UVCREA: CATDIR IO ERROR=',I7)
 1030 FORMAT (I4.4,2I2.2)
 1031 FORMAT ('UVCREA: CATIO I/O ERROR=',I7)
 1040 FORMAT ('Create ',A12,'.',A6,'.',I4,' (UV)  on disk',I3,
     *   '  cno',I5)
 1050 FORMAT ('ZCREAT ERROR',I6,' FOR ',A12,'.',A6,'.',I4)
 1051 FORMAT ('UVCREA: NO ROOM FOR ',A12,'.',A6,'.',I4)
      END
