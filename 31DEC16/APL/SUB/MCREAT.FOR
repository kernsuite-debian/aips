      SUBROUTINE MCREAT (IVOL, CNO, WBUFF, IERR)
C-----------------------------------------------------------------------
C! create and catalog a map file
C# Map-util Catalog
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
C   Subroutine to create a map file using the parameters in a CATBLK.
C   The file will be cataloged and marked with WRITE status.  The image
C   name parameters incl. physical type must be filled in.  A blank
C   physical type is converted to 'MA'.  The OUTSEQ default is applied
C   (0 => highest matching+1).  The name must be unique ignoring the
C   physical type.  The extension file areas of the CATBLK are cleared
C   and the "DATE-MAP" string is filled in.
C   In/Outs:
C      IVOL      I        Volume # on which to put file: 0 => ALL
C                         on output has volume used
C      WBUFF     I(256)   Working buffer
C   Outputs:
C      CNO       I        Catalog slot number
C      IERR      I        Error code; 0 => o.k.
C                                     1 => couldnt create,no room
C                                     2 => no create, duplicate name
C                                     3 => no room in catalog
C                                     4 => i/o problem on catalog
C                                     5 => Other Create errors
C                                     6 => no catalog file
C                                     7 => no create, duplicate name on
C                                          another disk (IVOL > 0)
C   Common: (in/out)
C      CATBLK    I(256)   Catalog block (via common MAPHDR)
C      CATB4     R(256)   Catalog block (equivalenced to CATBLK)
C-----------------------------------------------------------------------
      INTEGER   IVOL, CNO, WBUFF(256), IERR
C
      CHARACTER LTYPE*2, STAT*4, NAME*12, CLAS*6, PTYPE*2, PNAME*48
      INTEGER   SEQNO, SNO, IER, ITEMP, ID(3), SIZE, ASIZE
      LOGICAL   T, LVOL, IAMOK
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAS)
      CALL H2CHR (2, KHPTYO, CATR(KHPTY), PTYPE)
      IF (PTYPE.EQ.' ') PTYPE = 'MA'
      CALL CHR2H (2, PTYPE, KHPTYO, CATH(KHPTY))
      CALL CATCLR (CATBLK)
      LVOL = IVOL.EQ.0
      IF (LVOL) IVOL = NVOL
C                                      Check for default seqno
      SEQNO = CATBLK(KIIMS)
C                                       get highest sequence #
      IF (SEQNO.GT.0) GO TO 10
         SNO = 0
         ITEMP = 0
         CNO = 1
         LTYPE = ' '
         CALL CATDIR ('SRCH', ITEMP, CNO, NAME, CLAS, SNO, LTYPE,
     *      CATBLK(KIIMU), STAT, WBUFF, IER)
         IF ((IER.GT.0) .AND. (IER.NE.5)) GO TO 25
         SEQNO = SNO + 1
         IF (IER.EQ.5) SEQNO = 1
 10   SNO = SEQNO
      IERR = 0
      IF (DSKLOK(IVOL).EQ.1) THEN
         IF (LVOL) THEN
            IVOL = IVOL - 1
            IF (IVOL.GT.0) GO TO 10
            END IF
         IERR = 5
         MSGTXT = 'MCREAT: CANNOT CREATE ON READ-ONLY FILE SYSTEM'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                      Is user name unique?
      CNO = 1
      LTYPE = ' '
      ITEMP = 0
      CALL CATDIR ('SRNN', ITEMP, CNO, NAME, CLAS, SNO, LTYPE,
     *   CATBLK(KIIMU), STAT, WBUFF, IER)
      IF (IER.EQ.5) GO TO 20
      IF (IER.NE.0) GO TO 15
         IF ((IVOL.LE.0) .OR. (IVOL.EQ.ITEMP)) THEN
            IERR = 2
            WRITE (MSGTXT,1010) NAME, CLAS, SNO
            CALL MSGWRT (7)
            GO TO 999
         ELSE
            IERR = 7
            WRITE (MSGTXT,1011) NAME, CLAS, SNO, ITEMP
            END IF
         GO TO 980
 15   IF (IER.NE.1) GO TO 25
         IERR = 6
         IF (LVOL) GO TO 975
         IERR = 0
C                                       Allowed?
 20   IF (.NOT.IAMOK(IVOL,'MA')) THEN
         IERR = 1
         IF ((LVOL) .AND. (IVOL.GT.1)) GO TO 975
            WRITE (MSGTXT,1020) NAME, CLAS, SNO, IVOL
            CALL MSGWRT (7)
            GO TO 999
         END IF
C                                      create catalog slot
      CNO = 1
      CATBLK(KIIMS) = SNO
      CALL CATDIR ('OPEN', IVOL, CNO, NAME, CLAS, SNO, PTYPE,
     *   CATBLK(KIIMU), STAT, WBUFF, IER)
      IF (IER.EQ.0) GO TO 30
C                                      No catalog file
         IF (IER.NE.1) GO TO 22
            IERR = 6
            GO TO 975
C                                      No room in catalog
 22      IF (IER.NE.5) GO TO 25
            WRITE (MSGTXT,1022) IVOL
            IERR = 3
            GO TO 980
C                                      I/O error in catalog
 25      CONTINUE
            WRITE (MSGTXT,1025) IER
            IERR = 4
            GO TO 980
C                                       Update DATE-MAP
 30   CALL ZDATE (ID)
      WRITE (MSGTXT,1030) ID(1), ID(2), ID(3)
      CALL CHR2H (8, MSGTXT, 1, CATH(KHDMP))
C                                       Put header on disk
      CALL CATIO ('UPDT', IVOL, CNO, CATBLK, 'REST', WBUFF, IER)
      IF (IER.EQ.0) GO TO 40
         IERR = 4
         WRITE (MSGTXT,1031) IER
         CALL MSGWRT (6)
         GO TO 970
C                                      Generate physical name
 40   CALL ZPHFIL (PTYPE, IVOL, CNO, 1, PNAME, IER)
C                                      Figure out how big
      CALL MAPSIZ (CATBLK(KIDIM), CATBLK(KINAX), SIZE)
C                                      Create the file
      CALL ZCREAT (IVOL, PNAME, SIZE, T, ASIZE, IER)
      IF (IER.EQ.0) THEN
         WRITE (MSGTXT,1040) NAME, CLAS, SNO, PTYPE, IVOL, CNO
         CALL MSGWRT (2)
         GO TO 999
      ELSE
         IERR = 1
         IF ((IER.NE.3) .AND. (IER.NE.5)) IERR = 5
         IF ((.NOT.LVOL) .OR. (IERR.NE.1) .OR. (IVOL.LE.1)) THEN
            IF (IERR.NE.1) THEN
               WRITE (MSGTXT,1050) IER, NAME, CLAS, SNO
               CALL MSGWRT (8)
            ELSE
               IF (IER.EQ.3) WRITE (MSGTXT,1051) NAME, CLAS, SNO
               IF (IER.EQ.5) WRITE (MSGTXT,1020) NAME, CLAS, SNO, IVOL
               CALL MSGWRT (7)
               END IF
            END IF
         END IF
C                                      Errors:
C                                      Clear write status
 970  CALL CATDIR ('CSTA', IVOL, CNO, NAME, CLAS, SNO, PTYPE,
     *   CATBLK(KIIMU), 'CLWR', WBUFF, IER)
C                                       Clear catalog block
      CALL CATDIR ('CLOS', IVOL, CNO, NAME, CLAS, SNO, PTYPE,
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
 1010 FORMAT ('MCREAT: DUPLICATE NAME ',A12,'.',A6,'.',I4)
 1011 FORMAT ('MCREAT: DUPLICATE NAME ',A12,'.',A6,'.',I4,
     *   ' BUT ON DISK',I3)
 1020 FORMAT ('MCREAT: ',A12,'.',A6,'.',I4,' PROHIBITED ON DISK',I3)
 1022 FORMAT ('MCREAT: CATALOG FULL ON VOL ',I1)
 1025 FORMAT ('MCREAT: CATDIR IO ERROR=',I4)
 1030 FORMAT (I4.4,I2.2,I2.2)
 1031 FORMAT ('MCREAT: CATIO I/O ERROR=',I4)
 1040 FORMAT ('Create ',A12,'.',A6,'.',I4,' (',A2,')  on disk',I3,
     *   '  cno',I5)
 1050 FORMAT ('ZCREAT ERROR',I6,' FOR ',A12,'.',A6,'.',I4)
 1051 FORMAT ('MCREAT: NO ROOM FOR ',A12,'.',A6,'.',I4)
      END
