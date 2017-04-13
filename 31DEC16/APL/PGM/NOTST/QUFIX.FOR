LOCAL INCLUDE 'QUFIX.INC'
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER UVNAME*12, UVCLAS*6, IMNAM2*12, IMCLA2*6, IMNAM3*12,
     *   IMCLA3*6, CALIN*48, OUTEXT*48
      HOLLERITH XNAME1(3), XCLA1(2), XNAME2(3), XCLA2(2), XNAME3(3),
     *   XCLA3(2), XCALIN(12), XOUTXT(12)
      REAL      XS1, XDIS1, XS2, XDIS2, XS3, XDIS3, PCUT, XNBOX,
     *   XBOXS(4,50)
      INTEGER   DISK1, SEQ1, CNO1, DISK2, SEQ2, CNO2, DISK3, SEQ3, CNO3,
     *   NBOX, CLBOX(4,50), CATI1(256), CATI2(256), CATI3(256),
     *   BUFFER(512)
      DOUBLE PRECISION  FOFF(MAXIF), CATD1(128), CATD2(128), CATD3(128)
      REAL      FINC(MAXIF), CATR1(256), CATR2(256), CATR3(256)
      HOLLERITH CATH1(256), CATH2(256), CATH3(256)
      EQUIVALENCE (CATI1, CATR1, CATD1, CATH1)
      EQUIVALENCE (CATI2, CATR2, CATD2, CATH2)
      EQUIVALENCE (CATI3, CATR3, CATD3, CATH3)
C
      COMMON /INPARM/ XNAME1, XCLA1, XS1, XDIS1, XNAME2, XCLA2, XS2,
     *   XDIS2, XNAME3, XCLA3, XS3, XDIS3, PCUT, XNBOX, XBOXS, XCALIN,
     *   XOUTXT
      COMMON /CQUFIX/ UVNAME, UVCLAS, IMNAM2, IMCLA2, IMNAM3, IMCLA3,
     *   CALIN, OUTEXT
      COMMON /PQUFIX/ FOFF, FINC, BUFFER, DISK1, SEQ1, CNO1, DISK2,
     *   SEQ2, CNO2, DISK3, SEQ3, CNO3, NBOX, CLBOX
      COMMON /MAPHDR/ CATD1, CATD2, CATD3
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM QUFIX
C-----------------------------------------------------------------------
C! QUFIX determines RL phase corrections and applies them spectrally
C# Calibration UV VLA VLB polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2015
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
C   Task QUFIX prints data from uv data files in a variety of forms.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         UVNAME        UV File name to be corrected
C      INCLASS        UVCLAS        File class
C      INSEQ          SEQ1          File sequence number.
C      INDISK         DISK1         Disk volumn on which file resides.
C      IN2NAME        IMNAM2        Image file name good cal
C      IN2CLASS       IMCLA2        File class to be listed.
C      IN2SEQ         SEQ           File sequence number.
C      IN2DISK        DISK          Disk volumn on which file resides.
C      IN3NAME        IMNAM3        Image File name to be corrected
C      IN3CLASS       IMCLA3        File class
C      IN3SEQ         SEQ3          File sequence number.
C      IN3DISK        DISK3         Disk volumn on which file resides.
C      PCUT           PCUT          Min sqrt(q*q+u*u) to include
C      NBOXES         NBOX          Number boxes
C      CLBOX          CLBOX         Search windows
C      CALIN          CALIN         Text file instead of images
C      OUTTEXT        OUTEXT        Text file out rather than correction
C-----------------------------------------------------------------------
      INCLUDE 'QUFIX.INC'
      CHARACTER PRGM*6
      INTEGER   IRET, NVAL, NCH, NIF
      REAL      PDCOR(MAXCIF)
      DOUBLE PRECISION PDFREQ(MAXCIF)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'QUFIX '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL QUFIXI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get correction list
      IF (CALIN.NE.' ') THEN
         CALL QUFIXC (NVAL, PDFREQ, PDCOR, IRET)
      ELSE
         CALL QUFIXD (NVAL, PDFREQ, PDCOR, IRET)
         END IF
      IF (IRET.NE.0) GO TO 990
C                                       output as text
      IF (OUTEXT.NE.' ') THEN
         CALL QUFIXO (NVAL, PDFREQ, PDCOR, IRET)
C                                       translate and correct
      ELSE
         CALL QUFIXT (NVAL, PDFREQ, PDCOR, NCH, NIF)
         CALL QUFIXA (NCH, NIF, PDCOR, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL QUFIXH
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE QUFIXI (PRGN, IRET)
C-----------------------------------------------------------------------
C   QUFIXI gets input parameters for QUFIX
C   Inputs:
C      PRGN    C*6       Program name (2 chars/word)
C   Output:
C      IRET    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in QUFIX for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      INCLUDE 'QUFIX.INC'
      CHARACTER STAT*4, STATUS*4, TYPE*2, BNDCOD(MAXIF)*8
      INTEGER   IROUND, NPARM, IERR, I, LUN, NIF, IIVER, J, FRQSEL,
     *   ISBAND(MAXIF)
      REAL      EPS
      LOGICAL   FAIL
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      FAIL = .FALSE.
C                                       Get input parameters.
      NPARM = 247
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAME1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAME1, UVNAME)
      CALL H2CHR (6, 1, XCLA1, UVCLAS)
      CALL H2CHR (12, 1, XNAME2, IMNAM2)
      CALL H2CHR (6, 1, XCLA2, IMCLA2)
      CALL H2CHR (12, 1, XNAME3, IMNAM3)
      CALL H2CHR (6, 1, XCLA3, IMCLA3)
      CALL H2CHR (48, 1, XCALIN, CALIN)
      CALL H2CHR (48, 1, XOUTXT, OUTEXT)
C                                       Crunch input parameters.
      SEQ1 = IROUND (XS1)
      SEQ2 = IROUND (XS2)
      SEQ3 = IROUND (XS3)
      DISK1 = IROUND (XDIS1)
      DISK2 = IROUND (XDIS2)
      DISK3 = IROUND (XDIS3)
      CALL FILL (200, 0, CLBOX)
      IF (CALIN.NE.' ') THEN
         NBOX = 0
      ELSE
         NBOX = IROUND (XNBOX)
         DO 20 J = 1,NBOX
            DO 10 I = 1,4
               CLBOX(I,J) = IROUND (XBOXS(I,J))
 10            CONTINUE
 20         CONTINUE
         END IF
C                                       Get UV if correcting
      IF (OUTEXT.EQ.' ') THEN
         CNO1 = 1
         TYPE = 'UV'
         CALL CATDIR ('SRCH', DISK1, CNO1, UVNAME, UVCLAS, SEQ1, TYPE,
     *      NLUSER, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, UVNAME, UVCLAS, SEQ1, DISK1,
     *         NLUSER
            GO TO 990
            END IF
         STATUS = 'WRIT'
         CALL CATIO ('READ', DISK1, CNO1, CATI1, STATUS, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, 'READ HEADER OF UV DATA'
            GO TO 990
            END IF
C                                       Mark in CFILES
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK1
         FCNO(NCFILE) = CNO1
         FRW(NCFILE) = 1
C                                       Get uv header info.
         CALL UVPGET (IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get IF freq offset.
         LUN = 28
         IIVER = 1
         FRQSEL = 1
         CALL CHNDAT ('READ', BUFFER, DISK1, CNO1, IIVER, CATI1, LUN,
     *      NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.GT.0) GO TO 999
         END IF
C                                       Get MA if computing
      IF (CALIN.EQ.' ') THEN
         CNO2 = 1
         TYPE = 'MA'
         CALL CATDIR ('SRCH', DISK2, CNO2, IMNAM2, IMCLA2, SEQ2, TYPE,
     *      NLUSER, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, IMNAM2, IMCLA2, SEQ2, DISK2,
     *         NLUSER
            GO TO 990
            END IF
         STATUS = 'READ'
         CALL CATIO ('READ', DISK2, CNO2, CATI2, STATUS, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, 'READ HEADER OF MA DATA 1'
            GO TO 990
            END IF
C                                       Mark in CFILES
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK2
         FCNO(NCFILE) = CNO2
         FRW(NCFILE) = 0
C                                       uncal image
         CNO3 = 1
         TYPE = 'MA'
         CALL CATDIR ('SRCH', DISK3, CNO3, IMNAM3, IMCLA3, SEQ3, TYPE,
     *      NLUSER, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, IMNAM3, IMCLA3, SEQ3, DISK3,
     *         NLUSER
            GO TO 990
            END IF
         STATUS = 'READ'
         CALL CATIO ('READ', DISK3, CNO3, CATI3, STATUS, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, 'READ HEADER OF MA DATA 3'
            GO TO 990
            END IF
C                                       Mark in CFILES
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK3
         FCNO(NCFILE) = CNO3
         FRW(NCFILE) = 0
C                                       test matching
         IF (CATI2(KIDIM).NE.CATI3(KIDIM)) THEN
            MSGTXT = 'IMAGES DO NOT MATCH IN DIMENSIONS'
            GO TO 990
            END IF
         DO 40 I = 1,CATI2(KIDIM)
            J = I - 1
            IF (CATI2(KINAX+J).NE.CATI3(KINAX+J)) THEN
               WRITE (MSGTXT,1030) I, 'NUMBER PIXELS'
               CALL MSGWRT (7)
               FAIL = .TRUE.
               END IF
            EPS = 0.01 * ABS (CATR2(KRCIC+J))
            IF (ABS(CATD2(KDCRV+J)-CATD3(KDCRV+J)).GT.EPS) THEN
               WRITE (MSGTXT,1030) I, 'REF VALUE'
               CALL MSGWRT (7)
               FAIL = .TRUE.
               END IF
            IF (ABS(CATR2(KRCIC+J)-CATR3(KRCIC+J)).GT.EPS) THEN
               WRITE (MSGTXT,1030) I, 'INCREMENT'
               CALL MSGWRT (7)
               FAIL = .TRUE.
               END IF
            IF (ABS(CATR2(KRCRT+J)-CATR3(KRCRT+J)).GT.0.01) THEN
               WRITE (MSGTXT,1030) I, 'ROTATION'
               CALL MSGWRT (7)
               FAIL = .TRUE.
               END IF
            IF (ABS(CATR2(KRCRP+J)-CATR3(KRCRP+J)).GT.0.01) THEN
               WRITE (MSGTXT,1030) I, 'REF PIXEL'
               CALL MSGWRT (7)
               FAIL = .TRUE.
               END IF
 40         CONTINUE
         END IF
      IF (.NOT.FAIL) IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUFIXI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('QUFIXI ERROR',I3,1X,A)
 1030 FORMAT ('IMAGE MISMATCH AXIS',I2,' IN ',A)
      END
      SUBROUTINE QUFIXC (NVAL, PDFREQ, PDCOR, IRET)
C-----------------------------------------------------------------------
C   QUFIXC reads a CALIN file
C   Outputs:
C      NVAL     I      Number values in PDFREQ and PDCOR
C      PDFREQ   D(*)   Frequencies in GHz
C      PDCOR    R(*)   Correction in degrees
C      IRET     I      Error code: > 0 => QUIT
C-----------------------------------------------------------------------
      INTEGER   NVAL, IRET
      DOUBLE PRECISION PDFREQ(*)
      REAL      PDCOR(*)
C
      INCLUDE 'QUFIX.INC'
      INTEGER   LUN, FIND, NLINES, JTRIM, JLIM, I, KBP
      CHARACTER LINE*132
      DOUBLE PRECISION X
C-----------------------------------------------------------------------
      LUN = 3
      CALL ZTXOPN ('READ', LUN, FIND, CALIN, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING CALIN TEXT FILE'
         GO TO 990
         END IF
      NVAL = 0
      NLINES = 0
C                                       read loop
 20   CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) THEN
         WRITE (MSGTXT,1000) IRET, 'READING CALIN TEXT FILE'
         GO TO 990
C                                       got a line
      ELSE IF (IRET.EQ.0) THEN
         NLINES = NLINES + 1
         JLIM = JTRIM (LINE)
         IF (JLIM.LE.1) GO TO 20
         IF ((LINE(:1).EQ.';') .OR. (LINE(:1).EQ.'#')) GO TO 20
         KBP = 1
         CALL GETNUM (LINE, JLIM, KBP, X)
         IF (X.EQ.DBLANK) GO TO 20
         PDFREQ(NVAL+1) = X
         CALL GETNUM (LINE, JLIM, KBP, X)
         IF (X.EQ.DBLANK) GO TO 20
         PDCOR(NVAL+1) = X
         NVAL = NVAL + 1
         GO TO 20
         END IF
C                                       close
      CALL ZTXCLS (LUN, FIND, I)
      WRITE (MSGTXT,1020) NVAL, NLINES
      CALL MSGWRT (2)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUFIXC ERROR',I3,' ON ',A)
 1020 FORMAT ('QUFIXC: read',I6,' values in',I6,' lines of CALIN')
      END
      SUBROUTINE QUFIXD (NVAL, PDFREQ, PDCOR, IRET)
C-----------------------------------------------------------------------
C   QUFIXD reads two image files to determine the correction
C   Outputs:
C      NVAL     I      Number values in PDFREQ and PDCOR
C      PDFREQ   D(*)   Frequencies in GHz
C      PDCOR    R(*)   Correction in degrees
C      IRET     I      Error code: > 0 => QUIT
C-----------------------------------------------------------------------
      INTEGER   NVAL, IRET
      DOUBLE PRECISION PDFREQ(*)
      REAL      PDCOR(*)
C
      INCLUDE 'QUFIX.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   I, FAX, SAX, QP, UP, IROUND, IBLC(7), ITRC(7), LUN(4),
     *   NANG, IND(4), DEPTH(5), QDEPTH(5,4), I3, I4, I5, I6, I7, NX,
     *   NY, IX, IY, ISX, ISY, J, NF, NF1, NF2, WIN(4), NBY, BLKOF,
     *   BIND(4)
      REAL      TEMP, QU(4), BUFF(MABFSS,4), A1, A2, ANGS, ANGC, ANGSS,
     *   ANGCS
      LOGICAL   ISQU
      CHARACTER PHNAME*48
      DOUBLE PRECISION FREQ, REFP, INCF, FREF
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /47,48,49,50/
      EQUIVALENCE (DEPTH(1), I3), (DEPTH(2), I4), (DEPTH(3), I5),
     *   (DEPTH(4), I6), (DEPTH(5), I7)
C-----------------------------------------------------------------------
C                                       find freq axis
      CALL AXEFND (4, 'FREQ', CATI2(KIDIM), CATH2(KHCTP), FAX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FREQUENCY AXIS NOT FOUND'
         GO TO 990
         END IF
      IF (FAX.LT.2) THEN
         MSGTXT = 'FREQUENCY AXIS IS X OR Y, MUST BE AXIS 3 OR MORE'
         IRET = 10
         GO TO 990
         END IF
C                                       stokes axis
      ISQU = .FALSE.
      CALL AXEFND (6, 'STOKES', CATI2(KIDIM), CATH2(KHCTP), SAX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'STOKES AXIS NOT FOUND'
         IRET = 10
         GO TO 990
         END IF
      TEMP = (2.0D0 - CATD2(KDCRV+SAX)) / CATR2(KRCIC+SAX) +
     *   CATR2(KRCRP+SAX)
      QP = IROUND (TEMP)
      IF (ABS(QP-TEMP).LT.0.01) THEN
         TEMP = (3.0D0 - CATD2(KDCRV+SAX)) / CATR2(KRCIC+SAX) +
     *      CATR2(KRCRP+SAX)
         UP = IROUND (TEMP)
         IF (ABS(UP-TEMP).LT.0.01) THEN
            IF ((QP.GE.1) .AND. (QP.LE.CATI2(KINAX+SAX)) .AND.
     *         (UP.GE.1) .AND. (UP.LE.CATI2(KINAX+SAX))) ISQU=.TRUE.
            END IF
         END IF
      IF (.NOT.ISQU) THEN
         MSGTXT = 'Q AND U PLANES NOT FOUND IN IMAGES'
         IRET = 10
         GO TO 990
         END IF
C                                       open them
      CALL ZPHFIL ('MA', DISK2, CNO2, 1, PHNAME, IRET)
      CALL ZOPEN (LUN(1), IND(1), DISK2, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZOPEN (LUN(2), IND(2), DISK2, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZPHFIL ('MA', DISK3, CNO3, 1, PHNAME, IRET)
      CALL ZOPEN (LUN(3), IND(3), DISK3, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZOPEN (LUN(4), IND(4), DISK3, PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      NBY = 2 * MABFSS
      ISX = CATI2(KINAX)
      ISY = CATI2(KINAX+1)
      FREF = CATD2(KDCRV+FAX) / 1.D9
      REFP = CATR2(KRCRP+FAX)
      INCF = CATR2(KRCIC+FAX) / 1.D9
      NF1 = 1
      NF2 = CATI2(KINAX+FAX)
      CALL FILL (5, 1, IBLC(3))
      CALL COPY (5, CATI2(KINAX+2), ITRC(3))
      DO 10 I = 3,7
         ITRC(I) = MAX (1, ITRC(I))
 10      CONTINUE
C                                       loop over freq
      PCUT = PCUT * PCUT
      DO 190 NF = NF1,NF2
         ITRC(FAX+1) = NF
         IBLC(FAX+1) = NF
         ANGS = 0.0
         ANGC = 0.0
         ANGSS = 0.0
         ANGCS = 0.0
         NANG = 0
         IBLC(SAX+1) = QP
         ITRC(SAX+1) = QP
         DO 177 I7 = IBLC(7),ITRC(7)
            DO 176 I6 = IBLC(6),ITRC(6)
               DO 175 I5 = IBLC(5),ITRC(5)
                  DO 174 I4 = IBLC(4),ITRC(4)
                     DO 173 I3 = IBLC(3),ITRC(3)
                        CALL COPY (5, DEPTH, QDEPTH(1,1))
                        CALL COPY (5, DEPTH, QDEPTH(1,2))
                        CALL COPY (5, DEPTH, QDEPTH(1,3))
                        CALL COPY (5, DEPTH, QDEPTH(1,4))
                        QDEPTH(SAX-1,2) = UP
                        QDEPTH(SAX-1,4) = UP
                        DO 160 I = 1,NBOX
                           IF (CLBOX(1,I).LT.0) THEN
                              WIN(1) = CLBOX(3,I) - CLBOX(2,I) + 1
                              WIN(2) = CLBOX(4,I) - CLBOX(2,I) + 1
                              WIN(3) = CLBOX(3,I) + CLBOX(2,I) - 1
                              WIN(4) = CLBOX(4,I) + CLBOX(2,I) - 1
                           ELSE
                              CALL COPY (4, CLBOX(1,I), WIN)
                              END IF
C                                       init
                           DO 110 J = 1,4
                              CALL COMOFF (CATI2(KIDIM), CATI2(KINAX),
     *                           QDEPTH(1,J), BLKOF, IRET)
                              IF (IRET.NE.0) GO TO 999
                              BLKOF = BLKOF + 1
                              CALL MINIT ('READ', LUN(J), IND(J), ISX,
     *                           ISY, WIN, BUFF(1,J), NBY, BLKOF, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'INIT READ OF IMAGE', J
                                 GO TO 990
                                 END IF
 110                          CONTINUE
C                                       read plane
                           NY = WIN(4) - WIN(2) + 1
                           NX = WIN(3) - WIN(1) + 1
                           DO 140 IY = 1,NY
                              DO 120 J = 1,4
                                 CALL MDISK ('READ', LUN(J), IND(J),
     *                              BUFF(1,J), BIND(J), IRET)
                                 IF (IRET.NE.0) THEN
                                    WRITE (MSGTXT,1000) IRET,
     *                                 'READ IMAGE ROW', J
                                    GO TO 990
                                    END IF
 120                             CONTINUE
                              DO 130 IX = 1,NX
                                 IF (CLBOX(1,I).LT.0) THEN
                                    TEMP = (IX-CLBOX(3,I))**2
                                    TEMP = (IY-CLBOX(4,I))**2 + TEMP
                                    TEMP = SQRT (TEMP)
                                    IF (TEMP.GT.CLBOX(2,I)) GO TO 130
                                    END IF
                                 DO 125 J = 1,4
                                    QU(J) = BUFF(BIND(J)+IX-1,J)
                                    IF (QU(J).EQ.FBLANK) GO TO 130
 125                                CONTINUE
                                 IF (QU(1)*QU(1)+QU(2)*QU(2).LT.PCUT)
     *                              GO TO 130
                                 IF (QU(3)*QU(3)+QU(4)*QU(4).LT.PCUT)
     *                              GO TO 130
                                 A1 = ATAN2 (QU(2), QU(1))
                                 A2 = ATAN2 (QU(4), QU(3))
                                 ANGS = ANGS + SIN (A1 - A2)
                                 ANGC = ANGC + COS (A1 - A2)
                                 ANGSS = ANGSS + (SIN (A1 - A2))**2
                                 ANGCS = ANGCS + (COS (A1 - A2))**2
                                 NANG = NANG + 1
 130                             CONTINUE
 140                          CONTINUE
 160                       CONTINUE
 173                    CONTINUE
 174                 CONTINUE
 175              CONTINUE
 176           CONTINUE
 177        CONTINUE
         FREQ = FREF + (NF - REFP) * INCF
         IF (NANG.GT.0) THEN
            ANGS = ANGS / NANG
            ANGC = ANGC / NANG
            ANGSS = MAX (0.0, ANGSS / NANG - ANGS * ANGS)
            ANGCS = MAX (0.0, ANGCS / NANG - ANGC * ANGC)
            A1 = RAD2DG * ATAN2 (ANGS, ANGC)
            A2 = RAD2DG * SQRT (ANGS*ANGS*ANGCS + ANGC*ANGC*ANGSS) /
     *         (ANGS * ANGS + ANGC * ANGC)
            WRITE (MSGTXT,1177) NF, FREQ, A1, A2
            CALL MSGWRT (3)
            NVAL = NVAL + 1
            PDFREQ(NVAL) = FREQ
            PDCOR(NVAL) = A1
         ELSE
            A1 = 0.0
            WRITE (MSGTXT,1178) NF, FREQ
            CALL MSGWRT (6)
            END IF
 190     CONTINUE
      DO 195 J = 1,4
         CALL ZCLOSE (LUN(J), IND(J), I)
 195     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUFIXD ERROR',I3,' ON ',A,I2)
 1177 FORMAT ('Freq #',I5,F10.5,' GHz  corr',F8.3,' +-',F8.3,' degrees')
 1178 FORMAT ('Freq #',I5,F10.5,' GHz  NO DATA FOUND')
      END
      SUBROUTINE QUFIXO (NVAL, PDFREQ, PDCOR, IRET)
C-----------------------------------------------------------------------
C   QUFIXO writes an OUTTEXT correction file
C   Inputs:
C      NVAL     I      Number values in PDFREQ and PDCOR
C      PDFREQ   D(*)   Frequencies in GHz
C      PDCOR    R(*)   Correction in degrees
C   Outputs:
C      IRET     I      Error code: > 0 => QUIT
C-----------------------------------------------------------------------
      INTEGER   NVAL, IRET
      DOUBLE PRECISION PDFREQ(*)
      REAL      PDCOR(*)
C
      INCLUDE 'QUFIX.INC'
      INTEGER   LUN, FIND, JTRIM, L, I, TIME(3), DATE(3)
      CHARACTER LINE*132, CTIME*8, CDATE*12
C-----------------------------------------------------------------------
      LUN = 3
      CALL ZTXOPN ('WRIT', LUN, FIND, OUTEXT, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTTEXT TEXT FILE'
         GO TO 990
         END IF
C                                       label
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (LINE,1005) CDATE, CTIME
      L = JTRIM (LINE)
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(:L), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING OUTTEXT FILE'
         GO TO 990
         END IF

C                                       write loop
      DO 20 I = 1,NVAL
         WRITE (LINE,1010) PDFREQ(I), PDCOR(I)
         L = JTRIM (LINE)
         CALL ZTXIO ('WRITE', LUN, FIND, LINE(:L), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTTEXT FILE'
            GO TO 990
            END IF
 20      CONTINUE
C                                       close
      CALL ZTXCLS (LUN, FIND, I)
      WRITE (MSGTXT,1020) NVAL
      CALL MSGWRT (2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUFIXO ERROR',I3,' ON ',A)
 1005 FORMAT ('# QUFIX run ',A,A)
 1010 FORMAT (F13.9,F12.5)
 1020 FORMAT ('QUFIXO: wrote',I6,' values to OUTTEXT')
      END
      SUBROUTINE QUFIXT (NVAL, PDFREQ, PDCOR, NCH, NIF)
C-----------------------------------------------------------------------
C   QUFIXT translates the list of corrections to match the UV data
C   Inputs:
C      NVAL     I      Number values in PDFREQ and PDCOR
C      PDFREQ   D(*)   Frequencies in GHz
C   In/Out:
C      PDCOR    R(*)   Correction in degrees: NVAL in, (Nchan,Nif) out
C   Outputs:
C      NCH      I      Number spectral channels
C      NIF      I      Number IFs
C-----------------------------------------------------------------------
      INTEGER   NVAL, NCH, NIF
      DOUBLE PRECISION PDFREQ(*)
      REAL      PDCOR(*)
C
      INCLUDE 'QUFIX.INC'
      INTEGER   I, IIF, ICH, J, K
      DOUBLE PRECISION FF, F0, RP, CI, FI
      REAL      PDTEMP(MAXCIF), TEMP
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (JLOCIF.GE.0) THEN
         NIF = CATI1(KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
      CALL RCOPY (NVAL, PDCOR, PDTEMP)
      NCH = CATI1(KINAX+JLOCF)
      F0 = CATD1(KDCRV+JLOCF) / 1.0D9
      RP = CATR1(KRCRP+JLOCF)
      I = 0
      K = 1
      DO 50 IIF = 1,NIF
         CI = FINC(IIF) / 1.0D9
         FI = F0 + FOFF(IIF) / 1.D9
         DO 40 ICH = 1,NCH
            FF = FI + (ICH-RP) * CI
            I = I + 1
            IF (FF.LE.PDFREQ(1)) THEN
               PDCOR(I) = PDTEMP(1)
            ELSE IF (FF.GE.PDFREQ(NVAL)) THEN
               PDCOR(I) = PDTEMP(NVAL)
            ELSE
 20            DO 30 J = K,NVAL
                  IF ((FF.GE.PDFREQ(J)) .AND. (FF.LE.PDFREQ(J+1))) THEN
                     K = J
                     TEMP = PDFREQ(J+1) - PDFREQ(J)
                     IF (TEMP.NE.0.0) TEMP = (FF - PDFREQ(J)) / TEMP
                     PDCOR(I) = PDTEMP(J) + TEMP *
     *                  (PDTEMP(J+1) - PDTEMP(J))
                     GO TO 40
                     END IF
 30               CONTINUE
C                                       prevent infinite loop
C                                       should not be needed
               IF (K.EQ.1) THEN
                  PDCOR(I) = PDTEMP(1)
C                                       check full list
               ELSE
                  K = 1
                  GO TO 20
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE QUFIXA (NCH, NIF, PDCOR, IRET)
C-----------------------------------------------------------------------
C   QUFIXA applies the corrections to BP, CP, PD tables
C   Inputs:
C      PDCOR    R(*)   Correction in degrees: (Nchan,Nif)
C   Outputs:
C      IRET     I      Error code: > 0 => QUIT
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, IRET
      REAL      PDCOR(NCH,NIF)
C
      INCLUDE 'QUFIX.INC'
      INTEGER   NUMTA, LUN1, LUN2, TABUF1(512), TABUF2(512), IBPRNO,
     *   BPKOLS(MAXBPC), BPNUMV(MAXBPC), NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   LCHAN, NUMSHF, NREC, IREC, SOURID, SUBA, ANT, FREQID, LI, LC,
     *   REFANT(2), CPNUMV(6), CPKOLS(6), ICPRNO, PDKOLS(9), PDNUMV(9),
     *   IPDRNO, I, JNX, VER, PDKOLO(9), PDNUMO(9), BPVER, SUBARR,
     *   FRQSEL
      DOUBLE PRECISION TIME, CHSHFT(MAXIF)
      REAL      CF, SF, LOWSHF, DELSHF, INTERV, BANDW, WEIGHT(2*MAXIF),
     *   BNDPAS(2,MAXCIF), XT, YT, VFLUX(4,MAXCIF), PHDIFF(MAXCIF)
      CHARACTER LBPTYP*8, SOURSE*16, POLTYP*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (BNDPAS, VFLUX)
      DATA LUN1, LUN2 /79, 78/
C-----------------------------------------------------------------------
      CALL FNDEXT ('BP', CATI1, BPVER)
      VER = BPVER + 1
      SUBARR = -1
      FRQSEL = -1
C                                       modify existing BP table
      IF (BPVER.GT.0) THEN
         MSGTXT = 'Updating existing BP table, writing new one'
         CALL MSGWRT (5)
         CALL BPINI ('READ', TABUF2, DISK1, CNO1, BPVER, CATI1, LUN2,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         CALL BPINI ('WRIT', TABUF1, DISK1, CNO1, VER, CATI1, LUN1,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         NREC = TABUF2(5)
         DO 50 IREC = 1,NREC
            IBPRNO = IREC
            CALL TABBP ('READ', TABUF2, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ OLD BP TABLE'
               GO TO 990
               END IF
            IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR. (SUBA.EQ.SUBARR))
     *         .AND. ((FREQID.LE.0) .OR. (FRQSEL.LE.0) .OR.
     *         (FREQID.EQ.FRQSEL)) .AND. (IRET.GT.-2)) THEN
               DO 30 LI = 1,NIF
                  JNX = (LI - 1) * NUMFRQ + 1 + NUMFRQ*NUMIF
                  DO 20 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.FBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF - YT*SF
                        BNDPAS(2,JNX) = XT*SF + YT*CF
                        END IF
                     JNX = JNX + 1
 20                  CONTINUE
 30               CONTINUE
               END IF
            IBPRNO = IREC
            CALL TABBP ('WRIT', TABUF1, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW BP TABLE'
               GO TO 990
               END IF
 50         CONTINUE
         CALL TABIO ('CLOS', 0, IBPRNO, TABUF2, TABUF2, LI)
C                                       create one from scratch
      ELSE
         MSGTXT = 'Writing new BP table from scratch'
         CALL MSGWRT (5)
         CALL GETANT (DISK1, CNO1, SUBARR, CATI1, TABUF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ AN FILE WITH GETANT'
            GO TO 990
            END IF
         NUMANT = NSTNS
         NUMPOL = 2
         NUMIF = 1
         IF (JLOCIF.GE.0) NUMIF = CATI1(KINAX+JLOCIF)
         NUMFRQ = CATI1(KINAX+JLOCF)
         LCHAN = 1
         NUMSHF = 3
         LOWSHF = 0.0
         DELSHF = 0.0
         LBPTYP = ' '
         CALL BPINI ('WRIT', TABUF1, DISK1, CNO1, VER, CATI1, LUN1,
     *      IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *      LCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OLD BP TABLE'
            GO TO 990
            END IF
         TIME = 0.0D0
         INTERV = 0.0
         SOURID = 0
         SUBA = SUBARR
         FREQID = FRQSEL
         REFANT(1) = 1
         REFANT(2) = 1
         LI = NUMIF * NUMPOL * NUMFRQ
         CALL RFILL (2*MAXIF, 1.0, WEIGHT)
         DO 110 I = 1,LI
            BNDPAS(1,I) = 1.0
            BNDPAS(2,I) = 0.0
 110        CONTINUE
         DO 130 LI = 1,NIF
            JNX = (LI - 1) * NUMFRQ + 1 + NUMFRQ*NUMIF
            DO 120 LC = 1,NCH
               IF (PDCOR(LC,LI).NE.FBLANK) THEN
                  BNDPAS(1,JNX) = COS (PDCOR(LC,LI)*DG2RAD)
                  BNDPAS(2,JNX) = SIN (PDCOR(LC,LI)*DG2RAD)
               ELSE
                  BNDPAS(1,JNX) = FBLANK
                  BNDPAS(2,JNX) = FBLANK
                  END IF
               JNX = JNX + 1
 120           CONTINUE
 130        CONTINUE
         DO 150 ANT = 1,NSTNS
            IBPRNO = ANT
            CALL TABBP ('WRIT', TABUF1, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *         NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         CHSHFT, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW BP TABLE'
               GO TO 990
               END IF
 150        CONTINUE
         END IF
      CALL TABIO ('CLOS', 0, IBPRNO, TABUF1, TABUF1, LI)
C                                       CP file if any
      CALL FNDEXT ('CP', CATI1, NUMTA)
      VER = NUMTA + 1
 200  VER = VER - 1
      IF (VER.GT.0) THEN
         CALL CPINI ('READ', TABUF2, DISK1, CNO1, VER, CATI1, LUN2,
     *      ICPRNO, CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IRET)
         IF (((FREQID.GT.0) .AND.(FRQSEL.GT.0) .AND. (FRQSEL.NE.FREQID))
     *      .OR. (IRET.NE.0)) THEN
            CALL TABIO ('CLOS', 0, ICPRNO, TABUF2, TABUF2, LI)
            GO TO 200
            END IF
         MSGTXT = 'Updating existing CP table, writing new one'
         CALL MSGWRT (5)
         VER = NUMTA + 1
         CALL CPINI ('WRIT', TABUF1, DISK1, CNO1, VER, CATI1, LUN1,
     *      ICPRNO, CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN NEW CP TABLE'
            GO TO 990
            END IF
         NREC = TABUF2(5)
         DO 250 IREC = 1,NREC
            ICPRNO = IREC
            CALL TABCP ('READ', TABUF2, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *         NUMFRQ, SOURSE, SOURID, VFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ OLD CP TABLE'
               GO TO 990
               END IF
            DO 230 LI = 1,NIF
               JNX = (LI - 1) * NUMFRQ + 1
               DO 220 LC = 1,NCH
                  IF (PDCOR(LC,LI).NE.FBLANK) THEN
                     CF = COS (PDCOR(LC,LI)*DG2RAD)
                     SF = SIN (PDCOR(LC,LI)*DG2RAD)
                     XT = VFLUX(2,JNX)
                     YT = VFLUX(3,JNX)
                     VFLUX(2,JNX) = XT*CF - YT*SF
                     VFLUX(3,JNX) = XT*SF + YT*CF
                     END IF
                  JNX = JNX + 1
 220              CONTINUE
 230           CONTINUE
            ICPRNO = IREC
            CALL TABCP ('WRIT', TABUF1, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *         NUMFRQ, SOURSE, SOURID, VFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW CP TABLE'
               GO TO 990
               END IF
 250        CONTINUE
         CALL TABIO ('CLOS', 0, ICPRNO, TABUF2, TABUF2, LI)
         CALL TABIO ('CLOS', 0, ICPRNO, TABUF1, TABUF1, LI)
         END IF
C                                       PD update
      CALL FNDEXT ('PD', CATI1, NUMTA)
      IF (NUMTA.LE.0) THEN
         MSGTXT = 'NO PD TABLE - HOW CAN THIS BE?'
         GO TO 990
         END IF
      MSGTXT = 'Updating existing PD table, writing new one'
      CALL MSGWRT (5)
      VER = NUMTA
      CALL PDINI ('READ', TABUF2, DISK1, CNO1, VER, CATI1, LUN2,
     *   IPDRNO, PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OLD PD TABLE FOR READ'
         GO TO 990
         END IF
      VER = NUMTA + 1
      CALL PDINI ('WRIT', TABUF1, DISK1, CNO1, VER, CATI1, LUN1,
     *   IPDRNO, PDKOLO, PDNUMO, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPEN OLD PD TABLE FOR WRITE'
         GO TO 990
         END IF
      NREC = TABUF2(5)
      DO 360 IREC = 1,NREC
         IPDRNO = IREC
         CALL TABPD ('READ', TABUF2, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BNDPAS,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ PD TABLE'
            GO TO 990
            END IF
         IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR. (SUBA.EQ.SUBARR))
     *      .AND. (IRET.GT.-3)) THEN
C                                       Just add to phase difference
            IF (POLTYP.EQ.'ORI-ELP') THEN
               DO 320 LI = 1,NIF
                  JNX = (LI - 1) * NUMFRQ + 1
                  DO 310 LC = 1,NCH
                     IF (PDCOR(LC,LI).NE.FBLANK) PHDIFF(JNX) =
     *                  PHDIFF(JNX) + PDCOR(LC,LI) * DG2RAD
                     JNX = JNX + 1
 310                 CONTINUE
 320              CONTINUE
C                                       correct D terms
            ELSE
               DO 350 LI = 1,NIF
                  JNX = (LI - 1) * NUMFRQ + 1
                  DO 330 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.FBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF - YT*SF
                        BNDPAS(2,JNX) = XT*SF + YT*CF
                        END IF
                     JNX = JNX + 1
 330                 CONTINUE
C                                       2nd polarization opposite
                  JNX = (LI - 1) * NUMFRQ + 1 +
     *               NUMFRQ * NUMIF
                  DO 340 LC = 1,NCH
                     XT = BNDPAS(1,JNX)
                     YT = BNDPAS(2,JNX)
                     IF ((XT.NE.FBLANK) .AND. (YT.NE.FBLANK) .AND.
     *                  (PDCOR(LC,LI).NE.FBLANK)) THEN
                        CF = COS (PDCOR(LC,LI)*DG2RAD)
                        SF = SIN (PDCOR(LC,LI)*DG2RAD)
                        BNDPAS(1,JNX) = XT*CF + YT*SF
                        BNDPAS(2,JNX) = YT*CF - XT*SF
                        END IF
                     JNX = JNX + 1
 340                 CONTINUE
 350              CONTINUE
               END IF
            END IF
         IPDRNO = IREC
         CALL TABPD ('WRIT', TABUF1, IPDRNO, PDKOLO, PDNUMO, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, BNDPAS,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-WRITE PD TABLE'
            GO TO 990
            END IF
 360     CONTINUE
      CALL TABIO ('CLOS', 0, IPDRNO, TABUF1, TABUF1, LI)
      CALL TABIO ('CLOS', 0, IPDRNO, TABUF2, TABUF2, LI)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPAPPL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE QUFIXH
C-----------------------------------------------------------------------
C   writes HI records when solution is appled to data
C-----------------------------------------------------------------------
      INCLUDE 'QUFIX.INC'
      INTEGER   LUN, LUNTMP, TIME(3), DATE(3), IERR, BUFF2(256), I, J,
     *   JTRIM
      CHARACTER CTIME(2)*12, HILINE*72
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      LUN = LUNTMP (1)
C                                       Write History.
      CALL HIINIT (3)
C                                       Open old history
      CALL HIOPEN (LUN, DISK1, CNO1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       read text file
      IF (CALIN.NE.' ') THEN
         J = JTRIM (CALIN)
         WRITE (HILINE,1010) TSKNAM, CALIN(:J)
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1011) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       found from images
      ELSE
         CALL HENCO2 (TSKNAM, IMNAM2, IMCLA2, SEQ2, DISK2, LUN, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 200
         CALL HENCO3 (TSKNAM, IMNAM3, IMCLA3, SEQ3, DISK3, LUN, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 200
         PCUT = SQRT (PCUT)
         WRITE (HILINE,1020) TSKNAM, PCUT
         CALL HIADD (LUN, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 20 J = 1,NBOX
            WRITE (HILINE,1025) TSKNAM, J, (CLBOX(I,J), I = 1,4)
            CALL HIADD (LUN, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 20         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN, .TRUE., BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1010 FORMAT (A6,'CALIN =''',A,'''')
 1011 FORMAT (A6,25X,'/ Text file with corrections')
 1020 FORMAT (A6,'PCUT ',F8.4,'   / only SQRT(q*q+u*u) > PCUT used')
 1025 FORMAT (A6,'CLBOX(*,',I2.2,') =',3(I6,','),I6,'   / window(s)')
      END
