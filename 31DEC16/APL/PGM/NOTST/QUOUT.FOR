LOCAL INCLUDE 'QUOUT.INC'
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, LPNAME*48, INTYPE*2
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOURC(4,30), XLPNAM(12)
      REAL      XSIN, XDISIN, FORMAT, XINVER, BLC(7), TRC(7),
     *   FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF)
      INTEGER   DISKIN, SEQIN, CNOIN, BUFFER(512), LUNP, FINDP, INVERS,
     *   NLINES
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, FORMAT, XLPNAM,
     *   XSOURC, XINVER, BLC, TRC
      COMMON /CHPARM/ NAMEIN, CLAIN, XSOUR, LPNAME, INTYPE
      COMMON /INFOLS/ FOFF, FINC, BUFFER, DISKIN, SEQIN, CNOIN, LUNP,
     *   FINDP, INVERS, NLINES
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
LOCAL END
      PROGRAM QUOUT
C-----------------------------------------------------------------------
C! QUOUT writes a text file of a source's Q and U polarization
C# Calibration UV VLA VLB polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012
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
C   Task QUOUT writes a text file of Q/U versus freq
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be listed.
C      INCLASS        CLASS         File class to be listed.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      OUTTEXT        LPNAME
C      SOURCES        XSOUR(4,30)   Sources selected
C      INVERS         INVERS        CP table version: 0 => SU
C      BLC            BLC           BLC of image
C      TRC            TRC           TRC
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6
      INTEGER   IRET, I
      INCLUDE 'QUOUT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'QUOUT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL QUOIN (PRGM, IRET)
      IF (IRET.EQ.0) THEN
         IF (INTYPE.EQ.'UV') THEN
            CALL QUOUV (IRET)
         ELSE IF (INTYPE.EQ.'MA') THEN
            CALL QUOIM (IRET)
            END IF
         CALL ZTXCLS (LUNP, FINDP, I)
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE QUOIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   QUOIN gets input parameters for QUOUT
C   Inputs:
C      PRGN    C*6       Program name (2 chars/word)
C   Output:
C      IRET    I         Error code: 0 => ok
C                           5 => catalog troubles
C                           8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in QUOUT for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, STATUS*4
      INTEGER   IROUND, NPARM, IERR, I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'QUOUT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      LUNP = 11
      NLINES = 0
C                                       Get input parameters.
      NPARM = 155
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XSOURC(1,I), XSOUR(I))
 25      CONTINUE
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Get CATBLK.
      CNOIN = 1
      INTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, INTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      STATUS = 'REST'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Mark in CFILES if catalog
C                                       marked "READ"
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Open output device
      CALL ZTXOPN ('WRIT', LUNP, FINDP, LPNAME, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1080) IRET
         IRET = 1
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUOIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1080 FORMAT ('QUOIN: ERROR ',I3,' OPENING OUTPUT TEXT FILE')
      END
      SUBROUTINE QUOUV (IRET)
C-----------------------------------------------------------------------
C   QUOUV writes a text file with Q, U, and FREQs from either the CP or
C   SU table.
C   Output:
C      IRET   I   > 0 => problem
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'QUOUT.INC'
      INTEGER   IROUND, I, NSOURC, LUN, IIVER, NIF, FRQSEL, NUMIF, NREC,
     *   ISBAND(MAXIF), FREQID, JLOCF, ISURNO, TKOLS(MAXSUC), IREC, J,
     *   TNUMV(MAXSUC), ISNUM, NCHAN, K, NUMFRQ, TIME(3), DATE(3), L,
     *   JTRIM
      REAL      VFLUX(4,MAXCIF), ANG
      DOUBLE PRECISION FF, REFP, INCF, FREF, F0
      LOGICAL   WANT, DOANGL
      CHARACTER LINE*80, CTIME*8, CDATE*12, BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      DOANGL = FORMAT.GT.1.50
C                                       CP or SU
      INVERS = IROUND (XINVER)
      CALL FNDEXT ('CP', CATBLK, I)
C                                       SU table
      IF ((XINVER.LT.0.0) .OR. (I.LE.0)) THEN
         INVERS = -1
         CALL FNDEXT ('SU', CATBLK, I)
         IF (I.LE.0) THEN
            MSGTXT = 'REQUESTED SOURCE TABLE NOT FOUND'
            IRET = 10
            GO TO 990
            END IF
C                                       CP table
      ELSE
         IF ((INVERS.LE.0) .OR. (INVERS.GT.I)) INVERS = I
         END IF
C                                       number of sources specified
      NSOURC = 0
      DO 10 I = 1,30
         IF (XSOUR(I).NE.' ') NSOURC = NSOURC + 1
 10      CONTINUE
C                                       Get basic FREQ parameters
C                                       Get IF freq offset.
      LUN = 28
      IIVER = 1
      FREQID = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE'
         GO TO 990
         END IF
C                                       freq axis
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), JLOCF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'PROBLEM FINDING FREQUENCY AXIS'
         GO TO 990
         END IF
      FREF = CATD(KDCRV+JLOCF)
C                                       Init output
      IF (DOANGL) THEN
         LINE = ';FAS'
      ELSE
         LINE = ';FQUS'
         END IF
      L = JTRIM (LINE)
      CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
         GO TO 990
         END IF
C                                       SU table
      ISNUM = 0
      IF (INVERS.LE.0) THEN
         CALL SOUINI ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *      NUMIF, VELTYP, VELDEF, FREQID, ISURNO, TKOLS, TNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING SU TABLE'
            GO TO 990
            END IF
         WRITE (LINE,1010) CDATE, CTIME, 'SU', IIVER
         L = JTRIM (LINE)
         CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
            GO TO 990
            END IF
         NREC = BUFFER(5)
         DO 40 IREC = 1,NREC
            ISURNO = IREC
            CALL TABSOU ('READ', BUFFER, ISURNO, TKOLS, TNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING SU TABLE'
               GO TO 990
               END IF
            WANT = NSOURC.LE.0
            IF (NSOURC.GT.0) THEN
               DO 20 I = 1,30
                  IF (SNAME.EQ.XSOUR(I)) WANT = .TRUE.
 20               CONTINUE
               IF ((WANT) .AND. (NSOURC.GT.1)) ISNUM = IDSOUR
            ELSE IF (NREC.GT.1) THEN
               ISNUM = IDSOUR
               END IF
            IF (WANT) THEN
               DO 30 I = 1,NUMIF
                  FF = (FREF + FOFF(I)) / 1.0D9
                  IF ((FLUX(2,I).NE.0.0) .OR. (FLUX(3,I).NE.0.0)) THEN
                     NLINES = NLINES + 1
                     IF (DOANGL) THEN
                        ANG = RAD2DG * ATAN2 (FLUX(3,I), FLUX(2,I))
     *                     / 2.0D0
                        WRITE (LINE,1020) FF, ANG, ISNUM
                     ELSE
                        WRITE (LINE,1021) FF, FLUX(2,I), FLUX(3,I),
     *                     ISNUM
                        END IF
                     L = JTRIM (LINE)
                     CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
                        GO TO 990
                        END IF
                     END IF
 30               CONTINUE
               END IF
 40         CONTINUE
         CALL TABSOU ('CLOS', BUFFER, ISURNO, TKOLS, TNUMV, IDSOUR,
     *      SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, I)
C                                       CP table
      ELSE
         REFP = CATR(KRCRP+JLOCF)
         NCHAN = CATBLK(KINAX+JLOCF)
         CALL CPINI ('READ', BUFFER, DISKIN, CNOIN, INVERS, CATBLK, LUN,
     *      ISURNO, TKOLS, TNUMV, NUMIF, NUMFRQ, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING CP TABLE'
            GO TO 990
            END IF
         WRITE (LINE,1010) CDATE, CTIME, 'CP', INVERS
         L = JTRIM (LINE)
         CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
            GO TO 990
            END IF
         NREC = BUFFER(5)
         DO 150 IREC = 1,NREC
            ISURNO = NREC
            CALL TABCP ('READ', BUFFER, ISURNO, TKOLS, TNUMV, NUMIF,
     *         NUMFRQ, SNAME, IDSOUR, VFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING CP TABLE'
               GO TO 990
               END IF
            WANT = NSOURC.LE.0
            IF (NSOURC.GT.0) THEN
               DO 120 I = 1,30
                  IF (SNAME.EQ.XSOUR(I)) WANT = .TRUE.
 120              CONTINUE
               IF ((WANT) .AND. (NSOURC.GT.1)) ISNUM = IDSOUR
            ELSE IF (NREC.GT.1) THEN
               ISNUM = IDSOUR
               END IF
            IF (WANT) THEN
               K = 0
               DO 140 I = 1,NUMIF
                  INCF = FINC(I) / 1.D9
                  F0 = (FREF + FOFF(I)) / 1.D9
                  DO 130 J = 1,NUMFRQ
                     K = K + 1
                     FF = F0 + (J - REFP) * INCF
                     IF ((VFLUX(2,K).NE.0.0) .OR. (VFLUX(3,K).NE.0.0))
     *                  THEN
                        NLINES = NLINES + 1
                        IF (DOANGL) THEN
                           ANG = RAD2DG * ATAN2 (VFLUX(3,K), VFLUX(2,K))
     *                        / 2.0D0
                           WRITE (LINE,1020) FF, ANG, ISNUM
                        ELSE
                           WRITE (LINE,1021) FF, VFLUX(2,K), VFLUX(3,K),
     *                        ISNUM
                           END IF
                        L = JTRIM (LINE)
                        CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING OUTPUT FILE'
                           GO TO 990
                           END IF
                        END IF
 130                 CONTINUE
 140              CONTINUE
               END IF
 150        CONTINUE
         CALL TABCP ('CLOS', BUFFER, ISURNO, TKOLS, TNUMV, NUMIF,
     *      NUMFRQ, SNAME, IDSOUR, VFLUX, I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUOUV ERROR',I4,' ON ',A)
 1010 FORMAT ('# QUOUT run ',A,A,' using ',A,' version',I5)
 1020 FORMAT (F13.9,F12.5,I7)
 1021 FORMAT (F13.9,2F12.6,I7)
      END
      SUBROUTINE QUOIM (IRET)
C-----------------------------------------------------------------------
C   QUOIM will determine the nature of the data cube presented to it
C   and read either Q/U or pol. angle.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   FAX, SAX, QP, UP, IROUND, DATE(3), TIME(3), IBLC(7),
     *   ITRC(7), NQ, NU, DEPTH(5), WIN(4), NBY, BLKOF, L, JTRIM, I,
     *   NX, NY, LUN, IND, NF, NF1, NF2, IX, IY, BIND, I7, I6, I5,
     *   I4, I3, ISX, ISY
      REAL      TEMP, QS, US, LBLC(7), LTRC(7), IMBUFF(MABFSS), ANG
      LOGICAL   ISQU, DOANGL
      DOUBLE PRECISION FREQ, REFP, INCF, FREF
      CHARACTER CTEMP*8, CTIME*8, CDATE*12, LINE*80, PHNAME*48
      INCLUDE 'QUOUT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (DEPTH(1), I3), (DEPTH(2), I4), (DEPTH(3), I5),
     *   (DEPTH(4), I6), (DEPTH(5), I7)
C-----------------------------------------------------------------------
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      DOANGL = FORMAT.GT.1.50
C                                       find freq axis
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FAX, IRET)
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
      CALL AXEFND (6, 'STOKES', CATBLK(KIDIM), CATH(KHCTP), SAX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'STOKES AXIS NOT FOUND'
         IRET = 10
         GO TO 990
         END IF
      TEMP = (2.0D0 - CATD(KDCRV+SAX)) / CATR(KRCIC+SAX) +
     *   CATR(KRCRP+SAX)
      QP = IROUND (TEMP)
      IF (ABS(QP-TEMP).LT.0.01) THEN
         TEMP = (3.0D0 - CATD(KDCRV+SAX)) / CATR(KRCIC+SAX) +
     *      CATR(KRCRP+SAX)
         UP = IROUND (TEMP)
         IF (ABS(UP-TEMP).LT.0.01) THEN
            IF ((QP.GE.1) .AND. (QP.LE.CATBLK(KINAX+SAX)) .AND.
     *         (UP.GE.1) .AND. (UP.LE.CATBLK(KINAX+SAX))) ISQU=.TRUE.
            END IF
         END IF
C                                       start output
      IF (DOANGL) THEN
         LINE = ';FAS'
      ELSE
         LINE = ';FQUS'
         END IF
      L = JTRIM (LINE)
      CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
         GO TO 990
         END IF
C                                       open image file
      CALL ZPHFIL ('MA', DISKIN, CNOIN, 1, PHNAME, IRET)
      LUN = 27
      CALL ZOPEN (LUN, IND, DISKIN, PHNAME, .TRUE., .FALSE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      NBY = 2 * MABFSS
      ISX = CATBLK(KINAX)
      ISY = CATBLK(KINAX+1)
      FREF = CATD(KDCRV+FAX) / 1.D9
      REFP = CATR(KRCRP+FAX)
      INCF = CATR(KRCIC+FAX) / 1.D9
C                                       Polarization angle image
      IF (.NOT.ISQU) THEN
         CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
         IF (CTEMP.NE.'DEGREES') THEN
            MSGTXT = 'INPUT CUBE DOES NOT CONTAIN Q AND U CUBES'
            CALL MSGWRT (8)
            MSGTXT = 'BUT UNITS ARE NOT DEGREES AS EXPECTED FOR POLA'
            IRET = 10
            GO TO 990
            END IF
         TEMP = (7.0D0 - CATD(KDCRV+SAX)) / CATR(KRCIC+SAX) +
     *      CATR(KRCRP+SAX)
         QP = IROUND (TEMP)
         IF ((ABS(QP-TEMP).GT.0.01) .OR. (QP.LT.1) .OR.
     *      (QP.GT.CATBLK(KINAX+SAX))) THEN
            MSGTXT = 'STOKES AXIS VALUE 7.0 = POLA NOT FOUND'
            IRET = 10
            GO TO 990
            END IF
         WRITE (LINE,1010) CDATE, CTIME, 'Pol angle image'
         L = JTRIM (LINE)
         CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
            GO TO 990
            END IF
         CALL RCOPY (7, BLC, LBLC)
         CALL RCOPY (7, TRC, LTRC)
         LBLC(1+SAX) = QP
         LTRC(1+SAX) = QP
         CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), LBLC, LTRC, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 110 I = 1,7
            IBLC(I) = IROUND (LBLC(I))
            ITRC(I) = IROUND (LTRC(I))
 110        CONTINUE
         NX = ITRC(1) - IBLC(1) + 1
         NY = ITRC(2) - IBLC(2) + 1
         WIN(1) = IBLC(1)
         WIN(2) = IBLC(2)
         WIN(3) = ITRC(1)
         WIN(4) = ITRC(2)
         NF1 = IBLC(FAX+1)
         NF2 = ITRC(FAX+1)
         DO 190 NF = NF1,NF2
            QS = 0.0
            US = 0.0
            NQ = 0
            ITRC(FAX+1) = NF
            IBLC(FAX+1) = NF
            DO 150 I7 = IBLC(7),ITRC(7)
               DO 149 I6 = IBLC(6),ITRC(6)
                  DO 148 I5 = IBLC(5),ITRC(5)
                     DO 147 I4 = IBLC(4),ITRC(4)
                        DO 146 I3 = IBLC(3),ITRC(3)
                           CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                        DEPTH, BLKOF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           BLKOF = BLKOF + 1
                           CALL MINIT ('READ', LUN, IND, ISX, ISY, WIN,
     *                        IMBUFF, NBY, BLKOF, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET,
     *                           'INIT READ OF IMAGE'
                              GO TO 990
                              END IF
                           DO 130 IY = 1,NY
                              CALL MDISK ('READ', LUN, IND, IMBUFF,
     *                           BIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'READ IMAGE ROW'
                                 GO TO 990
                                 END IF
                              DO 120 IX = 1,NX
                                 TEMP = IMBUFF(BIND+IX-1)
                                 IF (TEMP.NE.FBLANK) THEN
                                    QS = QS + COS (DG2RAD * 2.D0 * TEMP)
                                    US = US + SIN (DG2RAD * 2.D0 * TEMP)
                                    NQ = NQ + 1
                                    END IF
 120                             CONTINUE
 130                          CONTINUE
 146                       CONTINUE
 147                    CONTINUE
 148                 CONTINUE
 149              CONTINUE
 150           CONTINUE
            IF (NQ.GT.0) THEN
               QS = QS / NQ
               US = US / NQ
               FREQ = FREF + (NF - REFP) * INCF
               NLINES = NLINES + 1
               IF (DOANGL) THEN
                  ANG = RAD2DG * ATAN2 (US, QS) / 2.0D0
                  WRITE (LINE,1150) FREQ, ANG, 1
               ELSE
                  WRITE (LINE,1151) FREQ, QS, US, 1
                  END IF
               L = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
                  GO TO 990
                  END IF
               END IF
 190        CONTINUE
C                                       Q/U cube
      ELSE
         CALL RCOPY (7, BLC, LBLC)
         CALL RCOPY (7, TRC, LTRC)
         CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), LBLC, LTRC, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 210 I = 1,7
            IBLC(I) = IROUND (LBLC(I))
            ITRC(I) = IROUND (LTRC(I))
 210        CONTINUE
         NX = ITRC(1) - IBLC(1) + 1
         NY = ITRC(2) - IBLC(2) + 1
         WIN(1) = IBLC(1)
         WIN(2) = IBLC(2)
         WIN(3) = ITRC(1)
         WIN(4) = ITRC(2)
         NF1 = IBLC(FAX+1)
         NF2 = ITRC(FAX+1)
         WRITE (LINE,1010) CDATE, CTIME, 'Q and U image'
         L = JTRIM (LINE)
         CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
            GO TO 990
            END IF
         DO 290 NF = NF1,NF2
            ITRC(FAX+1) = NF
            IBLC(FAX+1) = NF
C                                       read Q
            IBLC(SAX+1) = QP
            ITRC(SAX+1) = QP
            QS = 0.0
            NQ = 0
            DO 240 I7 = IBLC(7),ITRC(7)
               DO 239 I6 = IBLC(6),ITRC(6)
                  DO 238 I5 = IBLC(5),ITRC(5)
                     DO 237 I4 = IBLC(4),ITRC(4)
                        DO 236 I3 = IBLC(3),ITRC(3)
                           CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                        DEPTH, BLKOF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           BLKOF = BLKOF + 1
                           CALL MINIT ('READ', LUN, IND, ISX, ISY, WIN,
     *                        IMBUFF, NBY, BLKOF, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET,
     *                           'INIT READ OF IMAGE'
                              GO TO 990
                              END IF
                           DO 230 IY = 1,NY
                              CALL MDISK ('READ', LUN, IND, IMBUFF,
     *                           BIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'READ IMAGE ROW'
                                 GO TO 990
                                 END IF
                              DO 220 IX = 1,NX
                                 TEMP = IMBUFF(BIND+IX-1)
                                 IF (TEMP.NE.FBLANK) THEN
                                    QS = QS + TEMP
                                    NQ = NQ + 1
                                    END IF
 220                             CONTINUE
 230                          CONTINUE
 236                       CONTINUE
 237                    CONTINUE
 238                 CONTINUE
 239              CONTINUE
 240           CONTINUE
C                                       read U
            IBLC(SAX+1) = UP
            ITRC(SAX+1) = UP
            US = 0.0
            NU = 0
            DO 270 I7 = IBLC(7),ITRC(7)
               DO 269 I6 = IBLC(6),ITRC(6)
                  DO 268 I5 = IBLC(5),ITRC(5)
                     DO 267 I4 = IBLC(4),ITRC(4)
                        DO 266 I3 = IBLC(3),ITRC(3)
                           CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                        DEPTH, BLKOF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           BLKOF = BLKOF + 1
                           CALL MINIT ('READ', LUN, IND, ISX, ISY, WIN,
     *                        IMBUFF, NBY, BLKOF, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET,
     *                           'INIT READ OF IMAGE'
                              GO TO 990
                              END IF
                           DO 260 IY = 1,NY
                              CALL MDISK ('READ', LUN, IND, IMBUFF,
     *                           BIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'READ IMAGE ROW'
                                 GO TO 990
                                 END IF
                              DO 250 IX = 1,NX
                                 TEMP = IMBUFF(BIND+IX-1)
                                 IF (TEMP.NE.FBLANK) THEN
                                    US = US + TEMP
                                    NU = NU + 1
                                    END IF
 250                             CONTINUE
 260                          CONTINUE
 266                       CONTINUE
 267                    CONTINUE
 268                 CONTINUE
 269              CONTINUE
 270           CONTINUE
            IF (((QS.NE.0.0) .OR. (US.NE.0.0)) .AND. (NQ.GT.0) .AND.
     *         (NU.GT.0)) THEN
               QS = QS / NQ
               US = US / NU
               FREQ = FREF + (NF - REFP) * INCF
               NLINES = NLINES + 1
               IF (DOANGL) THEN
                  ANG = RAD2DG * ATAN2 (US, QS) / 2.0D0
                  WRITE (LINE,1150) FREQ, ANG, 1
               ELSE
                  WRITE (LINE,1151) FREQ, QS, US, 1
                  END IF
               L = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:L), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
                  GO TO 990
                  END IF
               END IF
 290        CONTINUE
         END IF
      CALL ZCLOSE (LUN, IND, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUOIM ERROR',I4,1X,A)
 1010 FORMAT ('# QUOUT run ',A,A,' using ',A)
 1150 FORMAT (F13.9,F12.5,I7)
 1151 FORMAT (F13.9,2F12.6,I7)
      END

