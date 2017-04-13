      PROGRAM LTESS
C-----------------------------------------------------------------------
C! Task do make linear combination mosaics
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002-2003, 2006-2008
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
C   LTESS makes a linear combination mosaic by correcting for the
C   individual primary beam patterns and optimising the signal-to-noise
C   ratio.
C   INPUTS:
C   AIPS name   Prog. name     Description
C   INNAME      NAME(1)        Input image name (name)
C   INCLASS     CLASS(1)       Input image name (class)
C   INSEQ       SEQ(1)         Input image name (sequence number)
C   INDISK      VOL(1)         Input image name (disk number)
C   OUTNAME     NAME(3)        Output image name (name)
C   OUTCLASS    CLASS(3)       Output image name (class)
C   OUTSEQ      SEQ(3)         Output image name (sequence number)
C   OUTDISK     VOL(3)         Output image name (disk number)
C   NMAPS       NFIELD         Number of maps to analyse
C   NOISE       FRES           Rms error for each image (Jy/beam)
C   BLC         BLC            Bottom left corner of LTESS image
C   TRC         TRC            Top right corner of LTESS image
C   PBSIZE      BMSIZE         FWHM of primary beam for each image
C                                 (arcsec)
C   APARM       APM            1 - cutoff for PB correction (%)
C   BADDISK     IBAD           List of disks on which not to put
C                                 scratch files
C   Programmer = R. Braun/T. Cornwell                      December 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(5)*12, CLASS(5)*6
      INTEGER   IRET, LUN2, LUN1, USID, VOL(5), SEQ(5)
      LOGICAL   FINISH, OLD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
C
C                                       Initialize common constants
      T = .TRUE.
      F = .FALSE.
      MAP = .TRUE.
      EXCL = .TRUE.
      WAIT = .TRUE.
      IERR = 0
      IRET = 0
      LUN1 = 16
      LUN2 = 17
      NCFILE = 0
      FINISH = .FALSE.
      SCLF = 1.0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFR1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Write inputs to history and
C                                       log files.
      CALL VMHIS (NAME, VOL, CLASS, SEQ, OLD, IERR)
      IF (IERR.NE.0) GO TO 990
C
      CONTINUE
C                                       Calculate Linear Combination
      CALL LINCOM (VMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C
      CATR(KRDMX) = MAX (CATR(KRDMX), IMGMAX)
      CATR(KRDMN) = MIN (CATR(KRDMN), IMGMIN)
      CATR(KRBLK) = FBLANK
      CATBLK(KITYP) = 4
C                                       Update VTESS catalog header
      CALL CATIO ('UPDT', VOL(3), SCRCNO(SCRNM(VMOUT)), CATBLK, 'REST',
     *   BUFFR3, IERR)
      IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1120) IERR
         CALL MSGWRT (4)
         IERR = 0
      END IF
C
C                                        Finished.
 990  IRET = IERR
      CALL DIE (IRET, BUFFR1)
C
 999  STOP
C-----------------------------------------------------------------------
 1120 FORMAT ('ERROR WRITING HEADER', I3)
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   FILES reads and floats the beam and the dirty map, opens the
C   default map, creates/opens the VTESS map, and creates the scratch
C   files. The dirty map, default map, and VM map are copied into small
C   scratch files.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(5)*12, CLASS(5)*6,  STAT*4, MAPUNT*8, MTYPE*2
      INTEGER   USID, CNO, VOL(5), SEQ(5), OLDIM, OLDNAX(7), IDEPTH(5),
     *   IDEL, JDEL, I, LUN1, LUN2, FIL, CORN(7), IFIELD, JERR, ITMP,
     *   RETVOL, ISIZE, LF, MNAX(2), NOMAT
      LOGICAL   OLD
      DOUBLE PRECISION OLDCRV(7), FREQ, MCRV(2)
      REAL      XBUF1(1), MCIC(2), MCRP(2), MEPS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (BUFFR1, XBUF1)
      DATA IDEPTH /5*1/
      DATA MAPUNT /'JY/BEAM '/
      DATA CORN /7*1/
C-----------------------------------------------------------------------
C
      IERR = 0
      NCFILE = 0
      LUN1 = 16
      LUN2 = 17
      NOMAT = 0
C                                       Read map
C                                       Get catalog slot for first map
      CNO = 1
      RETVOL = VOL(1)
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', RETVOL, CNO, NAME(1), CLASS(1), SEQ(1),
     *   MTYPE, USID, STAT, BUFFR3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), SEQ(1), RETVOL,
     *      USID
         GO TO 990
         END IF
C                                       Copy CATBLK and mark READ
      CALL CATIO ('READ', RETVOL, CNO, CATBLK, 'REST', BUFFR1(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       save coordinates
      MCRV(1) = CATD(KDCRV)
      MCIC(1) = CATR(KRCIC)
      MCRP(1) = CATR(KRCRP)
      MNAX(1) = CATBLK(KINAX)
      MCRV(2) = CATD(KDCRV+1)
      MCIC(2) = CATR(KRCIC+1)
      MCRP(2) = CATR(KRCRP+1)
      MNAX(2) = CATBLK(KINAX+1)
      MEPS = 0.01 * MAX (ABS(MCIC(1)), ABS(MCIC(2)))
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       set array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Get cell size in arcseconds
      CELLX = CATR(KRCIC)*3600.0
      CELLY = CATR(KRCIC+1)*3600.0
C                                       Set up windows
      IF ((BLC(1).LE.0) .OR. (BLC(1).GE.NX)) BLC(1) = 1
      IDEL = NX-1
      IF (TRC(1).GT.BLC(1)) IDEL = TRC(1)-BLC(1)
      TRC(1) = BLC(1) + IDEL
      IF ((BLC(2).LE.0) .OR. (BLC(2).GE.NY)) BLC(2) = 1
      JDEL = NY-1
      IF (TRC(2).GT.BLC(2)) JDEL = TRC(2) - BLC(2)
      TRC(2) = BLC(2) + JDEL
      XBEG = 1 + NX/2 - (TRC(1)-BLC(1)+1)/2
      YBEG = 1 + NY/2 - (TRC(2)-BLC(2)+1)/2
      XEND = XBEG + (TRC(1)-BLC(1))
      YEND = YBEG + (TRC(2)-BLC(2))
C
      RWNXY = REAL(TRC(1)-BLC(1)+1) * REAL(TRC(2)-BLC(2)+1)
C
      DO 10 I = 1,NUMFIL
         VMSZ(1,I) = NX
         VMSZ(2,I) = NY
 10      CONTINUE
C
      DO 20 IFIELD = 1,NFIELD
         VMSZ(1,DAT(IFIELD)) = NX
         VMSZ(2,DAT(IFIELD)) = NY
  20     CONTINUE
C
      VMSZ(1,VMOUT) = NX
      VMSZ(2,VMOUT) = NY
      HNX = NX
      HNY = NY
C                                       Create scratch files.
      NSCR = 0
      DO 30 FIL = 1,NUMSCR
         CALL MAPSIZ (2, VMSZ(1,FIL), ISIZE)
         CALL SCREAT (ISIZE, BUFFR1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, FIL
            GO TO 990
         END IF
         SCRNM(FIL) = NSCR
         VMVOL(FIL) = SCRVOL(NSCR)
         CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, VMFILE(FIL),
     *      IERR)
 30      CONTINUE
C
      SCRNM(VMOUT)  = NSCR + 1
C                                       Loop over dirty maps
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      DO 60 IFIELD = 1,NFIELD
         CNO = 1
         ITMP = SEQ(1) + IFIELD - 1
         RETVOL = VOL(1)
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', RETVOL, CNO, NAME(1), CLASS(1), ITMP,
     *      MTYPE, USID, STAT, BUFFR3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), ITMP, RETVOL,
     *         USID
            GO TO 990
            END IF
C                                       Get CATBLK for dirty map.
         CALL CATIO ('READ', RETVOL, CNO, CATBLK, 'READ', BUFFR3,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
            END IF
C                                       Mark map READ in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = RETVOL
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
C                                       Find type of telescope
C                                       Single dish if BMSIZE(IFIELD) <0
         IF (BMSIZE(IFIELD).LT.0.0) THEN
            BEAMTP(IFIELD) = 0
         ELSE IF (BMSIZE(IFIELD).GT.0.0) THEN
            BEAMTP(IFIELD) = 1
         ELSE
            CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), LF,
     *         IERR)
            IF (IERR.EQ.0) THEN
               FREQ = CATD(KDCRV+LF)
            ELSE
               FREQ = 0.0D0
               END IF
            BEAMTP(IFIELD) = 2
            IF (FREQ.NE.0.) THEN
               BMSIZE(IFIELD) = VELITE / FREQ
            ELSE
               BMSIZE(IFIELD) = 0.0
               END IF
            END IF
C                                       Find pointing center
         IDEPTH(1) = CHAN
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
         CALL XYPIX (CATD(KDORA), CATD(KDODE), RCENX(IFIELD),
     *      RCENY(IFIELD), JERR)
         IF (JERR.NE.0) THEN
            RCENX(IFIELD) = CATR(KRCRP)
            RCENY(IFIELD) = CATR(KRCRP+1)
            END IF
C
         VMVOL(DAT(IFIELD)) = RETVOL
         SCRNM(DAT(IFIELD)) = SCRNM(VMOUT) + IFIELD
         SCRCNO(SCRNM(DAT(IFIELD))) = CNO
         CALL ZPHFIL ('MA', RETVOL, CNO, 1, VMFILE(DAT(IFIELD)), IERR)
C                                       Set up block offsets for reading
         CORN(3) = CHAN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *      VMBO(DAT(IFIELD)), IERR)
         VMBO(DAT(IFIELD)) = VMBO(DAT(IFIELD)) + 1
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       test
         IF ((ABS(MCRV(1)-CATD(KDCRV)).GT.MEPS) .OR.
     *      (ABS(MCRV(2)-CATD(KDCRV+1)).GT.MEPS) .OR.
     *      (ABS(MCIC(1)-CATR(KRCIC)).GT.0.01*MEPS) .OR.
     *      (ABS(MCIC(2)-CATR(KRCIC+1)).GT.0.01*MEPS) .OR.
     *      (ABS(MCRP(1)-CATR(KRCRP)).GT.0.1) .OR.
     *      (ABS(MCRP(2)-CATR(KRCRP+1)).GT.0.1) .OR.
     *      (MNAX(1)-CATBLK(KINAX).NE.0) .OR.
     *      (MNAX(2)-CATBLK(KINAX+1).NE.0)) THEN
            WRITE (MSGTXT,1050) IFIELD
            CALL MSGWRT (7)
            NOMAT = NOMAT + 1
            END IF
 60      CONTINUE
      IF (NOMAT.GT.0) THEN
         WRITE (MSGTXT,1060) NOMAT
         IERR =  NOMAT
         GO TO 990
         END IF
C                                       End of loop over dirty maps
C                                       Create VM file if necessary
C                                       Put NAME in CATBLK
      IF (NAME(3).NE.' ') CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
C                                       Put VM class into CATBLK
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
C                                        Put in units of map
      CALL CHR2H (8, MAPUNT, 1, CATH(KHBUN))
C                                       Update sequence.
      CATBLK(KIIMS) = SEQ(3)
      CATR(KRDMX) =  -1.E20
      CATR(KRDMN) =  1.E20
      CATR(KRBLK) = FBLANK
C                                       Create output LTESS map file.
      CALL MCREAT (VOL(3), CNO, BUFFR1, IERR)
      OLD = .FALSE.
      IF (IERR.NE.0) THEN
         OLD = .TRUE.
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1190) IERR
            GO TO 990
C                                       Existing file: check it out
         ELSE
            OLDIM = CATBLK(KIDIM)
            DO 70 I = 1,7
               OLDNAX(I) = CATBLK(KINAX+I-1)
               OLDCRV(I) = CATD(KDCRV+I-1)
 70           CONTINUE
            CALL CATIO ('READ', VOL(3), CNO, CATBLK, 'REST', BUFFR3,
     *         IERR)
            IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 80
            IMGMAX = CATR(KRDMX)
            IMGMIN = CATR(KRDMN)
            DO 90 I = 1,2
               IF (OLDNAX(I).NE.CATBLK(KINAX+I-1)) GO TO 80
               IF (OLDCRV(I).NE.CATD(KDCRV+I-1)) GO TO 80
 90            CONTINUE
C                                       mark WRITE
            CALL CATIO ('WRIT', VOL(3), CNO, CATBLK, 'WRIT', BUFFR3,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1210) IERR
               CALL MSGWRT (8)
               END IF
            GO TO 100
C                                       not match
 80         IERR = 8
            WRITE (MSGTXT,1215)
            GO TO 990
            END IF
         END IF
C
 100  CORN(3) = CHAN
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *   VMBO(VMOUT), IERR)
      VMBO(VMOUT) = VMBO(VMOUT) + 1
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
C                                       Actual seq #
      SEQ(3) = CATBLK(KIIMS)
C                                        Mark for READ in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       copy keywords
      CALL KEYCOP (FVOL(1), FCNO(1), VOL(3), CNO, IERR)
C                                       Fill output file into common
      VMVOL(VMOUT) = VOL(3)
      SCRCNO(SCRNM(VMOUT)) = CNO
      CALL ZPHFIL ('MA', VOL(3), CNO, 1, VMFILE(VMOUT), IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILES: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I4)
 1010 FORMAT ('FILES: CANNOT COPY BEAM CATBLK, ERROR',I3)
 1020 FORMAT ('FILES: ERROR',I3,' CREATING SCRATCH FILE ',I2)
 1040 FORMAT ('FILES: CANNOT COPY DIRTY MAP, ERROR ',I3)
 1045 FORMAT ('FILES: CANNOT COPY LTESS MAP, ERROR ',I3)
 1050 FORMAT ('FILES: FIELD',I5,' DOES NOT MATCH GEOMETRY OF FIELD 1')
 1060 FORMAT ('FILES:',I5,' FIELDS DO NOT MATCH: USE HGEOM')
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1190 FORMAT ('FILES: COULD NOT CREATE LTESS MAP FILE, ERROR ',I3)
 1210 FORMAT ('FILES: CANNOT UPDATE LTESS CATBLK, ERROR',I3)
 1215 FORMAT ('OLD LTESS MAP NOT COMPATIBLE WITH DIRTY MAP')
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C   initializes the parameters, and sets up the file system.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DTESS.INC'
C
      CHARACTER PRGNAM*6, NAME(5)*12, CLASS(5)*6, MTYPE*2
      HOLLERITH XNAM1(3), XNAM3(3), XCLAS1(2), XCLAS3(2)
      INTEGER   USID, SEQ(5), VOL(5), I, LUN1, RETVOL
      INTEGER   NPARMS, IND, CNO, IROUND
      REAL      XSEQ1, XSEQ3, XVOL1, XVOL3, XERROR(64), XGAIN, XNPPB,
     *   XNFIEL, XBLC(7), XTRC(7), XBMSIZ(64), XAPM(7), XBAD(10)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XNAM3, XCLAS3, XSEQ3,
     *   XVOL3, XNFIEL, XERROR, XBLC, XTRC, XBMSIZ, XAPM, XBAD
      DATA PRGNAM /'LTESS '/
C-----------------------------------------------------------------------
C
      IERR = 0
      LUN1 = 16
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (.TRUE., BUFFR1)
      CALL VHDRIN
      CALL HIINIT (5)
      BLC(1) = 0
      BLC(2) = 0
      TRC(1) = 0
      TRC(2) = 0
      BUFSZ(1) = XBUFSZ * 2
      BUFSZ(2) = XBUFSZ * 2
      BUFSZ(3) = XBUFSZ * 2
      BUFSZ(4) = XBUFSZ * 2
      BUFSZ(5) = XBUFSZ * 2
      BUFSZ(6) = XBUFSZ * 2
      BPS = NBPS
C                                       Get AIPS adverbs.
      NPARMS = 7 + 7 + 1 + 64 + 7 + 7 + 64 + 7 + 10
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFFR1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       get actual dirty map name
 10   VOL(1) = IROUND(XVOL1)
      SEQ(1) = IROUND(XSEQ1)
      USID = NLUSER
      RETVOL = VOL(1)
      CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      MTYPE = 'MA'
      CALL MAPOPN ('READ', RETVOL, NAME(1), CLASS(1), SEQ, MTYPE, USID,
     *   LUN1, IND, CNO, CATBLK, BUFFR1, IERR)
      DRTCNO = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR,'READ'
         CALL MSGWRT (8)
         GO TO 999
         END IF
 20   CALL MAPCLS ('READ', RETVOL, CNO, LUN1, IND, CATBLK, F, BUFFR1,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, 'READ'
         CALL MSGWRT (6)
         END IF
 25   IERR = 0
C                                       Default name for Beam and LTESS
C                                       maps is Dirty map name.
      IF (NAME(3).EQ.' ') NAME(3) = NAME(1)
C                                       Copy CLASSes or default.
      IF (CLASS(3).EQ.' ') THEN
         IF (INDEX(CLASS(1), 'MAP') .EQ. 2) THEN
            CLASS(3)(1:1) = CLASS(1)(1:1)
            CLASS(3)(2:)  = 'LTESS'
         ELSE
            CLASS(3) = 'LTESS '
            END IF
         END IF
C                                       Get volume numbers
      VOL(3) = IROUND (XVOL3)
C                                       Get sequence numbers
      SEQ(3) = IROUND (XSEQ3)
C
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 70      CONTINUE
C                                       Get LTESS information
      BLC(1) = IROUND (MIN (XBLC(1), XTRC(1)))
      BLC(2) = IROUND (MIN (XBLC(2), XTRC(2)))
      TRC(1) = IROUND (MAX (XBLC(1), XTRC(1)))
      TRC(2) = IROUND (MAX (XBLC(2), XTRC(2)))
      CHAN = IROUND (MAX (1.0, XBLC(3)))
      NFIELD = MIN (NINT(XNFIEL), MATFLD)
      IF (NFIELD.LT.1) NFIELD = 1
C
      XNPPB = (XBAD(9) - IBAD(9)) * 1000
      RNPPB = MAX (XNPPB, 0.0)
      IF (XNPPB.EQ.0.0) RNPPB = 1.0
      XGAIN = XBAD(10) - IBAD(10)
      IF (XGAIN.NE.0.0) GAIN = XGAIN
      BLANKD = F
      DO 666 I = 1,NFIELD
         IF (I.LE.64) THEN
            FRES(I) = XERROR(I)
            BMSIZE(I) = XBMSIZ(I)
         ELSE
            FRES(I) = XERROR(64)
            BMSIZE(I) = XBMSIZ(64)
            END IF
         IF (BMSIZE(I).GE.0.0) BLANKD = T
 666     CONTINUE
C                                       cutoff for primary beam
      IF (XAPM(1).LE.0.0) XAPM(1) = 0.07
      CALL RCOPY (7, XAPM, PBPARM)
C                                       Get bad disks.
      IMG = 1
      RES = 2
      SCR = 3
      WK1 = 4
      WK2 = 5
      PRS = 6
      NUMSCR = PRS
C
      DO 779 I = 1,NFIELD
         DAT(I) = NUMSCR + I
 779     CONTINUE
      VMOUT = DAT(NFIELD) + 1
      NUMFIL = VMOUT
C                                       Output files go in common.
      WRITE (MSGTXT,1200) NUMSCR
      CALL MSGWRT (4)
      DO 125 I = 1,NUMFIL
         VMBO(I) = 1
 125     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('GETIN: ERROR',I3,' OPENING DIRTY MAP FILE FOR ',A4)
 1020 FORMAT ('GETIN: ERROR',I3,' CLOSING DIRTY MAP FILE FOR ',A4)
 1200 FORMAT ('Creating ',I3,' scratch files')
      END
      SUBROUTINE LINCOM (OUT, IERR)
C-----------------------------------------------------------------------
C Make linear combination image
C
C   Programmer =  R. Braun      November 1987
C-----------------------------------------------------------------------
      INTEGER    OUT, IFIELD
      REAL       VAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      DO 100 IFIELD=1,NFIELD
C
         IF (FRES(IFIELD).GT.0.0) THEN
            VAL = 1/FRES(IFIELD)**2
         ELSE
            VAL = 1.0
            END IF
         IF (IFIELD.EQ.1) THEN
            CALL APLPBI(IFIELD, DAT(IFIELD), VAL, PRS, RES, IERR)
            IF (IERR.NE.0) GO TO 990
         ELSE
            CALL APLPBI(IFIELD, DAT(IFIELD), VAL, WK1, WK2, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL ADDMAP(WK1, PRS, PRS, FBLANK, RESMAX, RESMIN, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL ADDMAP(WK2, RES, RES, FBLANK, RESMAX, RESMIN, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 100     CONTINUE
C
C Now divide by (primary beam)**2
C
      CALL DIVMAP (PRS, RES, PRS, FBLANK, IMGMAX, IMGMIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL SMTOBG (PRS, FBLANK, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LINCOM')
      END
      SUBROUTINE VMHIS (NAME, VOL, CLASS, SEQ, OLD, IERR)
C-----------------------------------------------------------------------
C   VMHIS copies the dirty map history if the VM map does not
C   already have a history file.  Then the inputs to VM with
C   the default values are added to the history file.
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(5)*12, CLASS(5)*6, HILINE*72, BEAMNM(2)*8, ATIME*8,
     *   ADATE*12
      INTEGER   VOL(5), SEQ(5), LUN1, LUN2, ITMDAT(6), IFIELD
      LOGICAL   OLD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BEAMNM /'Gaussian','VLA     '/
C-----------------------------------------------------------------------
      IERR = 0
      LUN1 = 16
      LUN2 = 17
C                                       Open old file: no copy old HI
      IF (.NOT.OLD) GO TO 20
         CALL HIOPEN (LUN1, VMVOL(VMOUT), SCRCNO(SCRNM(VMOUT)),
     *    BUFFR2, IERR)
         IF (IERR.EQ.5) GO TO 20
         IF (IERR.NE.0) GO TO 10
            CALL ZDATE (ITMDAT(1))
            CALL ZTIME (ITMDAT(4))
            CALL TIMDAT (ITMDAT(4), ITMDAT(1), ATIME, ADATE)
            WRITE (HILINE,1000) TSKNAM, RLSNAM, ADATE, ATIME
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            IF (IERR.NE.0) GO TO 50
            GO TO 30
 10      CONTINUE
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 60
C                                       Copy/open history files.
 20   CONTINUE
         CALL HISCOP (LUN2, LUN1, VMVOL(DAT(1)), VOL(3),
     *       SCRCNO(SCRNM(DAT(1))),
     *       SCRCNO(SCRNM(VMOUT)), CATBLK, BUFFR1, BUFFR2, IERR)
         IF (IERR.EQ.0) GO TO 30
C                                       Check if old file doesn't exist
         IF (IERR.EQ.2) GO TO 30
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (6)
            GO TO 60
C                                       Add to history file.
C                                       Dirty map name.
 30   WRITE (MSGTXT,2000) TSKNAM, NAME(1),CLASS(1), SEQ(1), VOL(1)
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       LTESS map name.
      WRITE (MSGTXT,2002) TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3)
      CALL MSGWRT (3)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2009) TSKNAM, BLC(1), BLC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2010) TSKNAM, TRC(1),TRC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Pointing centers
      DO 777 IFIELD=1,NFIELD
         WRITE (HILINE,2023) TSKNAM, IFIELD, IFIELD,
     *      RCENX(IFIELD), RCENY(IFIELD)
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
 777     CONTINUE
C                                       Noise level
      DO 779 IFIELD=1,NFIELD
         WRITE (HILINE,2006) TSKNAM, IFIELD, FRES(IFIELD)
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
 779     CONTINUE
C                                       Pointing centers
      DO 780 IFIELD=1,NFIELD
      IF (BEAMTP(IFIELD).EQ.0) THEN
         WRITE (HILINE,2026) TSKNAM, IFIELD
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
      ELSE
         WRITE (HILINE,2028) TSKNAM, IFIELD
         CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         IF (IERR.NE.0) GO TO 50
         IF (BMSIZE(IFIELD).LE.0.0) THEN
            WRITE (HILINE,2027) TSKNAM, IFIELD
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            IF (IERR.NE.0) GO TO 50
         ELSE
            WRITE (HILINE,2025) TSKNAM, IFIELD, BEAMNM(BEAMTP(IFIELD))
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            IF (IERR.NE.0) GO TO 50
            IF (BEAMTP(IFIELD).EQ.1) THEN
               WRITE (HILINE,2024) TSKNAM, IFIELD, BMSIZE(IFIELD)
            ELSE
               WRITE (HILINE,2022) TSKNAM, IFIELD, BMSIZE(IFIELD)
               END IF
            CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            IF (IERR.NE.0) GO TO 50
         END IF
      END IF
  780 CONTINUE
C                                       CHAN
      WRITE (HILINE,2177) TSKNAM, CHAN
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C
      GO TO 60
C                                       Error has occured.
 50   WRITE (MSGTXT,1050) IERR
      CALL MSGWRT (6)
C                                       Close history files.
 60   CALL HICLOS (LUN1, T, BUFFR2, IERR)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /******** Restart ',
     *   A12,1X,A8)
 1010 FORMAT ('UNABLE TO OPEN OLD LTESS-MAP HISTORY FILE')
 1020 FORMAT ('VMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('VMHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,'Dirty map =    ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2002 FORMAT (A6,'Ltess map =    ''',A12,''' . ''',A6,
     *   ''' . ',2I4)
 2009 FORMAT (A6,'BLC = ',2(I5,1X),' /Bottom left corner')
 2010 FORMAT (A6,'TRC = ',2(I5,1X),' /Top   right corner')
 2177 FORMAT (A6,'BLC(3) = ',I6,' /Channel number')
 2022 FORMAT (A6,'/Field',I5,' VLA lambda=',F7.4,' m for primary beam')
 2023 FORMAT (A6,'PBCENX(',I4,'), PBCENY(',I4,') = ',F7.2,',',
     *   F7.2,' /Pixels')
 2024 FORMAT (A6,'PBSIZE(',I4,') = ',F8.1,'/ Arcseconds')
 2025 FORMAT (A6,'/Field',I5,' : Primary beam = ',A8)
 2026 FORMAT (A6,'/Field',I5,' : Single aperture data')
 2028 FORMAT (A6,'/Field',I5,' : Interferometer  data')
 2027 FORMAT (A6,'/Field',I5,' : No primary beam correction')
 2006 FORMAT (A6,'NOISE(',I4,') = ',1PE11.3,'/ Jy/beam')
      END
