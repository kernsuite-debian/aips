LOCAL INCLUDE 'IMERG.INC'
      INTEGER BUFF1(256)
      INCLUDE 'INCS:DTCIO.INC'
      COMMON /OTHRIM/ BUFF1
LOCAL END
      PROGRAM IMERG
C-----------------------------------------------------------------------
C! Combine images in the Fourier domain
C# Map-util AP-fft
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2002-2003, 2006, 2008-2009, 2011
C;  Copyright (C) 2015
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
C   IMERG merges two input images by fourier transforming both,
C   normalizing the second to amplitudes within a U,V annulus of the
C   first then producing an output transform consisting of the
C   inner plane from the second input, the mean within the annulus
C   and the outer plane from the first. The output is the back
C   transform of this plane.
C   Last Edit : 18-November-1987
C   Programmer = R. Braun                     November 1987
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN2, LUN1, USID, VOL(4), SEQ(4), ITER
      LOGICAL   FINISH
      CHARACTER CLASS(4)*6, NAME(4)*12, STYPE*4
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C
C *********************************************************************
C
C                        INITIALIZATION PHASE
C
C *********************************************************************
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
      ITER = 0
      NCFILE = 0
      FINISH = .FALSE.
      SCLF = 1.0
C                                       Get input values.
      CALL GETIN (USID, NAME, VOL, CLASS, SEQ, STYPE, IRET)
C                                       Check for restart of AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFR1, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Get map files and create
C                                       output files if required.
      CALL FILES (USID, NAME, VOL, CLASS, SEQ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Calculate merger
      CALL VMERGE (VMOUT, STYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      CATR(KRDMX) = IMGMAX
      CATR(KRDMN) = IMGMIN
      CATBLK(KITYP) = 4
C                                       Update IMERG catalog header
      CALL CATIO ('UPDT', VOL(3), FCNO(3), CATBLK, 'REST', BUFFR3, IERR)
      IF ((IERR.NE.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1120) IERR
         CALL MSGWRT (4)
         END IF
C                                       Write inputs to history and
C                                       log files.
      CALL VMHIS (NAME, VOL, CLASS, SEQ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Finished.
 990  IRET = IERR
      CALL DIE (IRET, BUFFR1)
C
 999  STOP
C-----------------------------------------------------------------------
 1120 FORMAT ('ERROR',I3,' UPDATING IMERG HEADER ')
      END
      SUBROUTINE FILES (USID, NAME, VOL, CLASS, SEQ, IERR)
C-----------------------------------------------------------------------
C   Programmer =  R. Braun                     November 1987
C-----------------------------------------------------------------------
      CHARACTER NAME(4)*12, CLASS(4)*6
      INTEGER   USID
C
      CHARACTER STAT*4, MTYPE*2
      INTEGER   CATBLK(256), CNO, VOL(4), SEQ(4), I, LUN1, LUN2, FIL,
     *   CORN(7), WIN(4), XOFF, YOFF, IFIELD, ITMP, ISIZE, J
      DOUBLE PRECISION CATD(128)
      HOLLERITH CATH(256)
      REAL      CATR(256), XBUF1(1)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (BUFFR1, XBUF1)
C-----------------------------------------------------------------------
C
      IERR = 0
      NCFILE = 0
      LUN1 = 16
      LUN2 = 17
C
C                                       Get catalog slot for first MAP
      CNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', VOL(1), CNO, NAME(1), CLASS(1), SEQ(1),
     *   MTYPE, USID, STAT, BUFFR3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *      USID
         GO TO 990
         END IF
C                                       Copy CATBLK
      CALL CATIO ('READ', VOL(1), CNO, CATBLK, 'REST', BUFF1(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Determine NX and NY
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      CALL POWER2 (NX, I)
      CALL POWER2 (NY, J)
      IF ((NX.NE.I) .OR. (NY.NE.J)) THEN
         IERR = 10
         MSGTXT = 'IMAGE MUST BE A POWER OF 2 IN SIZE: SEE PADIM'
         GO TO 990
         END IF
      IF ((NX.LT.64) .OR. (NY.LT.64)) THEN
         MSGTXT = 'MYFFT DOES NOT WORK ON SUCH SMALL IMAGES'
         IERR = 11
         GO TO 990
         END IF
      IF ((NX.GT.MAXIMG) .OR. (NY.GT.MAXIMG)) THEN
         IERR = 12
         MSGTXT = 'MYFFT DOES NOT WORK ON SUCH LARGE IMAGES'
         GO TO 990
         END IF
C                                       are they powers of 2?
      IF (DORES) THEN
         BMSIZE(1) = CATR(KRBMJ) * 3600.
         BMSIZE(2) = CATR(KRBMN) * 3600.
         BMSIZE(3) = CATR(KRBPA)
      ELSE
         CALL RFILL (3, 0.0, BMSIZE)
         END IF
C                                       Set up windows
      BLC(1) = 1
      BLC(2) = 1
      TRC(1) = NX
      TRC(2) = NY
      XBEG = 1 + NX/2 - (TRC(1)-BLC(1)+1)/2
      YBEG = 1 + NY/2 - (TRC(2)-BLC(2)+1)/2
      XEND = XBEG + (TRC(1)-BLC(1))
      YEND = YBEG + (TRC(2)-BLC(2))
C
      RWNXY = REAL(TRC(1)-BLC(1)+1) * REAL(TRC(2)-BLC(2)+1)
C                                       set array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Accessing potential cube
      DO 20 I = 1,7
         CORN(I) = 1
 20      CONTINUE
C
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C
         DO 30 I = 1,NUMFIL
            VMSZ(1,I) = NX
            VMSZ(2,I) = NY
 30         CONTINUE
C
         VMSZ(1,WK1) = NY * 2
         VMSZ(2,WK1) = NX / 2 + 1
         VMSZ(1,WK2) = NY * 2
         VMSZ(2,WK2) = NX / 2 + 1
         VMSZ(1,WK3) = NY * 2
         VMSZ(2,WK3) = NX / 2 + 1
         VMSZ(1,VMOUT) = NX
         VMSZ(2,VMOUT) = NY
C                                       Create scratch files.
      NSCR = 0
      DO 50 FIL = NFIELD+1,NUMFIL-1
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
 50      CONTINUE
C
C                                       Loop over maps
C                                       Get catalog slot number for
C                                       the dirty map and it's CATBLK.
      DO 100 IFIELD = 1,NFIELD
         CNO = 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', VOL(IFIELD), CNO, NAME(IFIELD),
     *      CLASS(IFIELD), SEQ(IFIELD), MTYPE, USID, STAT, BUFFR3, IERR)
         IF (IERR.NE.0) THEN
            ITMP = SEQ(IFIELD) + 0.01
            WRITE (MSGTXT,1000) IERR, NAME(IFIELD), CLASS(IFIELD), ITMP,
     *         VOL(IFIELD), USID
            GO TO 990
            END IF
C                                       Get CATBLK for dirty map.
         CALL CATIO ('READ', VOL(IFIELD), CNO, CATBLK, 'READ', BUFFR3,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
            END IF
C                                       Mark map READ in /CFILES/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL(IFIELD)
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
C                                       Get dirty max,min.
C                                       Set info into COMMON.
         CALL ZPHFIL ('MA', VOL(IFIELD), CNO, 1, VMFILE(DAT(IFIELD)),
     *      IERR)
         VMVOL(DAT(IFIELD))= VOL(IFIELD)
         VMBO(DAT(IFIELD)) = 1
C                                       Read dirty map.
         WIN(1) = BLC(1)
         WIN(2) = BLC(2)
         WIN(3) = TRC(1)
         WIN(4) = TRC(2)
C                                       No shift for maps
         XOFF = 0
         YOFF = 0
C                                       Get cell size in arcseconds
C                                       We need it for primary beam
C                                       correction
         CELLX = CATR(KRCIC)*3600.
         CELLY = CATR(KRCIC+1)*3600.
C                                       Get restoring (or observing)
C                                       beam parms for low res. image
         IF (DORES) THEN
            BMAJ = CATR(KRBMJ)*3600.
            BMIN = CATR(KRBMN)*3600.
            BPA = CATR(KRBPA)
         ELSE
            BMAJ = 0.0
            BMIN = 0.0
            BPA = 0.0
            END IF
 100     CONTINUE
C                                       End of loop over dirty maps
C
C                                       Make sure NX,NY same as
C                                       for the beam.
      IF ((NX.NE.CATBLK(KINAX)) .OR. (NY.NE.CATBLK(KINAX+1))) THEN
         IERR = 1
         WRITE (MSGTXT,1105)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1106) NX,NY,CATBLK(KINAX),CATBLK(KINAX+1)
         GO TO 990
         END IF
C                                       Create VM file if necessary
C                                       Put NAME in CATBLK
      CALL CHR2H (12, NAME(3), KHIMNO, CATH(KHIMN))
C                                       Put VM class into CATBLK
      CALL CHR2H (6, CLASS(3), KHIMCO, CATH(KHIMC))
C                                       Update sequence.
      CATBLK(KIIMS) = SEQ(3)
C                                       Create output VM map file.
      CATBLK(KINAX+2) = 1
      CALL MCREAT (VOL(3), CNO, BUFFR1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
      SEQ(3) = CATBLK(KIIMS)
C                                        Mark for READ in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL(3)
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Fill output file into common
      VMVOL(VMOUT) = VOL(3)
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
 1090 FORMAT ('FILES: CANNOT COPY MAP CATBLK, ERROR',I3)
 1100 FORMAT ('FILES: ERROR',I5,' CREATING OUTPUT IMAGE')
 1105 FORMAT ('FILES: UNEQUAL DIMENSIONS IN MAPS')
 1106 FORMAT ('       FIRST =',2I5,' SECOND =',2I5)
      END
      SUBROUTINE GETIN (USID, NAME, VOL, CLASS, SEQ, STYPE, IERR)
C-----------------------------------------------------------------------
C   GETIN gets the input parameters for the program from AIPS
C   initializes the parameters, and sets up the file system.
C   Programmer =  R. Braun                        November 1987
C-----------------------------------------------------------------------
      INTEGER   USID, SEQ(3), VOL(3)
      CHARACTER NAME(4)*12, CLASS(4)*6, STYPE*4
C
      CHARACTER PRGNAM*6, MTYPE*2
      HOLLERITH XNAM1(3), XNAM2(3), XNAM3(3), XCLAS1(2), XCLAS2(2),
     *   XCLAS3(2), XSTYPE
      INTEGER   NPARMS,CATBLK(256), IND, CNO, I, LUN1
      REAL      XSEQ1, XSEQ2, XSEQ3, XVOL1, XVOL2, XVOL3, XCHAN,
     *   XUVRAN(2), XWT, XBAD(10), XSCLF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XVOL1, XNAM2, XCLAS2, XSEQ2,
     *   XVOL2, XNAM3, XCLAS3, XSEQ3, XVOL3, XCHAN, XUVRAN, XSCLF,
     *   XSTYPE, XWT, XBAD
      DATA PRGNAM /'IMERG '/
C-----------------------------------------------------------------------
      IERR = 0
      LUN1 = 16
C                                        Initialize common parameters.
C                                        global areas
      CALL ZDCHIN (.TRUE., BUFFR1)
      CALL VHDRIN
      CALL HIINIT (4)
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
      NPARMS = 37
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAM1, BUFFR1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Convert characters
      CALL H2CHR (12, 1, XNAM1, NAME(1))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (12, 1, XNAM2, NAME(2))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (12, 1, XNAM3, NAME(3))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      CALL H2CHR (4, 1, XSTYPE, STYPE)
      DORES = XWT.GT.0.0
C                                       get actual dirty map name
      VOL(1) = XVOL1 + 0.1
      SEQ(1) = XSEQ1 + 0.1
      USID = NLUSER
      MTYPE = 'MA'
      CALL MAPOPN ('READ', VOL, NAME(1), CLASS(1), SEQ, MTYPE, USID,
     *   LUN1, IND, CNO, CATBLK, BUFFR1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL MAPCLS ('READ', VOL, CNO, LUN1, IND, CATBLK, F, BUFFR1,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, 'READ'
         CALL MSGWRT (6)
         END IF
      IERR = 0
C                                       Default name for Beam and VM
C                                       maps is Dirty map name.
      IF (NAME(2).EQ.' ') NAME(2) = NAME(1)
      IF (NAME(3).EQ.' ') NAME(3) = NAME(1)
C                                       Copy CLASSes or default.
      IF (CLASS(2).EQ.' ') THEN
         CLASS(2) = 'IBEAM '
         IF (CLASS(1).EQ.'RMAP') CLASS(2) = 'RBEAM '
         IF (CLASS(1).EQ.'LMAP') CLASS(2) = 'LBEAM '
         END IF
      IF (CLASS(3).EQ.' ') THEN
         CLASS(3) = 'IMERG '
         IF (CLASS(1).EQ.'RMAP') CLASS(3) = 'RMERG '
         IF (CLASS(1).EQ.'LMAP') CLASS(3) = 'LMERG '
         END IF
C                                       Get volumn numbers
      VOL(2) = XVOL2 + 0.1
      VOL(3) = XVOL3 + 0.1
C                                       Get sequence numbers
      SEQ(2) = XSEQ2 + 0.1
      SEQ(3) = XSEQ3 + 0.1
      IF (SEQ(2).LE.0) SEQ(2) = SEQ(1)
C
      DO 70 I = 1,10
         IBAD(I) = XBAD(I)+0.1
 70      CONTINUE
      CHAN = MAX (1.0, XCHAN+0.1)
      DEFMIN = XUVRAN(1)
      DEFMAX = XUVRAN(2)
      SCLF = 0
      IF (XSCLF.GT.0.) SCLF = XSCLF
      NFIELD = 2
C
C                                       VM common areas
      DO 80 I = 1,NFIELD
         DAT(I) = I
 80      CONTINUE
      WK1 = DAT(NFIELD) + 1
      WK2 = DAT(NFIELD) + 2
      WK3 = DAT(NFIELD) + 3
C
      NUMFIL = WK3
      NUMSCR = NUMFIL - NFIELD
C                                       Output file goes in common.
      NUMFIL = NUMFIL + 1
      VMOUT = NUMFIL
      DO 125 I = 1,NUMFIL
         VMBO(I) = 1
 125     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('GETIN: ERROR',I3,' OPENING DIRTY MAP FILE FOR ',A4)
 1020 FORMAT ('GETIN: ERROR',I3,' CLOSING DIRTY MAP FILE FOR ',A4)
      END
      SUBROUTINE VMERGE (OUT, STYPE, IERR)
C-----------------------------------------------------------------------
C   VMERGE merges the u,v planes of DAT(1) and DAT(2)
C   and stores the answer in OUT.
C   Programmer =  R. Braun                    November 1987
C-----------------------------------------------------------------------
      INTEGER   OUT, IDIR, ULIM, KAP, NEED
      REAL      RMAX, RMIN, CELLU, CELLV, USQ, CELUSQ, CELVSQ, RADSQ,
     *   AMSQ1, AMSQ2, NORM2, RFACTH, UVMNSQ, UVMXSQ, UVAVSQ, WW, SIGMA
      INTEGER   AKOPEN, AKCESS, AKCLOS
      CHARACTER STYPE*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      RFACTH = 0.5
      CELLU = 3.6*180./(CELLX*REAL(NX)*3.1415927)
      CELUSQ = CELLU**2
      CELLV = 3.6*180./(CELLY*REAL(NY)*3.1415927)
      CELVSQ = CELLV**2
      UVMNSQ = DEFMIN**2
      UVMXSQ = DEFMAX**2
      UVAVSQ = ((DEFMIN+DEFMAX)/2.0)**2
      SIGMA = 2.0 * SQRT (LOG(2.0)) / (DEFMAX - DEFMIN)
C                                       Do FFT to get transform of IMG1
      IDIR = 3
C                                       should be enough
      NEED = 4 * NX * NY + 4 * (NX + NY)
      NEED = NEED / 1024
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = 0
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = 2 * NX * NY + 2 * (NX + NY)
         NEED = NEED / 1024
         NEED = MIN (32 * 1024, NEED)
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = 0
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 5120
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               MSGTXT = 'VMERGE: FAILED TO GET ANY AP MEMORY'
               CALL MSGWRT (8)
               IERR = 10
               GO TO 999
               END IF
            END IF
         END IF
      CALL MYFFT (NX, NY, IDIR, T, DAT(1), WK3, WK1, BUFSZ(1), BUFFR1,
     *   BUFFR2, RMAX, RMIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL QRLSE
C                                       Do FFT to get transform of IMG2
      IDIR = 3
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'VMERGE(2): FAILED TO GET ANY AP MEMORY'
         CALL MSGWRT (8)
         IERR = 10
         GO TO 999
         END IF
      CALL MYFFT (NX, NY, IDIR, T, DAT(2), WK2, WK3, BUFSZ(1), BUFFR1,
     *   BUFFR2, RMAX, RMIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Apply de-convolving function
      CALL MULGAU (WK3, WK2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL QRLSE
C
      ULIM = NX / 2 + 1
      IF (SCLF.GT.0.) THEN
         NORM2 = SCLF
      ELSE
      IF (AKOPEN (WK1,1, 'READ', BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (WK2,2, 'READ', BUFFR2).NE.0) GO TO 990
C                                       Begin loop thru map.
C                                       Normalise appropriately
      AMSQ1 = 0.
      AMSQ2 = 0.
      DO 125 IY = 1,ULIM
         USQ = CELUSQ*(IY-1)**2
         IF (AKCESS(WK1,BUFFR1).NE.0) GO TO 990
         IF (AKCESS(WK2,BUFFR2).NE.0) GO TO 990
         I1 = BIND(1)
         I2 = BIND(2)
         DO 120 IX = 1,NY/2+1
            RADSQ = USQ + CELVSQ*(IX-1)**2
            IF (RADSQ.GT.UVMNSQ.AND.RADSQ.LT.UVMXSQ) THEN
               AMSQ1 = AMSQ1 + BUFFR1(I1)**2 + BUFFR1(I1+1)**2
               AMSQ2 = AMSQ2 + BUFFR2(I2)**2 + BUFFR2(I2+1)**2
            END IF
            I1 = I1 + 2
            I2 = I2 + 2
 120        CONTINUE
         DO 122 IX = NY/2+2,NY
            RADSQ = USQ + CELVSQ*(NY+1-IX)**2
            IF (RADSQ.GT.UVMNSQ.AND.RADSQ.LT.UVMXSQ) THEN
               AMSQ1 = AMSQ1 + BUFFR1(I1)**2 + BUFFR1(I1+1)**2
               AMSQ2 = AMSQ2 + BUFFR2(I2)**2 + BUFFR2(I2+1)**2
            END IF
            I1 = I1 + 2
            I2 = I2 + 2
 122        CONTINUE
 125     CONTINUE
      IF (AKCLOS (WK1, BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (WK2, BUFFR2).NE.0) GO TO 990
C
      NORM2 = 0
      IF (AMSQ2.GT.0.) NORM2 = SQRT(AMSQ1/AMSQ2)
      SCLF = NORM2
      END IF
      WRITE (MSGTXT,2000) NORM2
      CALL MSGWRT (4)
      IF (AKOPEN (WK1, 1, 'READ', BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (WK2, 2, 'READ', BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (WK3, 3, 'WRIT', BUFFR3).NE.0) GO TO 990
C
      DO 135 IY = 1,ULIM
         USQ = CELUSQ*(IY-1)**2
         IF (AKCESS (WK1, BUFFR1).NE.0) GO TO 990
         IF (AKCESS (WK2, BUFFR2).NE.0) GO TO 990
         IF (AKCESS (WK3, BUFFR3).NE.0) GO TO 990
         I1 = BIND(1)
         I2 = BIND(2)
         I3 = BIND(3)
         DO 130 IX = 1,NY/2+1
            RADSQ = USQ + CELVSQ*(IX-1)**2
C                                       inner
            IF (RADSQ.LT.UVMNSQ) THEN
               BUFFR3(I3) = NORM2*BUFFR2(I2)
               BUFFR3(I3+1) = NORM2*BUFFR2(I2+1)
C                                       outer
            ELSE IF (RADSQ.GT.UVMXSQ) THEN
               BUFFR3(I3) = BUFFR1(I1)
               BUFFR3(I3+1) = BUFFR1(I1+1)
C                                       average MEAN
            ELSE IF (STYPE.EQ.'MEAN') THEN
               BUFFR3(I3) = RFACTH * (BUFFR1(I1) + NORM2*BUFFR2(I2))
               BUFFR3(I3+1) = RFACTH*(BUFFR1(I1+1)
     *            + NORM2*BUFFR2(I2+1))
C                                       feather
            ELSE IF (RADSQ.LT.UVAVSQ) THEN
               WW = EXP (-((SQRT (RADSQ) - DEFMIN) * SIGMA) ** 2)
               BUFFR3(I3) = (1.0-WW) * BUFFR1(I1) +
     *            WW * NORM2 * BUFFR2(I2)
               BUFFR3(I3+1) = (1.0-WW) * BUFFR1(I1+1) +
     *            WW * NORM2 * BUFFR2(I2+1)
            ELSE
               WW = EXP (-((SQRT (RADSQ) - DEFMAX) * SIGMA) ** 2)
               BUFFR3(I3) = WW * BUFFR1(I1) +
     *            (1.0-WW) * NORM2 * BUFFR2(I2)
               BUFFR3(I3+1) = WW * BUFFR1(I1+1) +
     *            (1.0-WW) * NORM2 * BUFFR2(I2+1)
               END IF
            I1 = I1 + 2
            I2 = I2 + 2
            I3 = I3 + 2
 130        CONTINUE
         DO 132 IX = NY/2+2,NY
            RADSQ = USQ + CELVSQ*(NY+1-IX)**2
C                                       inner
            IF (RADSQ.LT.UVMNSQ) THEN
               BUFFR3(I3) = NORM2*BUFFR2(I2)
               BUFFR3(I3+1) = NORM2*BUFFR2(I2+1)
C                                       outer
            ELSE IF (RADSQ.GT.UVMXSQ) THEN
               BUFFR3(I3) = BUFFR1(I1)
               BUFFR3(I3+1) = BUFFR1(I1+1)
C                                       average MEAN
            ELSE IF (STYPE.EQ.'MEAN') THEN
               BUFFR3(I3) = RFACTH * (BUFFR1(I1) + NORM2*BUFFR2(I2))
               BUFFR3(I3+1) = RFACTH*(BUFFR1(I1+1)
     *            + NORM2*BUFFR2(I2+1))
C                                       feather
            ELSE IF (RADSQ.LT.UVAVSQ) THEN
               WW = EXP (-((SQRT (RADSQ) - DEFMIN) * SIGMA) ** 2)
               BUFFR3(I3) = (1.0-WW) * BUFFR1(I1) +
     *            WW * NORM2 * BUFFR2(I2)
               BUFFR3(I3+1) = (1.0-WW) * BUFFR1(I1+1) +
     *            WW * NORM2 * BUFFR2(I2+1)
            ELSE
               WW = EXP (-((SQRT (RADSQ) - DEFMAX) * SIGMA) ** 2)
               BUFFR3(I3) = WW * BUFFR1(I1) +
     *            (1.0-WW) * NORM2 * BUFFR2(I2)
               BUFFR3(I3+1) = WW * BUFFR1(I1+1) +
     *            (1.0-WW) * NORM2 * BUFFR2(I2+1)
               END IF
            I1 = I1 + 2
            I2 = I2 + 2
            I3 = I3 + 2
 132        CONTINUE
 135     CONTINUE
      IF (AKCLOS (WK1, BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (WK2, BUFFR2).NE.0) GO TO 990
      IF (AKCLOS (WK3, BUFFR3).NE.0) GO TO 990
C                                        FFT back to map plane
      IDIR = -1
C
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'VMERGE(3): FAILED TO GET ANY AP MEMORY'
         CALL MSGWRT (8)
         IERR = 10
         GO TO 999
         END IF
C
      CALL MYFFT (NX, NY, IDIR, T, WK3, WK1, OUT, BUFSZ(1), BUFFR1,
     *   BUFFR2, RESMAX, RESMIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1180) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IMGMAX = RESMAX
      IMGMIN = RESMIN
C
      CALL QRLSE
C
      GO TO 999
C
 990  WRITE (MSGTXT,1010)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VMERGE: ERROR ',I3,' IN MYFFT MAP TO U,V')
 1020 FORMAT ('VMERGE: ERROR ',I3,' IN MULGAU')
 1010 FORMAT ('VMERGE')
 1180 FORMAT ('VMERGE: ERROR ',I3,' IN MYFFT U,V TO MAP')
 2000 FORMAT ('NORM. FACTOR FOR IMAGE 2 IS:',1PE12.5)
      END
      SUBROUTINE MULGAU (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   MULGAU multiplies a u,v image by an inverse Gaussian function
C   Programmer =  R. Braun      Jan 1988
C-----------------------------------------------------------------------
      INTEGER   IN, OUT, JLIM
      REAL      RFACT, RBMAJ, RBMIN, U, V, R, RMAX, COSPA, SINPA, SBMAJ,
     *   SBMIN, S, COSSA, SINSA
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INTEGER   IU, IV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      RMAX = 30.0
      RFACT = 4.0*(ATAN (1.0))**2 / LOG (2.0)
      COSPA = COS (1.745329E-2 * (BPA+90.0))
      SINPA = SIN (1.745329E-2 * (BPA+90.0))
      RBMAJ = RFACT*(BMAJ / (CELLX*REAL(NX)))**2
      RBMIN = RFACT*(BMIN / (CELLX*REAL(NX)))**2
      COSSA = COS (1.745329E-2 * (BMSIZE(3)+90.0))
      SINSA = SIN (1.745329E-2 * (BMSIZE(3)+90.0))
      SBMAJ = RFACT*(BMSIZE(1) / (CELLX*REAL(NX)))**2
      SBMIN = RFACT*(BMSIZE(2) / (CELLX*REAL(NX)))**2
C
      IF (AKOPEN (IN, 1, 'READ', BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (OUT, 2, 'WRIT',BUFFR2).NE.0) GO TO 990
C                                       Begin loop thru map.
      JLIM = NX / 2 + 1
      DO 125 IU = 1,JLIM
         IF (AKCESS (IN, BUFFR1).NE.0) GO TO 990
         IF (AKCESS (OUT, BUFFR2).NE.0) GO TO 990
         I1 = BIND(1)
         I2 = BIND(2)
      INCLUDE 'INCS:ZVND.INC'
         DO 120 IV = 1,NY/2
            U = - SINPA * REAL(IV-1) + COSPA * REAL(IU-1)
            V =   COSPA * REAL(IV-1) + SINPA * REAL(IU-1)
            R = RBMAJ*U**2 + RBMIN*V**2
            U = - SINSA * REAL(IV-1) + COSSA * REAL(IU-1)
            V =   COSSA * REAL(IV-1) + SINSA * REAL(IU-1)
            S = SBMAJ*U**2 + SBMIN*V**2
            R = R - S
            IF (R.GT.RMAX) THEN
               BUFFR2(I2) = 0.0
               BUFFR2(I2+1) = 0.0
            ELSE
               BUFFR2(I2)   = BUFFR1(I1) / EXP(-R)
               BUFFR2(I2+1)   = BUFFR1(I1+1) / EXP(-R)
               END IF
            I1 = I1 + 2
            I2 = I2 + 2
 120        CONTINUE
      INCLUDE 'INCS:ZVND.INC'
         DO 121 IV = NY/2+1, NY
            U = - SINPA * REAL(IV-NY) + COSPA * REAL(IU-1)
            V =   COSPA * REAL(IV-NY) + SINPA * REAL(IU-1)
            R = RBMAJ*U**2 + RBMIN*V**2
            U = - SINSA * REAL(IV-NY) + COSSA * REAL(IU-1)
            V =   COSSA * REAL(IV-NY) + SINSA * REAL(IU-1)
            S = SBMAJ*U**2 + SBMIN*V**2
            R = R - S
            IF (R.GT.RMAX) THEN
               BUFFR2(I2) = 0.0
               BUFFR2(I2+1) = 0.0
            ELSE
               BUFFR2(I2)   = BUFFR1(I1) / EXP(-R)
               BUFFR2(I2+1)   = BUFFR1(I1+1) / EXP(-R)
            END IF
            I1 = I1 + 2
            I2 = I2 + 2
 121        CONTINUE
 125     CONTINUE
      IF (AKCLOS (IN, BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (OUT, BUFFR2).NE.0) GO TO 990
C
      GO TO 999
C
 990  WRITE (MSGTXT,1010)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MULGAU')
      END
      SUBROUTINE MYFFT (NR, NC, IDIR, HERM, LI, LW, LO, JBUFSZ, XBUFF1,
     *   XBUFF2, SMAX, SMIN, IERR)
C-----------------------------------------------------------------------
C    MYFFT is a disk based, two dimensional FFT.  If the FFT all fits
C    in AP memory then the intermediate result is not written to disk.
C    Input or output images in the sky plane are in the usual form
C    (i.e. center at the center, X the first axis).  Input or output
C    images in the uv plane are transposed (v the first axis) and the
C    center-at-the-edges convention with the first element of the array
C    the center pixel.
C    NOTE: Uses AIPS LUNs 23, 24, 25.
C   Inputs:
C     NR     I    The number of rows in input array (# columns in
C                 output).  When HERM is TRUE and IDIR=-1, NR is twice
C                 the number of complex rows in the input file.
C     NC     I    The number of columns in input array
C                 (# rows in output).
C     IDIR   I    1 for forward (+i) transform, -1 for inverse (-i)
C                 transform.
C                 If HERM = .TRUE. the follwing are recognized:
C                   IDIR=1 keep real part only.
C                   IDIR=2 keep amplitudes only.
C                   IDIR=3 keep full complex (half plane)
C     HERM   L    When HERM = .FALSE., this routine does a complex to
C                 complex transform.
C                 When HERM = .TRUE. and IDIR = -1, it does a
C                 complex to real transform.  When HERM = .TRUE. and
C                 IDIR = 1, it does real to complex.
C     LI     I    File number in VMFILES of input.
C     LW     I    File number in VMFILES of work file (may equal LI).
C     LO     I    File number in VMFILES of output.
C     JBUFSZ I    Size of BUFF1, BUFF2 in bytes.  Should be large
C                 at least 4096 R   words.
C    Output:
C     XBUFF1 R    Working buffer
C     XBUFF2 R    Working buffer
C     SMAX   R    For HERM=.TRUE. the maximum value in the output file.
C     SMIN   R    For HERM=.TRUE. the minimum value in the output file.
C     IERR   I    Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER FIL(3)*48
      INTEGER   NR, NC, IDIR, LI, LW, LO, JBUFSZ,
     *   LUN(3), VOL(3), BO(3), MC, MR
      LOGICAL   HERM, FULL
      REAL     XBUFF1(1), XBUFF2(1), SMAX, SMIN
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA BO /1, 1, 1/
      DATA LUN /23,24,25/
C-----------------------------------------------------------------------
C                                       Fill arrays for PASS1/PASS2
      FULL = .NOT. HERM
      VOL(1) = VMVOL(LI)
      VOL(2) = VMVOL(LW)
      VOL(3) = VMVOL(LO)
      FIL(1) = VMFILE(LI)
      FIL(2) = VMFILE(LW)
      FIL(3) = VMFILE(LO)
      MC = NC
      MR = NR
      IF (FULL) MC = NR
      IF (FULL) MR = NC
C                                       Do FFT.
      CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, XBUFF1, JBUFSZ,
     *   XBUFF2, JBUFSZ, NR, NC, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, XBUFF1, JBUFSZ,
     *   XBUFF2, JBUFSZ, MR, MC, SMAX, SMIN, IERR)
C
 999  RETURN
      END
      SUBROUTINE VMHIS (NAME, VOL, CLASS, SEQ, IERR)
C-----------------------------------------------------------------------
C   VMHIS copies the dirty map history if the VM map does not
C   already have a history file.  Then the inputs to VM with
C   the default values are added to the history file.
C   Programmer =  T.J. Cornwell      September 1986
C-----------------------------------------------------------------------
      CHARACTER NAME(4)*12, CLASS(4)*6, HILINE*72
      INTEGER   VOL(4), SEQ(4), CATBLK(256), LUN1, LUN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'IMERG.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /MAPHDR/ CATBLK
C-----------------------------------------------------------------------
C                                       copy keywords partly
      CALL KEYPCP (VOL(1), FCNO(1), VOL(3), FCNO(3), 0, ' ', IERR)
C                                       Copy/open history files.
      LUN1 = 16
      LUN2 = 17
      CALL HISCOP (LUN2, LUN1, VOL(1), VOL(3), FCNO(1), FCNO(3), CATBLK,
     *   BUFFR1, BUFFR2, IERR)
C                                       Check if old file doesn't exist
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
         GO TO 60
         END IF
C                                       Add to history file.
C                                       High res. map name.
      WRITE (MSGTXT,2000) TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1)
      CALL MSGWRT (3)
      CALL HENCO1 (TSKNAM, NAME(1), CLASS(1), SEQ(1), VOL(1),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Low res. map name.
      WRITE (MSGTXT,2001) TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2)
      CALL MSGWRT (3)
      CALL HENCO2 (TSKNAM, NAME(2), CLASS(2), SEQ(2), VOL(2),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Merged map name.
      WRITE (MSGTXT,2002) TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3)
      CALL MSGWRT (3)
      CALL HENCOO (TSKNAM, NAME(3), CLASS(3), SEQ(3), VOL(3),
     *   LUN1, BUFFR2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2009) TSKNAM, BLC(1), BLC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Boxes
      WRITE (HILINE,2010) TSKNAM, TRC(1),TRC(2)
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       UVrange
      WRITE (HILINE,2011) TSKNAM, DEFMIN, DEFMAX
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       Norm. factor
      WRITE (HILINE,2012) TSKNAM, SCLF
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
C                                       CHAN
      WRITE (HILINE,2177) TSKNAM, CHAN
      CALL HIADD (LUN1, HILINE, BUFFR2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (2)
      IF (IERR.NE.0) GO TO 50
      GO TO 60
C                                       Error has occured.
 50   WRITE (MSGTXT,1050) IERR
      CALL MSGWRT (6)
C                                       Close history files.
 60   CALL HICLOS (LUN1, T, BUFFR2, IERR)
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('VMHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT ('VMHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,'HI-RES MAP = ''',A12,''' . ''',A6,''' . ',2I4)
 2001 FORMAT (A6,'LO-RES MAP = ''',A12,''' . ''',A6,''' . ',2I4)
 2002 FORMAT (A6,'IMERG MAP  = ''',A12,''' . ''',A6,''' . ',2I4)
 2009 FORMAT (A6,'BLC = ',2(I5,1X),' /BOTTOM LEFT CORNER')
 2010 FORMAT (A6,'TRC = ',2(I5,1X),' /TOP   RIGHT CORNER')
 2011 FORMAT (A6,'UVMIN,UVMAX = ',2(1PE12.4,1X),' /OF NORM. ANNULUS')
 2012 FORMAT (A6,'FACTOR = ',1PE12.4,' /NORM. OF LOW RES.')
 2177 FORMAT (A6,'BLC(3) = ',I6,' /CHANNEL NUMBER')
      END
