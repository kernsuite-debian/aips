LOCAL INCLUDE 'PANELP'
      INTEGER   MXX
      PARAMETER (MXX=256)
LOCAL END
LOCAL INCLUDE 'PANEL.INC'
      INCLUDE 'INCS:PMAD.INC'
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XOUTF(12),
     *   XINLST(12)
      REAL      XSEQ1, XDISK1, XSEQ2, XDISK2, BLC(7), TRC(7), XINC,
     *   YINC, XDOPLT, APARM(10), XDOTV, XCHAN, XGRCH
      COMMON /INPARM/ XNAME1, XCLAS1, XSEQ1, XDISK1, XNAME2, XCLAS2,
     *   XSEQ2, XDISK2, BLC, TRC, XINC, YINC, XOUTF, XDOPLT, APARM,
     *   XINLST, XDOTV, XCHAN, XGRCH
      INTEGER   SEQ1, CNO1, DISK1, SEQ2, CNO2, DISK2, SCRTCH(256),
     *   JBUFSZ, TXLUN, TXIND, IDOPLT, NPARMS, CATOLD(256,2)
      CHARACTER NAME1*12, CLASS1*6, NAME2*12, CLASS2*6, OUTFIL*48,
     *   OPTSTR*32, INLIST*48
      REAL      BUFF1(MABFSS)
      COMMON /PARMS/ CATOLD, BUFF1, SCRTCH, SEQ1, CNO1, DISK1, SEQ2,
     *   CNO2, DISK2, JBUFSZ, TXLUN, TXIND, IDOPLT, NPARMS
      COMMON /CPARMS/ NAME1, CLASS1, NAME2, CLASS2, OUTFIL, OPTSTR,
     *   INLIST
LOCAL END
LOCAL INCLUDE 'PANELDATA'
      REAL      MAPSZ, CELLSZ, FOCUS, RESN, RAD(2,10), TR(6), C(21),
     *   NODX(4,100,10), NODY(4,100,10), RMS, PAR(4,100,10)
      INTEGER   NR, NP(10), OPT, NC
      LOGICAL   PLTOFF
      COMMON /PDATA/ MAPSZ, CELLSZ, FOCUS, RESN, RAD, TR, C, NODX, NODY,
     *   RMS, PAR, NP, NR, OPT, NC, PLTOFF
LOCAL END
LOCAL INCLUDE 'PANELPLOT'
      INTEGER   CATSAV(256), PLBUF(256), GLUN, GFIND, TVCHN, GRCHN
      REAL      CH(4)
      COMMON /PLDATA/ CATSAV, PLBUF, CH, GLUN, GFIND, TVCHN, GRCHN
LOCAL END
      PROGRAM PANEL
C-----------------------------------------------------------------------
C! Process holography images from HOLOG/HOLGR to panel adjustments
C# UV ANALYSIS VLA
C-----------------------------------------------------------------------
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
C   Determine the panel adjustments given a surface deviation map
C   Variables list
C      VDEV     Array of panel deviations, in meters or degrees
C      VMAP     Array of shadowed parts of antenna
C      HIT      Array used in locating panel points
C      RESN     resolution of p.s. function
C      MAPSZ    Physical size of antenna image (m)
C      FOCUS    Antenna focal length
C      MGAIN    Antenna gain with input deviation map
C      MXGN     Antenna gain with no surface errors
C      CELLSZ   Cellsize in meters
C      GAIN     Array of estimated gains
C   Discarded
C      LEGW     Width of antenna feed leg (m)
C      LEGR     Radial distance of feed leg base from axis
C      VTXHT    Height of vertex from bottom of antenna
C   Now in APARM
C      NPT      Number of observed offsets in one row
C      PHSMAP   Is .TRUE. if input map is in phase, otherwise .false.
C      FREQ     Frequency in GHz
C   The panel layout data is in DATA statements in the init routine
C      NR          Number of rings of panels
C      NP(I)       Number of panels in ring 'I'
C      RAD(1,J)    Inner radius (m) of ring 'J'
C      RAD(2,J)    Outer radius (m) of ring 'J'
C-----------------------------------------------------------------------
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
      INCLUDE 'PANELP'
C
      CHARACTER PRGNAM*6
      INTEGER   IRET, MX, VMAP(MXX,MXX)
      REAL      RMS0, LAMBDA, MGAIN, MXGN, GAIN(3,5), VDEV(MXX,MXX),
     *   NDEV(MXX,MXX)
      LOGICAL   HIT(MXX,MXX)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DATA PRGNAM /'PANEL'/
C-----------------------------------------------------------------------
      MX = MXX
      CALL PANELI (PRGNAM, MX, VDEV, VMAP, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       initial rms
      CALL GETRMS (MX, VDEV, VMAP, RMS)
      WRITE (MSGTXT,1010) 'Initial', RMS
      CALL MSGWRT (4)
      RMS0 = RMS
C                                       Compute the current and maximum
C                                       power gains
      LAMBDA = 35.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(1,1) = MGAIN
      GAIN(3,1) = MXGN
      LAMBDA = 20.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(1,2) = MGAIN
      GAIN(3,2) = MXGN
      LAMBDA = 13.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(1,3) = MGAIN
      GAIN(3,3) = MXGN
      LAMBDA = 7.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(1,4) = MGAIN
      GAIN(3,4) = MXGN
      LAMBDA = 3.4
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(1,5) = MGAIN
      GAIN(3,5) = MXGN
C                                       show the raw data ?
      CALL DOPLOT (1, MX, VDEV, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       do the work
      CALL PANELD (MX, VDEV, VMAP, NDEV, HIT, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Plot adjustment map
      CALL DOPLOT (2, MX, NDEV, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Final rms
      CALL GETRMS (MX, VDEV, VMAP, RMS)
      WRITE (MSGTXT,1010) 'Final', RMS
      CALL MSGWRT (4)
C                                       Compute the expected gain after
C                                       panel adjustment
      LAMBDA = 35.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(2,1) = MGAIN
      LAMBDA = 20.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(2,2) = MGAIN
      LAMBDA = 13.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(2,3) = MGAIN
      LAMBDA = 7.
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(2,4) = MGAIN
      LAMBDA = 3.4
      CALL ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
      GAIN(2,5) = MGAIN
C                                       Plot the residual map
      CALL DOPLOT (3, MX, VDEV, IRET)
      CALL REPORT (RMS0, GAIN)
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  IF (TXIND.GT.0) CALL ZTXCLS (TXLUN, TXIND, MX)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT (A,' rms',F8.2,' mm')
      END
      SUBROUTINE PANELI (PRGNAM, MX, VDEV, VMAP, IRET)
C-----------------------------------------------------------------------
C   PANELI gets the input adverbs and reads in the image
C   Inputs:
C      PRGNAM   C*6    program name
C   Outputs:
C      MX       I      dimension of images
C      VDEV     R(*)   input image
C      VMAP     I(*)   blanking image
C      IRET     I      > 0 => error -> quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   MX, VMAP(*), IRET
      REAL      VDEV(*)
C
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
C
      INTEGER   IERR, MLUN, MIND, DX, DY, IY, IX, NY, NX, IWIN(4),
     *   IDEPTH(5), I, J, IDATE(3), ITIME(3), JTRIM, IBLKOF, IBIND,
     *   IROUND, LOCS, KEYTYP, MSGSAV, LL, KK
      CHARACTER MTYPE*2, LINE*64, ATIME*8, ADATE*12, STROPT(3)*32,
     *   CUNITS*8, KEYWRD*8
      REAL      CONST, X, Y, RF, SEC
      DOUBLE PRECISION FREQ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C                               rigid panel, displaced normal to surface
C                               rigid panel, lifted and tilted
C                               flexible panel.
      DATA STROPT /'Rigid panel, shift only',
     *             'Rigid panel, tilt+shift',
     *             'Flexible panel'/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      NPARMS = 68
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAME1, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
      RQUICK = .FALSE.
C
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAME1, NAME1)
      CALL H2CHR (6, 1, XCLAS1, CLASS1)
      SEQ1 = XSEQ1 + 0.01
      DISK1 = XDISK1 + 0.01
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLASS2)
      SEQ2 = XSEQ2 + 0.01
      DISK2 = XDISK2 + 0.01
      CALL H2CHR (48, 1, XOUTF, OUTFIL)
      IF (OUTFIL.EQ.' ') OUTFIL = 'HOME:panel.adj'
      CALL H2CHR (48, 1, XINLST, INLIST)
      IF (XDOPLT.EQ.0.0) XDOPLT = 7.0
      IDOPLT = IROUND (XDOPLT)
C                                       get the mask data
      MLUN = 21
      MTYPE = 'MA'
      CALL MAPOPN ('READ', DISK2, NAME2, CLASS2, SEQ2, MTYPE, NLUSER,
     *   MLUN, MIND, CNO2, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING MASK IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = CNO2
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATOLD(1,2))
C                                       Set default values BLC, TRC.
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SETTING WINDOW INTO MASK IMAGE'
         GO TO 990
         END IF
      DO 10 I = 1,5
         IDEPTH(I) = BLC(I+2) + 0.1
 10      CONTINUE
      IWIN(1) = BLC(1) + 0.1
      IWIN(2) = BLC(2) + 0.1
      IWIN(3) = TRC(1) + 0.1
      IWIN(4) = TRC(2) + 0.1
      DX = XINC + 0.1
      DX = MAX (1, DX)
      XINC = DX
      DY = YINC + 0.1
      DY = MAX (1, DY)
      YINC = DY
      IX = (IWIN(3) - IWIN(1)) / DX + 1
      IY = (IWIN(4) - IWIN(2)) / DY + 1
      IF ((IX.NE.IY) .OR. ((IX.NE.128) .AND. (IX.NE.256))) THEN
         WRITE (MSGTXT,1010) IX, IY
         GO TO 990
         END IF
      MX = IX
      NX = IWIN(3) - IWIN(1) + 1
      NY = IWIN(4) - IWIN(2) + 1
      JBUFSZ = 2 * MABFSS
C                                       Initialize for double buffering
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, IBLKOF, IERR)
      IBLKOF = IBLKOF + 1
      CALL MINIT ('READ', MLUN, MIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF1, JBUFSZ, IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT I/O FOR MASK'
         GO TO 990
         END IF
C                                       read in mask image
      LL = 0
      DO 30 IY = IWIN(2),IWIN(4)
         CALL MDISK ('READ', MLUN, MIND, BUFF1, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING MASK IMAGE'
            GO TO 990
            END IF
         IF (MOD(IY-IWIN(2),DY).EQ.0) THEN
            DO 20 IX = IWIN(1),IWIN(3),DX
               LL = LL + 1
               VDEV(LL) = BUFF1(IBIND+IX-IWIN(1))
 20            CONTINUE
            END IF
 30      CONTINUE
C                                       close
      CALL MAPCLS ('READ', DISK2, CNO2, MLUN, MIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING MASK IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE - 1
C                                       make mask
      LL = 0
      DO 40 J = 1,MX
         DO 35 I = 1,MX
            LL = LL + 1
            IF (VDEV(LL).EQ.FBLANK) THEN
               VMAP(LL) = 0
            ELSE IF (VDEV(LL).LE.APARM(2)) THEN
               VMAP(LL) = 0
            ELSE
               VMAP(LL) = 1
               END IF
 35         CONTINUE
 40      CONTINUE
C                                       get the image data
      CALL MAPOPN ('READ', DISK1, NAME1, CLASS1, SEQ1, MTYPE, NLUSER,
     *   MLUN, MIND, CNO1, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING MASK IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATOLD(1,1))
C                                       INIT i/o
      CALL MINIT ('READ', MLUN, MIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF1, JBUFSZ, IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT I/O FOR IMAGE DATA'
         GO TO 990
         END IF
C                                       read in data image
      ll = 0
      DO 60 IY = IWIN(2),IWIN(4)
         CALL MDISK ('READ', MLUN, MIND, BUFF1, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING IMAGE DATA'
            GO TO 990
            END IF
         IF (MOD(IY-IWIN(2),DY).EQ.0) THEN
            DO 50 IX = IWIN(1),IWIN(3),DX
               LL = LL + 1
               VDEV(LL) = BUFF1(IBIND+IX-IWIN(1))
               IF (VDEV(LL).EQ.FBLANK) VMAP(LL) = 0
               IF (VMAP(LL).LE.0) VDEV(LL) = 0.0
C                                       convert to mm
               VDEV(LL) = VDEV(LL) * 1000.0
 50            CONTINUE
            END IF
 60      CONTINUE
C                                       close
      CALL MAPCLS ('READ', DISK1, CNO1, MLUN, MIND, CATBLK, .FALSE.,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING IMAGE DATA FILE'
         GO TO 990
         END IF
      NCFILE = NCFILE - 1
C                                       panel parameters
      CALL GETPAN (INLIST, NR, NP, RAD, FOCUS)
      CALL SETNOD (NR, NP, RAD, NODX, NODY)
C                                       Convert to meters
      MAPSZ = 0.0
      CALL AXEFND (4, 'X   ', CATBLK(KIDIM), CATH(KHCTP), I, IERR)
      IF (IERR.EQ.0) THEN
         MAPSZ = CATBLK(KINAX+I) * ABS (CATR(KRCIC+I))
      ELSE
         MSGTXT = 'X axis not found, using APARM(3)'
         CALL MSGWRT (6)
         MAPSZ = APARM(3)
         END IF
      IF (MAPSZ.LE.0.0) THEN
         MSGTXT = 'MAPSIZE IN METERS IS REQUIRED'
         GO TO 990
         END IF
C                                       number samples
      KEYWRD = 'VisUsed'
      I = 1
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CATKEY ('READ', DISK1, CNO1, KEYWRD, I, LOCS, IX, KEYTYP,
     *   SCRTCH, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         APARM(5) = IX
         APARM(5) = SQRT (APARM(5))
      ELSE
         MSGTXT = 'APARM(5) is being used since VisUsed not present'
         CALL MSGWRT (7)
         END IF
      APARM(5) = MAX (1.0, APARM(5))
      RESN = MAPSZ / APARM(5)
      CELLSZ = MAPSZ / MX
      CALL H2CHR (8, 1, CATH(KHBUN), CUNITS)
      CALL CHLTOU (8, CUNITS)
      IF (CUNITS(:6).EQ.'DEGREE') THEN
         FREQ = 0.0D0
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), I, IERR)
         IF (IERR.EQ.0) THEN
            FREQ = CATD(KDCRV+I)
         ELSE
            MSGTXT = 'FREQ axis not found, using APARM(4)'
            CALL MSGWRT (6)
            FREQ = APARM(4)
            END IF
         IF (FREQ.LE.0.0D0) THEN
            MSGTXT = 'FREQUENCY IS REQUIRED FOR PHASE IMAGES'
            GO TO 990
            END IF
         CONST = 0.3 / 720.0 / FREQ
         LL = 0
         KK = MX / 2 + 1
         DO 70 J = 1,MX
            Y = KK - J
            DO 65 I = 1,MX
               LL = LL + 1
               X = KK - I
               RF = CELLSZ * SQRT (X**2+Y**2) / (2.*FOCUS)
               SEC = SQRT (1.+RF*RF)
               VDEV(LL) = VDEV(LL) * CONST * SEC
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       open the output file
      TXLUN = 11
      CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTFIL, .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT TEXT FILE'
         TXIND = 0
         GO TO 990
         END IF
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
      LINE = ' Processed : ' // ADATE // ATIME
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITE TEXT FILE HEADER'
         GO TO 990
         END IF
      OPT = APARM(1) + 0.1
      IF ((OPT.LE.0) .OR. (OPT.GT.3)) OPT = 1
      APARM(1) = OPT
      OPTSTR = STROPT(OPT)
      LINE = ' Panel fitting option : ' // OPTSTR
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITE TEXT FILE HEADER'
         GO TO 990
         END IF
C                                       plot parameters
      PLTOFF = .TRUE.
C                                       the array-space mapping
      TR(2) = MAPSZ/MX
      TR(6) = TR(2)
      TR(1) = -MAPSZ/2. - TR(2)
      TR(4) = TR(1)
      TR(3) = 0.
      TR(5) = 0.
C                                       the contour levels (in mm)
C                                       (Contours every 0.5 mm)
      NC = 21
      DO 80 I = 1,NC
         C(I) = (I-11)/2. - 0.25
 80      CONTINUE
C
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PANELI ERROR',I4,' ON ',A)
 1010 FORMAT ('PANELI REQUESTED IMAGE SIZE',2I5,
     *   ' NOT SQUARE 128 OR 256')
      END
      SUBROUTINE GETPAN (INLIST, NR, NP, RAD, FOCUS)
C-----------------------------------------------------------------------
C   Inputs:
C      INLIST   C*(*)    Input text file
C   Outputs:
C      NR       I        Number rings
C      NP       I(*)     Number panels in each ring
C      RAD      R(2,*)   Inner and outer radius each ring
C      FOCUS    R        Focal length
C-----------------------------------------------------------------------
      CHARACTER INLIST*(*)
      INTEGER   NR, NP(*)
      REAL      RAD(2,*), FOCUS
C
      INTEGER   NRADS
      PARAMETER (NRADS=6)
C
      INTEGER   LUN, IND, NPL(NRADS), IRET, KBPLIM, KBP, JTRIM
      CHARACTER LINE*132
      DOUBLE PRECISION X
      REAL      RPL(2,NRADS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NPL /12, 16, 24, 40, 40, 40/
      DATA RPL /1.983,3.683, 3.683,5.563, 5.563,7.391, 7.391,9.144,
     *   9.144,10.87, 10.87,12.5/
C-----------------------------------------------------------------------
      IRET = -1
      FOCUS = -1.0
      NR = 0
      IF (INLIST.NE.' ') THEN
         LUN = 11
         CALL ZTXOPN ('READ', LUN, IND, INLIST, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', INLIST
            GO TO 90
            END IF
C                                       Read focus
 10      CALL ZTXIO ('READ', LUN, IND, LINE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', INLIST
            GO TO 80
         ELSE
            IF ((LINE.NE.' ') .AND. (LINE(:1).NE.'#') .AND.
     *         (LINE(:1).NE.';')) THEN
               KBP = 1
               KBPLIM = JTRIM (LINE)
               CALL GETNUM (LINE, KBPLIM, KBP, X)
               IF (X.NE.DBLANK) FOCUS = X
               END IF
            IF (FOCUS.LE.0.0) GO TO 10
            END IF
C                                       Read panel data
 20      CALL ZTXIO ('READ', LUN, IND, LINE, IRET)
         IF (IRET.EQ.2) THEN
            IRET = 0
            GO TO 80
         ELSE IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', INLIST
            GO TO 80
         ELSE
            IF ((LINE.NE.' ') .AND. (LINE(:1).NE.'#') .AND.
     *         (LINE(:1).NE.';')) THEN
               NR = NR + 1
               KBP = 1
               KBPLIM = JTRIM (LINE)
               CALL GETNUM (LINE, KBPLIM, KBP, X)
               IF (X.NE.DBLANK) THEN
                  NP(NR) = X + 0.1
                  CALL GETNUM (LINE, KBPLIM, KBP, X)
                  IF (X.NE.DBLANK) THEN
                     RAD(1,NR) = X
                     CALL GETNUM (LINE, KBPLIM, KBP, X)
                     IF (X.NE.DBLANK) RAD(2,NR) = X
                     END IF
                  END IF
               IF (X.EQ.DBLANK) NR = NR - 1
               GO TO 20
               END IF
            END IF
C                                       close text file
 80      CALL ZTXCLS (LUN, IND, KBP)
C
 90      IF (IRET.NE.0) CALL MSGWRT (8)
         END IF
C                                       use VLA on ' ' and error
      IF ((IRET.NE.0) .OR. (FOCUS.LE.0.0) .OR. (NR.LE.0)) THEN
         NR = NRADS
         CALL COPY (NR, NPL, NP)
         CALL RCOPY (2*NR, RPL, RAD)
         FOCUS = 8.8
         MSGTXT = 'GETPAN: using VLA panle structure and focus'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETPAN ERROR',I3,' ON ',A,' ',A)
      END
      SUBROUTINE PANELD (MX, VDEV, VMAP, NDEV, HIT, IRET)
C-----------------------------------------------------------------------
C   does main body of computation
C   examine each panel
C      To simplify matters, and yet keep track of the fact that a
C      rectangular grid sits uncomfortably with a circular antenna, we
C      proceed in 4 steps:
C      a. set up a regular (fine) grid on a reference panel oriented
C         along the y-axis
C      b. transform ah point on the grid to the actual panel
C      c. locate the nearest actual sample point
C      d.  transform back to the reference panel.
C   Inputs
C      MX     I      X/Y dimension of arrays
C      VDEV   R(*)   Data array
C      VMAP   I(*)   Mask arrays
C   Outputs
C      NDEV   R(*)   Count of samples hitting pixel
C      HIT    L(*)   work area to mark points used
C-----------------------------------------------------------------------
      INTEGER   MX, VMAP(MX,*), IRET
      REAL      VDEV(MX,*), NDEV(MX,*)
      LOGICAL   HIT(MX,*)
C
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
      INTEGER   I, J, NV, IT, JT, IP, IR
      REAL      X, Y, TH1, TH2, FI, X1, X2, Y1, Y2, INC, ZETA, XX, FIT,
     *   V(3,500), XI, YI, FI1, XT, YT, RT, MN
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       set fine enough to locate
C                                       all the points
      INC = 15.0 / MX
      DO 15 I = 1,MX
         DO 10 J = 1,MX
            HIT(J,I) = VMAP(J,I).GT.0
 10         CONTINUE
 15      CONTINUE
C                                       loop over rings
      DO 50 IR = 1,NR
         FI = TWOPI / NP(IR)
         X1 = RAD(1,IR) * SIN (FI/2.0)
         X2 = RAD(2,IR) * SIN (FI/2.0)
         Y1 = RAD(1,IR) * COS (FI/2.0)
         Y2 = RAD(2,IR) * COS (FI/2.0)
C                                       loop over panels
         DO 40 IP = 1,NP(IR)
            IF (IP.EQ.61) THEN
                I = 99
                END IF
            TH1 = (IP-1) * TWOPI / NP(IR)
            TH2 = IP * TWOPI / NP(IR)
            ZETA = (IP-0.5) * FI
            NV = 0
            Y = Y1
 20         IF (Y.LE.Y2) THEN
               XX = X1 + (X2-X1) * (Y-Y1) / (Y2-Y1)
               X = -XX
 30            IF (X.LE.XX) THEN
                  FI1 = ATAN2 (X, Y)
                  FIT = FI1 + ZETA
                  RT = SQRT (X**2 + Y**2)
                  XT = RT * SIN (FIT)
                  YT = RT * COS (FIT)
                  JT = NINT ((XT - TR(1))/TR(2))
                  IT = NINT ((YT - TR(4))/TR(6))
C                                       checks :
C                                       a. is this point truly
C                                          within the panel?
                  XT = TR(1) + TR(2) * JT
                  YT = TR(4) + TR(6) * IT
                  RT = SQRT (XT**2 + YT**2)
                  FIT = ATAN2 (XT, YT)
                  IF (FIT.LT.0.) FIT = FIT + TWOPI
C                                       b.  have we used this point
                  IF ((HIT(JT,IT)) .AND. (FIT.GE.TH1) .AND. (FIT.LE.TH2)
     *               .AND.  (RT.GE.RAD(1,IR)) .AND. (RT.LE.RAD(2,IR)))
     *               THEN
                     NV = NV + 1
C                                       set up the parameters for the
C                                       panel solution -- locate each
C                                       point on the reference panel
                     FI1 = FIT - ZETA
                     V(1,NV) = RT * SIN (FI1)
                     V(2,NV) = RT * COS (FI1) - Y1
                     V(3,NV) = VDEV(JT,IT)
                     HIT(JT,IT) = .FALSE.
C                                       ignore if blanked
                     IF ((VMAP(JT,IT).LE.0) .OR. (VDEV(JT,IT).EQ.0.0))
     *                  NV = NV - 1
                     END IF
                  X = X + INC
                  GO TO 30
                  END IF
               Y = Y + INC
               GO TO 20
               END IF
C                                       work out panel adjustment
            IF (NV.GT.0) THEN
               CALL SOLVEP (OPT, IR, IP, NV, V, PAR, X1, X2, Y2, IRET)
               IF (IRET.NE.0) THEN
                  I = 1
                  GO TO 999
                  END IF
            ELSE
               CALL RFILL (4, 0.0, PAR(1,IP,IR))
               END IF
 40         CONTINUE
 50     CONTINUE
C                                       generate the new surface map

      DO 115 I = 1,MX
         DO 110 J = 1,MX
            HIT(J,I) = VMAP(J,I).GT.0
 110        CONTINUE
 115     CONTINUE
C                                       loop over rings
      DO 150 IR = 1,NR
         FI = TWOPI / NP(IR)
         X1 = RAD(1,IR) * SIN (FI/2.0)
         X2 = RAD(2,IR) * SIN (FI/2.0)
         Y1 = RAD(1,IR) * COS (FI/2.0)
         Y2 = RAD(2,IR) * COS (FI/2.0)
C                                       loop over panels
         DO 140 IP = 1,NP(IR)
            TH1 = (IP-1) * TWOPI / NP(IR)
            TH2 = IP * TWOPI / NP(IR)
            ZETA = (IP-0.5) * FI
            Y = Y1
 120        IF (Y.LE.Y2) THEN
               XX = X1 + (X2-X1)*(Y-Y1)/(Y2-Y1)
               X = -XX
 130           IF (X.LE.XX) THEN
                  FI1 = ATAN2 (X,Y)
                  FIT = FI1 + ZETA
                  RT = SQRT (X**2 + Y**2)
                  XT = RT * SIN (FIT)
                  YT = RT * COS (FIT)
                  JT = NINT ((XT - TR(1))/TR(2))
                  IT = NINT ((YT - TR(4))/TR(6))
C                                       checks :
C                                       a.point  within the panel?
                  XT = TR(1) + TR(2) * JT
                  YT = TR(4) + TR(6) * IT
                  RT = SQRT (XT**2 + YT**2)
                  FIT = ATAN2 (XT, YT)
                  IF (FIT.LT.0.) FIT = FIT + TWOPI
C                                       b. have we used this point ?
                  IF ((HIT(JT,IT)) .AND. (FIT.GE.TH1) .AND. (FIT.LE.TH2)
     *               .AND. (RT.GE.RAD(1,IR)) .AND. (RT.LE.RAD(2,IR)))
     *               THEN
                     FI1 = FIT - ZETA
                     XI = RT * SIN (FI1)
                     YI = RT * COS (FI1) - Y1
                     CALL APPLYP (OPT, PAR, IR, IP, XI, YI, X1, X2,
     *                  Y2, MN)
                     NDEV(JT,IT) = MN
                     VDEV(JT,IT) = VDEV(JT,IT) - NDEV(JT,IT)
                     HIT(JT,IT) = .FALSE.
                     END IF
                  X = X + INC
                  GO TO 130
                  END IF
               Y = Y + INC
               GO TO 120
               END IF
 140        CONTINUE
 150     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETRMS (MX, V, VMAP, RMS)
C-----------------------------------------------------------------------
C   GETRMS computes the image rms
C   Inputs
C      MX     I      Dimension of arrays
C      V      R(*)   Image
C      VMAP   I(*)   Mask
C   Outputs:
C      RMS    R      Image rms
C-----------------------------------------------------------------------
      INTEGER   MX, VMAP(MX,*)
      REAL      V(MX,*), RMS
C
      INTEGER   I, J
      REAL      CN
C-----------------------------------------------------------------------
      CN =  0.0
      RMS = 0.0
      DO 20 J = 1,MX
        DO 10 I = 1,MX
           IF (VMAP(I,J).GT.0) THEN
              CN  = CN + 1.0
              RMS = RMS + V(I,J)**2
              END IF
 10        CONTINUE
 20     CONTINUE
C
      IF (CN.GT.0.0) RMS = SQRT (RMS/CN)
C
 999  RETURN
      END
      SUBROUTINE SETNOD (NR, NP, RAD, NODX, NODY)
C-----------------------------------------------------------------------
C   compute node points from panel info
C   Inputs:
C      NR     I      Number rings
C      NP     I(*)   Number panels in ring i
C      RAD    R(*)   Radius (m) of inside/outside of ring
C   Outputs:
C      NODX   R(*)   X coordinates of corners
C      NODY   R(*)   Y coordinates of corners
C-----------------------------------------------------------------------
      INTEGER   NR, NP(*)
      REAL      RAD(2,*), NODX(4,100,*), NODY(4,100,*)
C
      INTEGER   I, J
      REAL      TH1, TH2
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Loop over rings
      DO 20 I = 1,NR
C                                       Loop over panels IN each ring
         DO 10 J = 1,NP(I)
            TH1 = (J-1) * TWOPI / NP(I)
            TH2 = J * TWOPI / NP(I)
C                                       Set coords bottom left corner
C                                       of Jth panel in Ring I.
            NODX(1,J,I) =  RAD(1,I) * SIN(TH1)
            NODY(1,J,I) =  RAD(1,I) * COS(TH1)
C                                       Coords of bottom right corner
            NODX(2,J,I) =  RAD(1,I) * SIN(TH2)
            NODY(2,J,I) =  RAD(1,I) * COS(TH2)
C                                       Coords of top right corner
            NODX(3,J,I) =  RAD(2,I) * SIN(TH2)
            NODY(3,J,I) =  RAD(2,I) * COS(TH2)
C                                       Coords of top left corner
            NODX(4,J,I) =  RAD(2,I) * SIN(TH1)
            NODY(4,J,I) =  RAD(2,I) * COS(TH1)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ANTGN (MX, VDEV, VMAP, LAMBDA, MGAIN, MXGN)
C-----------------------------------------------------------------------
C   This routine calculates the actual and theoretical power gain.
C   It assumes uniform illumination.
C   Inputs:
C      LAMBDA   R   Wavelength
C   Outputs:
C      MGAIN    R   max gain  this roughness
C      MXGN     R   theoretical max gain
C-----------------------------------------------------------------------
      INTEGER   MX, VMAP(MX,*)
      REAL      VDEV(MX,*), LAMBDA, MGAIN, MXGN
C
      INCLUDE 'PANELDATA'
      INTEGER   I, J
      REAL      R, X, Y, VPHS, INTR, INTI, INT2, FACT, Q, AMAX
      DOUBLE PRECISION FORPI
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      FORPI = 4.0D0 * PI
      INTR = 0.0
      INTI = 0.0
      INT2 = 0.
      FACT = 1000. * RESN / LAMBDA
C                                       Convert surface error to phase
C                                       and compute gain loss
      DO 20 J = 1,MX
         Y = TR(4) + J*TR(6)
         DO 10 I = 1,MX
            IF (VMAP(I,J).GT.0) THEN
               X = TR(4) + J*TR(6)
               R = SQRT (X*X + Y*Y)
               Q = R / (2.*FOCUS)
               VPHS = VDEV(I,J) * FORPI / (SQRT(1.+Q*Q) * LAMBDA)
               INTR = INTR + COS (VPHS)
               INTI = INTI + SIN (VPHS)
               INT2 = INT2 + 1.0
               END IF
 10         CONTINUE
 20      CONTINUE
C
      AMAX = SQRT (INTR*INTR + INTI*INTI) / INT2
      AMAX = AMAX * FACT * FACT
      MGAIN = AMAX * FORPI
      MGAIN = 10. * LOG10 (MGAIN)
      MXGN = INT2 * FACT * FACT * FORPI
      MXGN = 10. * LOG10 (MXGN)
C
 999  RETURN
      END
      SUBROUTINE SOLVEP (OPT, IR, IP, NV, V, PAR, X1, X2, Y2, IRET)
C-----------------------------------------------------------------------
C   Does a solution for 1 panel
C   Inputs:
C      OPT    I      model:
C                    1  rigid panel, displaced normal to surface
C                    2  rigid panel, lifted and tilted
C                    3  flexible panel.
C      IR     I      Ring number
C      IP     I      Panel number
C      NV     I
C      V      R(*)
C      X2     R
C      Y1     R
C      Y2     R
C   Outputs:
C      PAR    R(*)
C      IRET   I
C-----------------------------------------------------------------------
      INTEGER   OPT, IP, IR, NV, IRET
      REAL      V(3,*), PAR(4,100,*), X1, X2, Y2
C
      INTEGER   I, J, NCOUNT
      REAL      MN, X, Y, F, A1, A2, A3, A4
      DOUBLE PRECISION M(4,4), R(4), W(4,4), DBG(4,4)
C-----------------------------------------------------------------------
      IRET = 0
C                                       initialise the arrays:
      DO 10 I = 1,4
         DO 5 J = 1,4
            M(J,I) = 0.
 5          CONTINUE
         R(I) = 0.
 10      CONTINUE
C                                       rigid, shift only
      IF (OPT.EQ.1) THEN
         MN = 0.
         NCOUNT = 0
         DO 20 I = 1,NV
            IF (V(3,I).NE.0.) THEN
               MN = MN + V(3,I)
               NCOUNT = NCOUNT + 1
               END IF
 20         CONTINUE
         MN = MN / NCOUNT
         PAR(1,IP,IR) = MN
C                                       rigid, shift+tilt
      ELSE IF (OPT.EQ.2) then
         DO 30 I = 1,NV
            IF (V(3,I).NE.0.) THEN
               M(1,1) = M(1,1) + V(1,I)*V(1,I)
               M(1,2) = M(1,2) + V(1,I)*V(2,I)
               M(1,3) = M(1,3) + V(1,I)
               M(2,1) = M(1,2)
               M(2,2) = M(2,2) + V(2,I)*V(2,I)
               M(2,3) = M(2,3) + V(2,I)
               M(3,1) = M(1,3)
               M(3,2) = M(2,3)
               M(3,3) = M(3,3) + 1.0
               R(1)   = R(1) + V(3,I)*V(1,I)
               R(2)   = R(2) + V(3,I)*V(2,I)
               R(3)   = R(3) + V(3,I)
               END IF
 30         CONTINUE
         CALL RCOPY (16, M, DBG)
         CALL GAUSSJ (M, 3, 4, W, 3, 4, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 40 I = 1,3
            MN = 0.
            DO 35 J = 1,3
               MN = MN + M(J,I) * R(J)
 35            CONTINUE
            PAR(I,IP,IR) = MN
 40         CONTINUE
C                                       flexible
      ELSE IF (OPT.EQ.3) then
         DO 50 I = 1,NV
            IF (V(3,I).NE.0.) THEN
               X = V(1,I)
               Y = V(2,I)
               F = X1 + Y*(X2-X1)/Y2
               A1 = (Y2-Y) * (1.-X/F) / (2.0*Y2)
               A2 =     Y  * (1.-X/F) / (2.0*Y2)
               A3 = (Y2-Y) * (1.+X/F) / (2.0*Y2)
               A4 =     Y  * (1.+X/F) / (2.0*Y2)
               M(1,1) = M(1,1) + A1*A1
               M(1,2) = M(1,2) + A1*A2
               M(1,3) = M(1,3) + A1*A3
               M(1,4) = M(1,4) + A1*A4
               M(2,1) = M(1,2)
               M(2,2) = M(2,2) + A2*A2
               M(2,3) = M(2,3) + A2*A3
               M(2,4) = M(2,4) + A2*A4
               M(3,1) = M(1,3)
               M(3,2) = M(2,3)
               M(3,3) = M(3,3) + A3*A3
               M(3,4) = M(3,4) + A3*A4
               M(4,1) = M(1,4)
               M(4,2) = M(2,4)
               M(4,3) = M(3,4)
               M(4,4) = M(4,4) + A4*A4
               R(1)   = R(1) + V(3,i)*A1
               R(2)   = R(2) + V(3,i)*A2
               R(3)   = R(3) + V(3,i)*A3
               R(4)   = R(4) + V(3,i)*A4
               END IF
 50         CONTINUE
         CALL GAUSSJ (M, 4, 4, W, 4, 4, IRET)
         IF (IRET.NE.0) GO TO 999
c        call ludcmp (m,4,4, indx, w)
c        call lubksb (m,4,4, indx, r)
         DO 60 I = 1,4
            MN = 0.
            DO 55 J = 1,4
               MN = MN + M(J,I) * R(J)
 55            CONTINUE
            PAR(I,IP,IR) = MN
 60         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE APPLYP (OPT, PAR, IR, IP, XI, YI, X1, X2, Y2, MN)
C-----------------------------------------------------------------------
C   applies the model to determine the correction
C   Inputs:
C      OPT   I      model:
C                   1  rigid panel, displaced normal to surface
C                   2  rigid panel, lifted and tilted
C                   3  flexible panel.
C      IR    I      Ring number
C      IP    I      Panel number
C      NV    I
C      V     R(*)
C      X1    R
C      X2    R
C      Y2    R
C   Outputs:
C      MN    R      nodel fit value
C-----------------------------------------------------------------------
      INTEGER   OPT, IR, IP
      REAL      PAR(4,100,*), XI, YI, X1, X2, Y2, MN
C
      INTEGER   K
      REAL      F, A(4)
C-----------------------------------------------------------------------
C                                       rigid, lift only
      IF (OPT.EQ.1) THEN
         MN = PAR(1,IP,IR)
C                                       rigid, lift and tilt
      ELSE IF (OPT.EQ.2) then
         MN = XI * PAR(1,IP,IR) + YI * PAR(2,IP,IR) + PAR(3,IP,IR)
C                                       flexible
      ELSE IF (OPT.EQ.3) then
         F = X1 + YI * (X2-X1) / Y2
         A(1) = (Y2-YI) * (1.-XI/F) / (2.0*Y2)
         A(2) =     YI  * (1.-XI/F) / (2.0*Y2)
         A(3) = (Y2-YI) * (1.+XI/F) / (2.0*Y2)
         A(4) =     YI  * (1.+XI/F) / (2.0*Y2)
         MN = 0.
         DO 30 K = 1,4
            MN = MN + A(K) * PAR(K,IP,IR)
 30         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE REPORT (RMS0, GAIN)
C-----------------------------------------------------------------------
C   generate the report to the message terminal and text file
C   Inputs:
C      RMS0   R      initial rms
C      GAIN   R(*)   initial, final, theoretical gains at 5 freq
C-----------------------------------------------------------------------
      REAL      RMS0, GAIN(3,5)
C
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
      INTEGER   IP, IR, K, I, J, JTRIM, IRET
      REAL      X1, X2, Y1, Y2, A(4), Z(4), FI, X(4), Y(4), XI, YI, MN,
     *   F
      CHARACTER LINE *132
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
*-------------------------------------------------------
      WRITE (LINE,1000) 'before', RMS0
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (LINE,1000) 'after ', RMS
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write gain computations
      MSGTXT = 'Estimated Gain Performance -- Uniform Illumination'
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1101)
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      MSGTXT =  '---------------------------------------------------'
     :   // '-------'
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1102) ' Uncorrected  |', (GAIN(1,J), J = 1,5)
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1102) '   Corrected  |', (GAIN(2,J), J = 1,5)
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT,1102) ' Theoretical  |', (GAIN(3,J), J = 1,5)
      CALL MSGWRT (4)
      LINE = MSGTXT
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = ' Units are (INCH/1000).'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = ' LOWER the panel if the number is POSITIVE'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = ' Panels are numbered clockwise, starting from'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = ' the highest point'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = ' '
      J = 1
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = 'ring  panel      Inner edge              Outer edge'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
      LINE = '               left       right        left      right'
      J = JTRIM (LINE)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       print results
C                                       loop rings
      DO 60 IR = 1,NR
         FI = TWOPI / NP(IR)
         X1 = RAD(1,IR) * SIN(FI/2.0)
         X2 = RAD(2,IR) * SIN (FI/2.0)
         Y1 = RAD(1,IR) * COS (FI/2.0)
         Y2 = RAD(2,IR) * COS (FI/2.0)
         X(1) = -X1
         X(2) =  X1
         X(3) = -X2
         X(4) =  X2
         Y(1) = 0.
         Y(2) = 0.
         Y(3) = Y2 - Y1
         Y(4) = Y2 - Y1
C                                       loop panels
         DO 50 IP = 1,NP(IR)
C                                       rigid, shift only
            IF (OPT.EQ.1) THEN
               DO 10 K = 1,4
                  Z(K) = PAR(1,IP,IR) * 39.37
 10               CONTINUE
C                                       rigid, tilt+shift
            ELSE IF (OPT.EQ.2) THEN
               DO 20 K = 1,4
                  Z(K) = (X(K) * PAR(1,IP,IR)
     *                + Y(K) * PAR(2,IP,IR)
     *                + PAR(3,IP,IR)) * 39.37
 20               CONTINUE
C                                       flexible
            ELSE IF (OPT.EQ.3) then
               DO 40 I = 1,4
                  XI = X(I)
                  YI = Y(I)
                  F = X1 + YI * (X2-X1) / Y2
                  A(1) = (Y2-YI) * (1.-XI/F) / (2.0*Y2)
                  A(2) =     YI  * (1.-XI/F) / (2.0*Y2)
                  A(3) = (Y2-YI) * (1.+XI/F) / (2.0*Y2)
                  A(4) =     YI  * (1.+XI/F) / (2.0*Y2)
                  mn = 0.
                  DO 30 K = 1,4
                     MN = MN + A(K) * PAR(K,IP,IR)
 30                  continue
C                                       convert to INCH/1000.
                  Z(I) = MN * 1000.0 / 25.4
 40               CONTINUE
               END IF
            WRITE (LINE,1140) IR, IP, Z
            J = JTRIM (LINE)
            CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE(:J), IRET)
            IF (IRET.NE.0) GO TO 990
 50         CONTINUE
 60      CONTINUE
      GO TO 995
C
 990  MSGTXT = 'REPORT: ERROR WRITING OUTPUT TEXT FILE'
      CALL MSGWRT (8)
C
 995  CALL ZTXCLS (TXLUN, TXIND, I)
      TXIND = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('rms ',A6,' adjustment :',F9.2,' mm')
 1101 FORMAT (18X,'3.5cm   2.0cm   1.3cm   0.7cm  0.34cm')
 1102 FORMAT (A,5F8.2)
 1140 FORMAT (I3,2X,I3,3X,2F10.0,4X,2F10.0)
      END
      SUBROUTINE GAUSSJ (A, N, NP, B, M, MP, IRET)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   NMAX
      PARAMETER (NMAX = 50)
C
      INTEGER   N, NP, M, MP, IRET
      DOUBLE PRECISION A(NP,NP), B(NP,MP)
C
      INTEGER   IPIV(NMAX), INDXR(NMAX), INDXC(NMAX), I, J, K, IROW,
     *   ICOL, L, LL
      DOUBLE PRECISION BIG, DUM, PIVINV
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DO 20 J = 1,N
        IPIV(J) = 0
 20     CONTINUE
      IRET = 0
      DO 70 I = 1,N
         BIG = 0.
         DO 30 J = 1,N
            IF (IPIV(J).NE.1) THEN
               DO 25 K = 1,N
                  IF (IPIV(K).EQ.0) THEN
                     IF (ABS(A(J,K)).GE.BIG) THEN
                        BIG = ABS (A(J,K))
                        IROW = J
                        ICOL = K
                        END IF
                  ELSE IF (IPIV(K).GT.1) THEN
                     GO TO 900
                     END IF
 25               CONTINUE
               END IF
 30         CONTINUE

        IPIV(ICOL) = IPIV(ICOL) + 1
        IF (IROW.NE.ICOL) THEN
           DO 35 L = 1,N
              DUM = A(IROW,L)
              A(IROW,L) = A(ICOL,L)
              A(ICOL,L) = DUM
 35           CONTINUE
           DO 40 L = 1,M
              DUM = B(IROW,L)
              B(IROW,L) = B(ICOL,L)
              B(ICOL,L) = DUM
 40           CONTINUE
           END IF

        INDXR(I) = IROW
        INDXC(I) = ICOL
        IF (A(ICOL,ICOL).EQ.0.) THEN
           GO TO 900
           END IF
        PIVINV = 1.0 / A(ICOL,ICOL)
        A(ICOL,ICOL) = 1.
        DO 45 L = 1,N
           A(ICOL,L) = A(ICOL,L) * PIVINV
 45        CONTINUE
        DO 50 L = 1,M
           B(ICOL,L) = B(ICOL,L) * PIVINV
 50        CONTINUE
        DO 65 LL = 1,N
           IF (LL.NE.ICOL) THEN
              DUM = A(LL,ICOL)
              A(LL,ICOL) = 0.
              DO 55 L = 1,N
                 A(LL,L) = A(LL,L) - A(ICOL,L) * DUM
 55              CONTINUE
              DO 60 L = 1,M
                 B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
 60              CONTINUE
               END IF
 65         CONTINUE
 70      CONTINUE

      DO 80 L = N,1,-1
         IF (INDXR(L).NE.INDXC(L)) THEN
            DO 75 K = 1,N
               DUM = A(K,INDXR(L))
               A(K,INDXR(L)) = A(K,INDXC(L))
               A(K,INDXC(L)) = DUM
 75            CONTINUE
            END IF
 80      CONTINUE
      GO TO 999
C
 900  MSGTXT = 'GAUSSJ: MATRIX IS SINGULAR'
      CALL MSGWRT (8)
      IRET = 1
C
 999  RETURN
      END
      SUBROUTINE DOPLOT (TYPE, MX, V, IRET)
C-----------------------------------------------------------------------
C   Generates the full pgplot gray-scale, contour, layout, label
C   Inputs:
C      TYPE   I   1, 2, 3 for stage opf operation
C-----------------------------------------------------------------------
      INTEGER   TYPE, MX, IRET
      REAL      V(MX,*)
C
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
      INTEGER   NBITS, LBITS(6)
C     CHARACTER XL*70, YL*70
      CHARACTER GL*70
      LOGICAL   LAST
C     DATA XL /'X (parallel to EL axis) (m)'/
C     DATA YL /'(m)'/
C-----------------------------------------------------------------------
      NBITS = 6
      CALL ZGTBIT (NBITS, IDOPLT, LBITS)
      IRET = 0
C                                       label
      IF (TYPE.EQ.1) THEN
         WRITE (GL,1001) RMS
      ELSE IF (TYPE.EQ.2) THEN
         WRITE (GL,1002) OPTSTR
      ELSE IF (TYPE.EQ.3) THEN
         WRITE (GL,1003) RMS, OPTSTR
         END IF
C                                       aips style plot
      LAST = (TYPE.EQ.3) .OR. ((TYPE.EQ.2) .AND. (LBITS(3).LE.0))
     *   .OR. ((TYPE.EQ.1) .AND. (LBITS(2)+LBITS(3).LE.0))
      IF (LBITS(TYPE).EQ.1) CALL AIPLOT (LAST, GL, MX, V, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('Raw data - rms',F8.2,' mm')
 1002 FORMAT ('Adjustment map - Fit ',A)
 1003 FORMAT ('Residuals - rms',F8.2,' mm   Fit ',A)
      END
      SUBROUTINE AIPLOT (LAST, GL, N, V, IRET)
C-----------------------------------------------------------------------
C   generates a full plot in AIPS style
C   Inputs:
C      LAST   L       Last plot?
C      GL     C*(*)   Plot title
C      M      I       Dimension of square array V
C      V      R(*)    data array
C   Outputs:
C      IRET   I       error code
C-----------------------------------------------------------------------
      LOGICAL   LAST
      INTEGER   N, IRET
      CHARACTER GL*(*)
      REAL      V(N,N)
C
      INCLUDE 'PANEL.INC'
      INCLUDE 'PANELDATA'
      INCLUDE 'PANELPLOT'
      INTEGER   I, J, K, L, M, DD, IVER, IGSIZE, TVCORN(2), ITYPE,
     *   IDEPTH(5), ILABEL, NTEXT, IROUND, IROW(768), JTRIM, IHPVAL,
     *   ILPVAL
      REAL      BIGA(768,768), DIFF, YGAP, LBLC(7), LTRC(7), NBLC(2),
     *   NTRC(2), LXINC, LYINC, RANGE(2), GFAC, GOFF, F, XYRATO, VV, X,
     *   Y, XP(5), YP(5)
      CHARACTER IGFILE*48, TXTMSG(2)*80, NAMSTR*18
      LOGICAL   SLICE, DOTV
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA TVCORN /2*0/
      DATA SLICE /.FALSE./
C                                       gray 0 -> 4 mm
      DATA F/4.0/
C-----------------------------------------------------------------------
C                                       compute interpolated array
C                                       column interpolation
      DD = 768 / N
      DO 30 J = 1,N
         L = (J - 1) * DD + 1
         DO 20 I = 1,N-1
            K = (I - 1) * DD + 1
            BIGA(K,L) = V(I,J)
            IF ((V(I,J).NE.FBLANK) .AND. (V(I+1,J).NE.FBLANK)) THEN
               DIFF = (V(I+1,J) - V(I,J)) / DD
               DO 10 M = 1,DD-1
                  BIGA(K+M,L) = BIGA(K,L) + M * DIFF
 10               CONTINUE
            ELSE IF (DD.EQ.3) THEN
               BIGA(K+1,L) = V(I,J)
               BIGA(K+2,L) = V(I+1,J)
            ELSE
               BIGA(K+1,L) = V(I,J)
               BIGA(K+2,L) = V(I,J)
               BIGA(K+3,L) = FBLANK
               BIGA(K+4,L) = V(I+1,J)
               BIGA(K+5,L) = V(I+1,J)
               END IF
 20         CONTINUE
         K = (N - 1) * DD + 1
         BIGA(K,L) = V(N,J)
 30      CONTINUE
C                                       row interpolation
      DO 60 J = 1,N-1
         L = (J - 1) * DD + 1
         IF (DD.EQ.3) THEN
            DO 40 K = 1,3*N-2
               IF ((BIGA(K,L).EQ.FBLANK) .OR. (BIGA(K,L+DD).EQ.FBLANK))
     *            THEN
                  BIGA(K,L+1) = BIGA(K,L)
                  BIGA(K,L+2) = BIGA(K,L+DD)
               ELSE
                  DIFF = (BIGA(K,L+DD) - BIGA(K,L)) / DD
                  BIGA(K,L+1) = BIGA(K,L) + DIFF
                  BIGA(K,L+2) = BIGA(K,L+3) - DIFF
                  END IF
 40            CONTINUE
         ELSE
            DO 50 K = 1,6*N-5
               IF ((BIGA(K,L).NE.FBLANK) .AND. (BIGA(K,L+DD).NE.FBLANK))
     *            THEN
                  DIFF = (BIGA(K,L+DD) - BIGA(K,L)) / DD
                  DO 45 M = 1,DD-1
                     BIGA(K,L+M) = BIGA(K,L) + M * DIFF
 45                  CONTINUE
               ELSE
                  BIGA(K,L+1) = BIGA(K,L)
                  BIGA(K,L+2) = BIGA(K,L)
                  BIGA(K,L+3) = FBLANK
                  BIGA(K,L+4) = BIGA(K,L+DD)
                  BIGA(K,L+5) = BIGA(K,L+DD)
                  END IF
 50            CONTINUE
            END IF
 60      CONTINUE
C                                       add PL file to catalog
      IVER = 0
      IF (XDOTV.LE.0.0) THEN
         CALL MADDEX ('PL', DISK1, CNO1, CATBLK, PLBUF, .TRUE., 'UPDT',
     *      IVER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ADD PL FILE TO CATALOG HEADER'
            GO TO 995
            END IF
         END IF
C                                       header
      CALL COPY (256, CATBLK, CATSAV)
      CALL SUBHDR (BLC, TRC, XINC, YINC)
      CALL RFILL (7, 1.0, LBLC)
      CALL RFILL (5, 1.0, LTRC(3))
      LTRC(1) = N
      LTRC(2) = LTRC(1)
      LXINC = 1.0 / DD
      LYINC = LXINC
      CALL SUBHDR (LBLC, LTRC, LXINC, LYINC)
      LTRC(1) = (N - 1) * DD + 1
      LTRC(2) = LTRC(1)
C                                       plot init
      LOCNUM = 1
      CALL FILL (5, 1, IDEPTH)
      CALL SETLOC (IDEPTH, .TRUE.)
      CALL ZPHFIL ('PL', DISK1, CNO1, IVER, IGFILE, IRET)
      ITYPE = 1
      IGSIZE = 2
      GLUN = 26
      TVCHN = XCHAN + 0.1
      TVCHN = MAX (1, MIN (NGRAY, TVCHN))
      GRCHN = XGRCH + 0.1
      DOTV = XDOTV.GT.0.0
      CALL GINIT (DISK1, CNO1, IGFILE, IGSIZE, ITYPE, NPARMS, XNAME1,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUF, GLUN, GFIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITIALIZE PLOT FILE'
         GO TO 990
         END IF
      IF (DOTV) GPHDOD = .TRUE.
C                                       init labeling
      ILABEL = -7
      CALL LABINI (LBLC, LTRC, IDEPTH, CH, ILABEL, SLICE, YGAP, TXTMSG,
     *   NTEXT)
      CH(1) = 5.5
      CH(2) = 3.5
      CH(3) = 0.5
      CH(4) = 4.0
      XYRATO = 1.0
C                                       Init for plotting
      NBLC(1) = LBLC(1) - 0.7
      NBLC(2) = LBLC(2) - 0.7
      NTRC(1) = LTRC(1) + 0.7
      NTRC(2) = LTRC(2) + 0.7
      CALL GINITL (NBLC, NTRC, XYRATO, CH, IDEPTH, PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT LINE DRAWING'
         GO TO 990
         END IF
      RANGE(1) = 0.0
      RANGE(2) = F / 1000.0
      GFAC = 2.0D0 ** MIN (30, NBITWD)  -  4.0D0
      GFAC = (RANGE(2)-RANGE(1)) / GFAC
      GOFF = (RANGE(2)+RANGE(1)) / 2.0D0
      RANGE(2) = (RANGE(2) - GOFF) / GFAC
      RANGE(1) = (RANGE(1) - GOFF) / GFAC
      IHPVAL = IROUND (RANGE(2))
      ILPVAL = IROUND (RANGE(1))
      RANGE(1) = (ILPVAL * GFAC + GOFF)
      RANGE(2) = (IHPVAL * GFAC + GOFF)
      CALL GINITG (ILPVAL, IHPVAL, RANGE, PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT GRAY SCALE'
         GO TO 990
         END IF
      CALL GLTYPE (1, PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SET LINE TYPE = 1'
         GO TO 990
         END IF
      M = CATBLK(KINAX)
      IF (APARM(8).LE.0.0) THEN
         TXTMSG(1) = 'Start grey scale'
         CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'ADD COMMENT'
            GO TO 990
            END IF
         DO 90 J = 1,M
            DO 80 I = 1,M
               IF (BIGA(I,J).NE.FBLANK) THEN
                  VV = (ABS(BIGA(I,J)/1000.0) - GOFF) / GFAC
                  IROW(I) = IROUND (VV)
               ELSE
                  IROW(I) = ILPVAL
                  END IF
 80            CONTINUE
           X = BLC(1)
           Y = J
           CALL GPOS (X, Y, PLBUF, IRET)
           IF (IRET.NE.0) THEN
              WRITE (MSGTXT,1000) IRET, 'GPOS CALL FOR GRAYPX'
              GO TO 990
              END IF
           CALL GRAYPX (M, 0, IROW, PLBUF, IRET)
           IF (IRET.NE.0) THEN
              WRITE (MSGTXT,1000) IRET, 'WRITE LINE OF GRAY SCALE'
              GO TO 990
              END IF
 90        CONTINUE
        END IF
C                                       Now do labels
      TXTMSG(1) = 'Start labeling'
      CALL GCOMNT (-1, TXTMSG, PLBUF, IRET)
      IF (IRET.EQ.0) CALL GLTYPE (1, PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'COMMENT AND LINE TYPE'
         GO TO 990
         END IF
      CALL GPOS (NBLC(1), NBLC(2), PLBUF, IRET)
      IF (IRET.EQ.0) CALL GVEC (NTRC(1), NBLC(2), PLBUF, IRET)
      IF (IRET.EQ.0) CALL GVEC (NTRC(1), NTRC(2), PLBUF, IRET)
      IF (IRET.EQ.0) CALL GVEC (NBLC(1), NTRC(2), PLBUF, IRET)
      IF (IRET.EQ.0) CALL GVEC (NBLC(1), NBLC(2), PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING BORDER LINES'
         GO TO 990
         END IF
      CALL CLAB1 (NBLC, NTRC, CH, ILABEL, XYRATO, .FALSE., PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING AXIS LABELS'
         GO TO 990
         END IF
      CALL GPOS (NBLC(1), NTRC(2), PLBUF, IRET)
      J = JTRIM (GL)
      IF (IRET.EQ.0) CALL GCHAR (J, 0, 0.0, 1.0, GL(:J), PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE PLOT TITLE'
         GO TO 990
         END IF
      CALL GPOS (NBLC(1), NTRC(2), PLBUF, IRET)
      IF (IRET.EQ.0) THEN
         TXTMSG(1) = 'IMAGE = '
         I = 9
         CALL H2CHR (12, KHIMNO, CATOLD(KHIMN,1), NAMSTR(1:12))
         CALL H2CHR (6, KHIMCO, CATOLD(KHIMC,1), NAMSTR(13:18))
         CALL NAMEST (NAMSTR, CATOLD(KIIMS,1), TXTMSG(1)(I:), J)
         I = I + J + 4
         TXTMSG(1)(I:) = 'MASK = '
         I = I + 7
         CALL H2CHR (12, KHIMNO, CATOLD(KHIMN,2), NAMSTR(1:12))
         CALL H2CHR (6, KHIMCO, CATOLD(KHIMC,2), NAMSTR(13:18))
         CALL NAMEST (NAMSTR, CATOLD(KIIMS,2), TXTMSG(1)(I:), J)
         J = JTRIM (TXTMSG(1))
         CALL GCHAR (J, 0, 0.0, 2.5, TXTMSG(1)(:J), PLBUF, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE PLOT TITLE'
         GO TO 990
         END IF
C                                       contouring
      IF (APARM(9).LE.0.0) THEN
         TXTMSG(1) = 'Do panel layout'
         CALL GCOMNT (-1, TXTMSG, PLBUF, IRET)
         IF (IRET.EQ.0) CALL GLTYPE (2, PLBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COMMENT AND LINE TYPE'
            GO TO 990
            END IF
         CALL DRWCON (M, 768, BIGA, PLBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DRAWING CONTOURS'
            GO TO 990
            END IF
         END IF
C                                       do panel layout
      IF (APARM(10).LE.0.0) THEN
         TXTMSG(1) = 'Do panel layout'
         CALL GCOMNT (-1, TXTMSG, PLBUF, IRET)
         IF (IRET.EQ.0) CALL GLTYPE (3, PLBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COMMENT AND LINE TYPE'
            GO TO 990
            END IF
         DO 130 I = 1,NR
            DO 120 J = 1,NP(I)
               DO 110 K = 1,4
                  XP(K) = NODX(K,J,I)
                  YP(K) = NODY(K,J,I)
                  XP(K) = CATR(KRCRP) + XP(K) / ABS(CATR(KRCIC))
                  YP(K) = CATR(KRCRP+1) + YP(K) / ABS(CATR(KRCIC+1))
 110              CONTINUE
               XP(5) = XP(1)
               YP(5) = YP(1)
               CALL GPOS (XP(1), YP(1), PLBUF, IRET)
               IF (IRET.EQ.0) CALL GVEC (XP(2), YP(2), PLBUF, IRET)
               IF (IRET.EQ.0) CALL GVEC (XP(3), YP(3), PLBUF, IRET)
               IF (IRET.EQ.0) CALL GVEC (XP(4), YP(4), PLBUF, IRET)
               IF (IRET.EQ.0) CALL GVEC (XP(5), YP(5), PLBUF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'DRAWING PANEL OUTLINE'
                  GO TO 990
                  END IF
 120           CONTINUE
 130        CONTINUE
         END IF
C                                       Finish up plot file
      GPHPAG = .NOT.LAST
      CALL GFINIS (PLBUF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH THE PLOT'
         GO TO 995
      ELSE IF (IRET.LT.0) THEN
         IDOPLT = 0
         IRET = 0
         END IF
C                                       Write sucessful finish message.
      CALL COPY (256, CATSAV, CATBLK)
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISK1, CNO1, IVER, PLBUF, IRET)
         WRITE (MSGTXT,1130) IVER
         CALL MSGWRT (2)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
      CALL COPY (256, CATSAV, CATBLK)
      IF (.NOT.DOTV) THEN
         CALL ZCLOSE (GLUN, GFIND, IRET)
         CALL ZDESTR (DISK1, IGFILE, IRET)
         IF (IVER.GT.0) CALL DELEXT ('PL', DISK1, CNO1, 'READ',
     *      CATBLK, PLBUF, IVER, IRET)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AIPLOT ERROR',I5,' ON ',A)
 1130 FORMAT ('AIPLOT: wrote plot file version',I4)
      END
      SUBROUTINE DRWCON (M, N, V, PLBUF, IRET)
C-----------------------------------------------------------------------
C   DRWCON will write commands to a plot file for the execution of
C   a contour plot.  CONDRW reads from disk, DRWCON uses image in core.
C   Inputs:
C      M       I      Number pixels to use in image
C      N       I      Dimension in X of V
C      V       R(*)   Image
C   In/out:
C      PLBUF   I(*)   Plot buffer
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   M, N, PLBUF(*), IRET
      REAL      V(N,*)
C
      INCLUDE 'PANELDATA'
      REAL      VAL(3), XPOS(3), YPOS(3), TEMP, VC, VL, VM, VS, XA, XB,
     *   XL, XLAST, XM, XS, YA, YB, YL, YLAST, YM, YS, DELTAX, DELTAY,
     *   TLEV, XLEV
      INTEGER   IPERM(3,6), IBLCX, IBLCY, IBLCY1, ICOL, II, INDEX, I,
     *   INPIXS, IPLUS, IROW, ISKIP, ITRCX, ITRCXM, ITRCY, ITRI, MININT,
     *   LOCINT, IROUND, ISLEV, JJ, LASTC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA IPERM /1,3,2, 3,2,1, 3,1,2, 2,1,3, 1,2,3, 2,3,1/
C-----------------------------------------------------------------------
      ISKIP = 2
      XLAST = -1000.
      YLAST = -1000.
      TEMP = M - 1
      TEMP = 10 - 3 * LOG10 (TEMP)
      LOCINT = IROUND (TEMP)
      IF (LOCINT.LT.2) LOCINT = 2
      IBLCY = 1
      ITRCY = M
      ITRCX = M
      IBLCX = 1
      INPIXS = ITRCX - IBLCX + 1
      LASTC = 0
C                                       magic parms for dashed lines
      XLEV = 256.0 / INPIXS
      TLEV = 256.0 / (ITRCY - IBLCY + 1.0)
      ISLEV = SQRT (1.0 / (XLEV * TLEV)) + 0.1
      IF (ISLEV.LT.1) ISLEV = 1
      IF (XLEV.LT.1.0) XLEV = (SQRT (XLEV) + 3.0*XLEV) / 4.0
C                                       save first row.
      CALL RCOPY (M, V(1,1), RLROW)
C                                       Loop over all rows.
      IBLCY1 = IBLCY + 1
      DO 300 IROW = IBLCY1,ITRCY
C                                       Loop over all pixels in row.
         IPLUS = 0
         ITRCXM = ITRCX - 1
         DO 250 ICOL = IBLCX,ITRCXM
            IPLUS = IPLUS + 1
C                                       Init values
            VAL(1) = V(IPLUS,IROW)
            VAL(2) = V(IPLUS+1,IROW)
            VAL(3) = RLROW(IPLUS)
C                                       Init positions.
            XPOS(1) = ICOL
            XPOS(2) = ICOL + 1
            XPOS(3) = ICOL
            YPOS(1) = IROW
            YPOS(2) = IROW
            YPOS(3) = IROW - 1
C                                       Loop for both triangles.
            DO 200 ITRI = 1,2
C                                       Changes for 2nd triangle.
               IF (ITRI.EQ.2) THEN
                  VAL(1) = RLROW(IPLUS+1)
                  XPOS(1) = ICOL + 1
                  YPOS(1) = IROW - 1
                  END IF
C                                       Order points in triangle.
               DO 130 II = 1,3
                  IF (VAL(II).EQ.FBLANK) GO TO 200
 130              CONTINUE
               INDEX = 0
               IF (VAL(1).GT.VAL(2)) INDEX = 1
               IF (VAL(3).GE.VAL(1)) INDEX = INDEX + 2
               IF (VAL(2).GE.VAL(3)) INDEX = INDEX + 4
C                                       find large, med, small
C                                       values and X,Y positions.
               II = IPERM(1,INDEX)
               VL = VAL(II)
               XL = XPOS(II)
               YL = YPOS(II)
C
               II = IPERM(2,INDEX)
               VM = VAL(II)
               XM = XPOS(II)
               YM = YPOS(II)
C
               II = IPERM(3,INDEX)
               VS = VAL(II)
               XS = XPOS(II)
               YS = YPOS(II)
C                                       Loop for all levels.
               DO 190 II = 1,NC
                  VC = C(II)
C                                       Cut down negatives
                  IF (VC.LT.0.0) THEN
                     IF ((XLEV.LT.2.85) .AND. (ITRI.EQ.2)) THEN
                        GO TO 190
                     ELSE IF (XLEV.LT.1.0) THEN
                        JJ = IROW + ICOL + II
                        IF (MOD(JJ, ISLEV).NE.0) GO TO 190
                        END IF
                     END IF
                  IF ((VC.GT.VL) .OR. ((VL-VS).LE.0.0)) GO TO 200
C                                       If level not right, next lev.
                  IF (VC.LE.VS) GO TO 190
C                                       Interpolate btwn max two corns.
                  TEMP = (VC-VS) / (VL-VS)
                  XA = TEMP * (XL-XS) + XS
                  YA = TEMP * (YL-YS) + YS
C                                       See which corners 2nd pt. btwn.
C                                       Level btwn med & small corners.
                  IF ((VC.LT.VM) .AND. (VM.NE.VS)) THEN
                     TEMP = (VC-VS) / (VM-VS)
                     XB = TEMP * (XM-XS) + XS
                     YB = TEMP * (YM-YS) + YS
C                                       Level btwn large & med corners.
                  ELSE
                     TEMP = (VC-VM) / (VL-VM)
                     XB = TEMP * (XL-XM) + XM
                     YB = TEMP * (YL-YM) + YM
                     END IF
C                                       Issue position & write commands
C                                       We can avoid position command
C                                       if we switch A and B.
                  IF ((XLAST.EQ.XB) .AND. (YLAST.EQ.YB)) THEN
                     TEMP = XA
                     XA = XB
                     XB = TEMP
                     TEMP = YA
                     YA = YB
                     YB = TEMP
C                                       See if we need to position.
                  ELSE IF ((XLAST.NE.XA) .OR. (YLAST.NE.YA)) THEN
                     CALL GPOS (XA, YA, PLBUF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
C                                       Draw vector.
                  IF (VC.GE.0.0) THEN
                     CALL GVEC (XB, YB, PLBUF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     XLAST = XB
                     YLAST = YB
                     GO TO 190
C                                       Negative contours broken
                  ELSE
                     TEMP = LOCINT * SQRT (((XB-XA)**2 + (YB-YA)**2)
     *                  / 2.0)
                     MININT = IROUND (TEMP)
                     IF (MININT.LT.2) MININT = 2
                     DELTAX = (XB - XA) / MININT
                     DELTAY = (YB - YA) / MININT
                     DO 185 I = 1,MININT,2
                        XB = XA + DELTAX
                        YB = YA + DELTAY
                        CALL GVEC (XB, YB, PLBUF, IRET)
                        IF (IRET.NE.0) GO TO 999
                        IF (I.LT.MININT-1) THEN
                           XA = XB + DELTAX
                           YA = YB + DELTAY
                           CALL GPOS (XA, YA, PLBUF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           END IF
 185                    CONTINUE
                     XLAST = XB
                     YLAST = YB
                     END IF
 190              CONTINUE
 200           CONTINUE
 250        CONTINUE
         CALL RCOPY (INPIXS, V(1,IROW), RLROW)
 300     CONTINUE
C
 999  RETURN
      END
