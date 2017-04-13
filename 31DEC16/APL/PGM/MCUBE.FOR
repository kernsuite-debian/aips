LOCAL INCLUDE 'MCUBE.INC'
      REAL      INSEQ, INDSK, IN2SEQ, IN3SEQ, OUTSEQ, OUTDSK, DOALIN,
     *   ZPIXEL(2), NPLANS, DOCONC
      HOLLERITH XINNAM(3), XINCLS(2), XOUTNM(3), XOUTCL(2)
      CHARACTER INNAM*12, INCLS*6, OUTNAM*12, OUTCLS*6, HILINE*72
      INTEGER   ISVOL, ISEQ1, ISEQ2, ISEQ3, IUSER, ISLOT1, INPLAN,
     *   NRDIM, IZPIX(2), IDLUN, IDIND, IDVOL, IDSLOT, IDSEQ, NODIM,
     *   ISVOL1, IH1LUN, HIBUFF(256), IGNAX, IHDR1(256), IHDR2(256),
     *   ISVOLA, ISLOT
      LOGICAL   IGNFRQ
      REAL      BLC(7), TRC(7), RHDR1(256), RHDR2(256)
      HOLLERITH HHDR1(256), HHDR2(256)
      DOUBLE PRECISION    DHDR1(128), DHDR2(128)
      COMMON /INPARM/ XINNAM, XINCLS, INSEQ, INDSK, IN2SEQ, IN3SEQ,
     *   XOUTNM, XOUTCL, OUTSEQ, OUTDSK, DOALIN, ZPIXEL, NPLANS, DOCONC
      COMMON /CHPARM/ INNAM, INCLS, OUTNAM, OUTCLS, HILINE
      COMMON /MCUCOM/ ISVOL, ISEQ1, ISEQ2, ISEQ3, IUSER, ISLOT1, INPLAN,
     *   NRDIM, IZPIX, IDLUN, IDIND, IDVOL, IDSLOT, IDSEQ, NODIM,
     *   ISVOL1, BLC, TRC, IH1LUN, HIBUFF, IGNAX, IGNFRQ, ISVOLA, ISLOT
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1, HHDR1, DHDR1)
      EQUIVALENCE (IHDR2, RHDR2, HHDR2, DHDR2)
LOCAL END
LOCAL INCLUDE 'MCBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SCRTCH(256)
      REAL      RBLK1(MABFSS), RBLK2(MABFSS)
      COMMON /BUFRS/ RBLK1, RBLK2, SCRTCH
LOCAL END
      PROGRAM MCUBE
C-----------------------------------------------------------------------
C! Task to create an n+1 dimensional cube from n dimensional maps
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2003, 2005, 2008-2010, 2012-2013
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
C   MCUBE (make cube) will create a Real*4 n+1 dimensional map from a
C   set of n dimensional maps.  Maps that fall in the input range
C   specified by the user but do not fit in the cube because of axis
C   or coordinate mismatches will be ignored after a warning message
C   is printed.  Planes that do not have a corresponding map will be
C   filled with blank pixels.  This program will swap 1 pixel deep
C   coordinates in the headers to produce matches and swap coordinates
C   so that the n+1 actual dimension will be in the n+1 dimension slot
C   in the header.  If the output file already exists, the task simply
C   inserts the new images into the n+1-dim image, even if the old
C   cube is in integer format (overflows are blanked).  The input
C   images may be integer or floating.
C   New (August 1985) version: supports n-dimensional going to
C   n-dimensional.
C   INPUTS: (from Aips)
C      INNAME   R(3)    the entry name for all input files.  If
C                       blank the first match consistent with the other
C                       parameters is used.
C      INCLASS  R(2)    the class of all input files.  If blank
C                       the first match consistent with the other
C                       parameters is used.
C      INSEQ    R       the first sequence number in the range of
C                       2D maps.  If zero the first match in the
C                       catalog consistent with the other parameters
C                       will be used.
C      INDISK   R       the disk volume number of the source file.
C                       If zero all disks are searched and the first
C                       match found is used.
C      IN2SEQ   R       The last sequence number in the set of
C                       2D input maps. 0 means INSEQ + NPOINTS - 1.
C      IN3SEQ   R       The increment in sequence number to use
C                       in looping from INSEQ to IN2SEQ. 0 => 1
C      OUTNAME  R(3)    the name of the new subimage file.  If
C                       blank the value in INNAME will be used.
C      OUTCLASS R(2)    the class of the new subimage file.  If
C                       blank the value in INCLASS will be used.
C      OUTSEQ   R       the sequence number for the new subimage
C                       file.  If zero the first sequence number that
C                       will produce a unique file specification is
C                       used.
C      OUTDISK  R       the disk volume number for the new subimage
C                       file.  If zero the new file will be created
C                       on the highest # disk with room.
C      AXREF    R       The Z pixel (1 relative) of the plane
C                       defined by INSEQ.  0 means 1.
C      AX2REF   R       The Z pixel (1 relative) of the plane
C                       defined by IN2SEQ.  0 => INSEQ + (NPOINTS-1) *
C                       IN3SEQ.
C      NPOINTS  R       The number of planes in the cube.
C-----------------------------------------------------------------------
      INTEGER   IRETCD, IERR, INX, OLD, I
      LOGICAL   SAVE, QUICK, SMALL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MCUBE.INC'
      INCLUDE 'MCBUFRS'
C-----------------------------------------------------------------------
C                                       Get parms, inits, etc.
      CALL CUBINI (OLD, QUICK, SCRTCH, IRETCD)
      IF (IRETCD.NE.0) GO TO 995
C                                       Do history file init stuff
      CALL CUBHIS (OLD)
C                                       Write "planes"
      I = IHDR1(KINAX) * 2
      IF (I.GT.0) I = NBPS / I
      SMALL = ((I.GT.1) .AND. (NODIM.LE.2)) .OR. (NODIM.LT.2)
      IF (SMALL) THEN
         CALL PLNWRI (OLD, IRETCD)
      ELSE
         CALL CUBWRI (OLD, IRETCD)
         END IF
      IF (IRETCD.NE.0) GO TO 900
C                                       Close output
 900  SAVE = IRETCD.EQ.0
      CALL HICLOS (IH1LUN, .TRUE., HIBUFF, IERR)
      CALL MAPCLS ('INIT', IDVOL, IDSLOT, IDLUN, IDIND, IHDR1, SAVE,
     *   SCRTCH, IERR)
C                                       Destroy new on error
      IF ((IRETCD.NE.0) .AND. (OLD.LT.0)) CALL MDESTR (IDVOL, IDSLOT,
     *   IHDR1, SCRTCH, INX, IERR)
C
 995  CALL DIETSK (IRETCD, QUICK, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE CUBINI (OLD, QUICK, SCRTCH, IRETCD)
C-----------------------------------------------------------------------
C   CUBINI performs initializing for MCUBE including creating the
C   output file if needed.  The commons /INPARM/ and /MCUCOM/ are set
C   up here.  The output cube header is placed in IHDR1 in common
C   /MAPHDR/.
C   Outputs:  OLD     I        1,2 => adding to existing file
C                              2 => N-dim to N-dim
C                              < 0 => new file, else same
C             QUICK   L        T => AIPS already restarted
C             SCRTCH  I(256)   Scratch
C             IRETCD  I        Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, MTYPE*2, CTYPE*8
      LOGICAL   QUICK
      INTEGER   OLD, SCRTCH(256), IRETCD, INPRMS, IND1, INAXIS, IERR,
     *   INX, ILUN1, IROUND, IPLINC, ISVOL2, LOCS, VALUE, KEYT, NKEY
      REAL      COOINC, TEMP, XBLC(7), XTRC(7)
      LOGICAL   F
      INCLUDE 'MCUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'MCUBE '/
      DATA ILUN1 /17/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       AIPS start up
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      INPRMS = 21
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XINNAM, SCRTCH, IRETCD)
      IF (IRETCD.GT.0) QUICK = .TRUE.
      IF (IRETCD.EQ.1) GO TO 999
      IF (QUICK) CALL RELPOP (IRETCD, SCRTCH, IERR)
      IF (IRETCD.NE.0) GO TO 999
      IRETCD = 8
      IDLUN = 16
      CALL RFILL (7, 0.0, BLC)
      CALL RFILL (7, 0.0, TRC)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (12, 1, XOUTNM, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
C                                       Get map 1
      ISVOL = IROUND (INDSK)
      ISVOL1 = ISVOL
      ISVOL2 = ISVOL
      ISEQ1 = IROUND (INSEQ)
      ISEQ2 = IROUND (IN2SEQ)
      ISEQ3 = IROUND (IN3SEQ)
      IF (ISEQ3.EQ.0) ISEQ3 = 1
      IUSER = NLUSER
      MTYPE = 'MA'
      CALL MAPOPN ('READ', ISVOL1, INNAM, INCLS, ISEQ1, MTYPE, IUSER,
     *   ILUN1, IND1, ISLOT1, IHDR2, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Does out map exist already
      OLD = -1
      IDVOL = IROUND (OUTDSK)
      IDSEQ = IROUND (OUTSEQ)
      CALL MAKOUT (INNAM, INCLS, ISEQ1, '      ', OUTNAM, OUTCLS, IDSEQ)
      IF (IDSEQ.GT.0) THEN
         MSGSUP = 32000
         MTYPE = 'MA'
         CALL MAPOPN ('WRIT', IDVOL, OUTNAM, OUTCLS, IDSEQ, MTYPE,
     *      IUSER, IDLUN, IDIND, IDSLOT, IHDR1, SCRTCH, IERR)
         MSGSUP = 0
         IF (IERR.NE.3) THEN
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR
               CALL MSGWRT (8)
               GO TO 990
            ELSE
               WRITE (MSGTXT,1010)
               CALL MSGWRT (3)
               END IF
C                                       Old exists: line up axes
            OLD = 1
            CALL MALIGN (1, DOALIN, BLC, TRC, XBLC, XTRC, NRDIM, NODIM,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Check alignment now
            CALL LUPAX (OLD, INAXIS, COOINC, IPLINC, NRDIM, NODIM,
     *         DOCONC, IGNAX, IGNFRQ, IERR)
            IF (IERR.NE.0) GO TO 980
            INPLAN = IHDR1(KINAX+NRDIM)
            IRETCD = 0
C                                       ignored axis?
            NKEY = 1
            KEYT = 4
            CALL CATKEY ('REED', IDVOL, IDSLOT, 'IGNOREAX', NKEY, LOCS,
     *         VALUE, KEYT, SCRTCH, IERR)
            IF (IERR.EQ.0) THEN
               IGNAX = VALUE
               CALL H2CHR (8, 1, RHDR2(KHCTP+2*(IGNAX-1)), CTYPE)
               CALL CHLTOU (8, CTYPE)
               IGNFRQ = CTYPE(:4).EQ.'FREQ'
               END IF
            GO TO 990
            END IF
         END IF
C                                       Old does not exist: create
      CALL COPY (256, IHDR2, IHDR1)
      IZPIX(1) = ZPIXEL(1) + 0.01
      IZPIX(2) = ZPIXEL(2) + 0.01
      INPLAN = NPLANS + 0.01
      IF (ISEQ2.EQ.0) ISEQ2 = ISEQ1 + (INPLAN-1)*ISEQ3
C                                       Get map at other end
      IF (ISEQ2.NE.ISEQ1) THEN
         MTYPE = 'MA'
         CALL MAPOPN ('READ', ISVOL2, INNAM, INCLS, ISEQ2, MTYPE, IUSER,
     *      IDLUN, IDIND, IDSLOT, IHDR2, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MAPCLS ('READ', ISVOL2, IDSLOT, IDLUN, IDIND, IHDR2, F,
     *      SCRTCH, IERR)
         END IF
C                                       Line up axes
      CALL MALIGN (2, DOALIN, BLC, TRC, XBLC, XTRC, NRDIM, NODIM, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (ISEQ2.EQ.ISEQ1) NODIM = NRDIM
      CALL LUPAX (OLD, INAXIS, COOINC, IPLINC, NRDIM, NODIM, DOCONC,
     *   IGNAX, IGNFRQ, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (IZPIX(1).EQ.0) IZPIX(1) = 1
      IF (IHDR1(KINAX+INAXIS-1).LE.1) THEN
         IF (IZPIX(2).EQ.0) IZPIX(2) = INPLAN
         IF (IZPIX(2).EQ.IZPIX(1)) THEN
            WRITE (MSGTXT,1100) IZPIX
            CALL MSGWRT (8)
            GO TO 990
         ELSE
            COOINC = COOINC / (IZPIX(2) - IZPIX(1))
            IF (INAXIS.NE.NODIM) CALL SWAPAX (IHDR1, HHDR1, RHDR1,
     *         DHDR1, INAXIS, NODIM, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
      ELSE
         IF (RHDR1(KRCIC+INAXIS-1).EQ.0.0) RHDR1(KRCIC+INAXIS-1) = 1.0
         IF (IZPIX(2).LE.0) IZPIX(2) = IZPIX(1) + COOINC /
     *      RHDR1(KRCIC+INAXIS-1)
         IF (IZPIX(2).NE.IZPIX(1)) COOINC = COOINC / (IZPIX(2)-IZPIX(1))
         IF ((COOINC.EQ.0.0) .OR. (ISEQ2.EQ.ISEQ1)) COOINC =
     *      RHDR1(KRCIC+INAXIS-1)
         END IF
      INAXIS = NODIM - 1
      CALL MAPCLS ('READ', ISVOL1, ISLOT1, ILUN1, IND1, IHDR1, F,
     *   SCRTCH, IERR)
C                                       Build into new header
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
      IHDR1(KINAX+INAXIS) = INPLAN
      CALL CHR2H (12, OUTNAM, KHIMNO, HHDR1(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, HHDR1(KHIMC))
      IHDR1(KIIMS) = IDSEQ
      DHDR1(KDCRV+INAXIS) = DHDR1(KDCRV+INAXIS) + RHDR1(KRCIC+INAXIS)
     *   * (1.0 - RHDR1(KRCRP+INAXIS))
      RHDR1(KRCIC+INAXIS) = COOINC
      RHDR1(KRCRP+INAXIS) = IZPIX(1)
      TEMP = RHDR1(KRDMX)
      RHDR1(KRDMX) = RHDR1(KRDMN)
      RHDR1(KRDMN) = TEMP
      RHDR1(KRBLK) = 0.0
C                                       Add offset to alt. ref. pixel
      RHDR1(KRARP) = RHDR1(KRARP) + ZPIXEL(1) - 1.0
C                                       Create and open
      CALL MCREAT (IDVOL, IDSLOT, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IDSEQ = IHDR1(KIIMS)
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', IDVOL, OUTNAM, OUTCLS, IDSEQ, MTYPE, IUSER,
     *   IDLUN, IDIND, IDSLOT, IHDR1, SCRTCH, IRETCD)
      IF (IRETCD.NE.0) THEN
         CALL MDESTR (IDVOL, IDSLOT, IHDR1, SCRTCH, INX, IERR)
C                                       ignored axis?
      ELSE
         CALL KEYPCP (ISVOL1, ISLOT1, IDVOL, IDSLOT, 0, ' ', IERR)
         IF (IGNAX.GT.0) THEN
            NKEY = 1
            KEYT = 4
            VALUE = IGNAX
            LOCS = 1
            CALL CATKEY ('WRIT', IDVOL, IDSLOT, 'IGNOREAX', NKEY, LOCS,
     *         VALUE, KEYT, SCRTCH, IERR)
            END IF
         END IF
      GO TO 999
C                                       File closings
 980  CALL MAPCLS ('WRIT', IDVOL, IDSLOT, IDLUN, IDIND, IHDR1, F,
     *   SCRTCH, IERR)
C
 990  CALL MAPCLS ('READ', ISVOL1, ISLOT1, ILUN1, IND1, IHDR2, F,
     *   SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPEN EXISTING CUBE ERROR',I7)
 1010 FORMAT ('Warning: writing to existing cube')
 1100 FORMAT ('PIXEL POSITIONS FOR INSEQ, IN2SEQ',2I7,' ILLEGAL')
      END
      SUBROUTINE CUBWRI (OLD, IRET)
C-----------------------------------------------------------------------
C   CUBWRI reads the n-dim images and enters them in the n+1-dim image.
C   Fills unused planes w blanks if old = false.
C   Inputs:  OLD   I          Is this old map or new, N+1 or N dim
C   Outputs: IRET  I          Error code: 0 => ok
C   Commons: RBLK1   R(*)     Scratch in common /BUFRS/
C            CATBLK  I(256)   Headers in common /MAPHDR/
C-----------------------------------------------------------------------
      CHARACTER OPTELL*4, MTYPE*2, CTYPE*8
      INTEGER   OLD, IRET, I, ISEQ, ILUN, IND, K, IPLANE, IERR, LM,
     *   IPLINC, IROUND, ICGVER, INFRAX, OUFRAX
      REAL      XMIN, XMAX, XM, DUMMY, VINC, OUAREA, INAREA, SCALE
      DOUBLE PRECISION VALUE, RVAL, DTEMP, FREQ, AFREQ, BFREQ, CFREQ
      LOGICAL   DOBLNK, F, DOCGEX
      INCLUDE 'MCUBE.INC'
      INCLUDE 'MCBUFRS'
      INTEGER   ITABLE(MAXIMG)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /18/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Check
      I = (ISEQ2 - ISEQ1) / ISEQ3 + 1
      IF (I.LE.0) THEN
         MSGTXT = 'NO PLANES TO WRITE: CHECK INSEQ, IN2SEQ, IN3SEQ'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       Inits
      IRET = 0
      XMIN = RHDR1(KRDMN)
      XMAX = RHDR1(KRDMX)
      DOBLNK = RHDR1(KRBLK) .EQ. FBLANK
C                                       Table of planes written
      I = INPLAN
      CALL FILL (I, 0, ITABLE)
      XM = ISEQ2 - ISEQ1 + 1
      IF (XM.EQ.0.0) XM = 1.0
      XM = REAL (IZPIX(2) - IZPIX(1) + 1) / XM * ISEQ3
      LM = IROUND (XM)
      CALL H2CHR (8, 1, RHDR1(KHBUN), CTYPE)
      CALL CHLTOU (8, CTYPE)
      IF ((CTYPE.EQ.'JY/BEAM') .AND. (RHDR1(KRBMJ).GT.0.0) .AND.
     *   (RHDR1(KRBMN).GT.0.0)) THEN
         OUAREA = RHDR1(KRBMJ) * RHDR1(KRBMN)
      ELSE
         OUAREA = 0.0
         END IF
      CALL AXEFND (4, 'FREQ', IHDR1(KIDIM), RHDR1(KHCTP), OUFRAX, IERR)
      IF (IERR.NE.0) CALL AXEFND (4, 'FQID', IHDR1(KIDIM), RHDR1(KHCTP),
     *   OUFRAX, IERR)
      IF (IERR.NE.0) OUFRAX = -1
C                                       Controlled loop over seq #
      ISEQ = ISEQ1 - ISEQ3
 20   ISEQ = ISEQ + ISEQ3
      IF (((ISEQ.LE.ISEQ2) .AND. (ISEQ3.GT.0)) .OR. ((ISEQ.GE.ISEQ2)
     *   .AND. (ISEQ3.LT.0))) THEN
C                                       Check TELL
         CALL GTTELL (0, OPTELL, DUMMY, SCRTCH, IERR)
         WRITE (MSGTXT,1019) OPTELL
         IF (IERR.GT.1) CALL MSGWRT (6)
         IF (IERR.EQ.2) GO TO 50
         IF (IERR.EQ.3) THEN
            IRET = 99
            GO TO 999
            END IF
C                                       Open input
         ISVOLA = ISVOL
         MTYPE = 'MA'
         CALL MAPOPN ('READ', ISVOLA, INNAM, INCLS, ISEQ, MTYPE, IUSER,
     *      ILUN, IND, ISLOT, IHDR2, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Check correspondence
         IPLANE = 0
         IF (OLD.LT.0) IPLANE = (LM * (ISEQ - ISEQ1)) / ISEQ3 + IZPIX(1)
         CALL CHKMAP (NRDIM, NODIM, DOALIN, BLC, TRC, IPLANE, IPLINC,
     *      DOCONC, IERR)
C                                       Write into "cube"
         IF (IERR.EQ.0) THEN
            CALL H2CHR (8, 1, RHDR2(KHBUN), CTYPE)
            CALL CHLTOU (8, CTYPE)
            IF ((CTYPE.EQ.'JY/BEAM') .AND. (RHDR2(KRBMJ).GT.0.0) .AND.
     *         (RHDR2(KRBMN).GT.0.0)) THEN
               INAREA = RHDR2(KRBMJ) * RHDR2(KRBMN)
            ELSE
               INAREA = 0.0
               END IF
            IF ((INAREA.GT.0.0) .AND. (OUAREA.GT.0.0)) THEN
               SCALE = OUAREA / INAREA
               DOCGEX = .TRUE.
            ELSE
               SCALE = 1.0
               DOCGEX = .FALSE.
               END IF
            CALL FNDEXT ('CG', IHDR2, ICGVER)
            CALL AXEFND (4, 'FREQ', IHDR2(KIDIM), RHDR2(KHCTP), INFRAX,
     *         IERR)
            IF (IERR.NE.0) CALL AXEFND (4, 'FQID', IHDR2(KIDIM),
     *         RHDR2(KHCTP), INFRAX, IERR)
            IF (IERR.NE.0) THEN
               INFRAX = -1
               AFREQ = 0.0D0
               BFREQ = 0.0D0
               CFREQ = 0.0D0
            ELSE
               AFREQ = RHDR2(KRCIC+INFRAX)
               BFREQ = RHDR2(KRCRP+INFRAX)
               CFREQ = DHDR2(KDCRV+INFRAX)
               END IF
C                                       history of ignored coordinate
            IF (IGNAX.GT.0) THEN
               CALL H2CHR (8, 1, RHDR2(KHCTP+2*(IGNAX-1)), CTYPE)
               VALUE = DHDR2(KDCRV+IGNAX-1) + (1.-RHDR2(KRCRP+IGNAX-1))
     *            * RHDR2(KRCIC+IGNAX-1)
               WRITE (HILINE,1010) VALUE, CTYPE, IPLANE
               CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
               IERR = 0
               IF (IGNFRQ) THEN
                  DTEMP = DHDR1(KDCRV+NODIM-1) + RHDR1(KRCIC+NODIM-1) *
     *               (IPLANE - RHDR1(KRCRP+NODIM-1))
                  I = DTEMP + 0.5D0
                  VINC = RHDR2(KRCIC+IGNAX-1)
                  RVAL = DHDR1(KDCRV+IGNAX-1)
                  CALL FQWRIT (I, VALUE, VINC, RVAL, IERR)
                  IERR = 0
                  END IF
               END IF
            DOBLNK = (DOBLNK) .OR. (RHDR2(KRBLK).EQ.FBLANK)
            K = IHDR2(KINAX+NODIM-1)
            IF ((NODIM.EQ.2) .AND. (IPLINC.EQ.1)) K = 1
            DO 25 I = 1,K
               BLC(NODIM) = I
               TRC(NODIM) = I
               IF ((NODIM.EQ.2) .AND. (IPLINC.EQ.1)) TRC(2) =
     *            IHDR2(KINAX+1)
               IF ((IPLANE.LT.1) .OR. (IPLANE.GT.IHDR1(KINAX+NODIM-1)))
     *            THEN
                  WRITE (MSGTXT,1020) I, IPLANE
                  CALL MSGWRT (6)
               ELSE
                  WRITE (MSGTXT,1021) I, IPLANE, SCALE
                  CALL MSGWRT (3)
                  CALL WRPLAN (IPLANE, NODIM, IHDR2(KINAX), BLC, TRC,
     *               ILUN, IND, IDLUN, IDIND, SCALE, XMIN, XMAX, IERR)
                  IF (IERR.LT.0) DOBLNK = .TRUE.
                  IRET = MAX (IRET, IERR)
                  IF (IERR.GT.0) GO TO 30
                  IF (ITABLE(IPLANE).EQ.1) THEN
                     WRITE (MSGTXT,1022) IPLANE, ISEQ
                     CALL MSGWRT (6)
                     END IF
                  ITABLE(IPLANE) = 1
                  IF ((DOCGEX) .AND. (NODIM.EQ.INFRAX+1)) THEN
                     FREQ = CFREQ + AFREQ * (I - BFREQ)
                     CALL MCUBCG (FREQ, RHDR2(KRBMJ), RHDR2(KRBMN),
     *                  RHDR2(KRBPA), ICGVER, IERR)
                     END IF
                  END IF
               IPLANE = IPLANE + IPLINC
 25            CONTINUE
            END IF
C                                       Close
 30      CALL MAPCLS ('READ', ISVOLA, ISLOT, ILUN, IND, IHDR2, F,
     *      SCRTCH, IERR)
         IF (IRET.NE.0) GO TO 999
         GO TO 20
         END IF
C                                       Fill in new cube with blanks
 50   IF (OLD.LE.0) THEN
         K = 0
         DO 60 I = 1,INPLAN
            IF (ITABLE(I).EQ.0) THEN
               WRITE (MSGTXT,1050) I
               CALL MSGWRT (2)
               CALL WRBLNK (I, NODIM, IDLUN, IDIND, IERR)
               DOBLNK = .TRUE.
               END IF
 60         CONTINUE
         END IF
C                                       Fill in header
      RHDR1(KRDMN) = XMIN
      RHDR1(KRDMX) = XMAX
      IF (DOBLNK)  RHDR1(KRBLK) = FBLANK
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MCUBE COORD=',1PE13.5,5X,'/ ignored on ',A8,
     *   ' axis plane',I5)
 1019 FORMAT ('Told to ',A4,' by TELL')
 1020 FORMAT ('Cannot write input plane',I5,' to output plane',I5)
 1021 FORMAT ('Writing input plane',I5,' to output plane',I5,' scale',
     *   F8.4)
 1022 FORMAT ('Map plane',I5,' overwrites previous plane.  Seq =',I5)
 1050 FORMAT ('Blanking empty output plane',I5)
      END
      SUBROUTINE CUBHIS (OLD)
C-----------------------------------------------------------------------
C   CUBHIS handles HI file chores for MCUBE
C   Inputs: OLD   I         >0 => just add some parms, else create
C-----------------------------------------------------------------------
      INTEGER   OLD, IH2LUN, IERR, TEMP(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MCUBE.INC'
C-----------------------------------------------------------------------
      CALL HIINIT (2)
      IH1LUN = 27
      IH2LUN = 28
C                                       Old - just open
      IF (OLD.GT.0) THEN
         CALL HIOPEN (IH1LUN, IDVOL, IDSLOT, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (HILINE,1000) RLSNAM
         CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       New - Create, copy old 1
      ELSE
         CALL HISCOP (IH2LUN, IH1LUN, ISVOL1, IDVOL, ISLOT1, IDSLOT,
     *      IHDR1, TEMP, HIBUFF, IERR)
         IF (IERR.GT.3) GO TO 999
         IF (IERR.EQ.3) GO TO 999
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, INNAM, INCLS, ISEQ1, ISVOL1, IH1LUN, HIBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1030) ISEQ2, ISEQ3
      CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, IDSEQ, IDVOL, IH1LUN,
     *   HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (OLD.LE.0) THEN
         WRITE (HILINE,1031) IZPIX, INPLAN
         CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      WRITE (HILINE,1035) BLC
      CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1036) TRC
      CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCUBE RELEASE= ''',A7,' ''')
 1030 FORMAT ('MCUBE IN2SEQ =',I4,'; IN3SEQ =',I4,'; INTYPE =''MA''')
 1031 FORMAT ('MCUBE AXREF =',I4,',',I4,';  NPOINTS =',I6)
 1035 FORMAT ('MCUBE BLC=',6(F5.0,','),F5.0,' / bottom left corner')
 1036 FORMAT ('MCUBE TRC=',6(F5.0,','),F5.0,' / top right corner')
      END
      SUBROUTINE CHKMAP (NRDIM, NODIM, DOALIN, XBLC, XTRC, IPLANE,
     *   IPLINC, DOCONC, IERR)
C-----------------------------------------------------------------------
C   CHKMAP (check map) will compare an n+1D image header with an nD
C   header to determine if the nD image can be a "plane" of the n+1D.
C   Inputs:
C      NRDIM        I        # real dimensions in input (nD) image
C      NODIM        I        Axis number being built
C      DOALIN       R        How to do alignment
C      COMMON /MAPHDR/       N+1-dim header, N-dim header
C   In/out: IPLANE  I        plane of nD map in n+1D map. (In: guess)
C   Output: XBLC    R(7)     BLC to apply to N-dim map (OLD now true)
C           XTRC    R(7)     Ditto
C           IPLINC  I        Increment in plane number to use 2nd image
C   Out:    IERR    I   The error code. 0=ok, 1=error.
C-----------------------------------------------------------------------
      REAL      RHDR1(256), RHDR2(256), COODIF, COOINC, EPS, PLANE,
     *   XBLC(7), XTRC(7), DOALIN, BLC(7), TRC(7), DOCONC
      INTEGER   IHDR1(256), IHDR2(256), IAXIS, IERR, INPLAN, IPLANE,
     *   IROUND, I, J, NRDIM, NODIM, LPLANE, IPLINC, IDUM
      LOGICAL   IGNFRQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1)
      EQUIVALENCE (IHDR2, RHDR2)
      DATA EPS /0.02/
C-----------------------------------------------------------------------
      IERR = 0
      LPLANE = IPLANE
      CALL RFILL (7, 0.0, BLC)
      CALL RFILL (7, 0.0, TRC)
C                                       Set initial values from header.
      COOINC = RHDR1(KRCIC+NODIM-1)
      IF (COOINC.EQ.0.0) COOINC = 1.0
      INPLAN = IHDR1(KINAX+NODIM-1)
C                                       See if axis line up.
      CALL MALIGN (1, DOALIN, BLC, TRC, XBLC, XTRC, I, J, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((I.NE.NRDIM) .OR. (J.NE.NODIM)) GO TO 980
      CALL LUPAX (1, IAXIS, COODIF, IPLINC, NRDIM, NODIM, DOCONC, IDUM,
     *   IGNFRQ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       See if plane N+1
      IF (IAXIS.NE.NODIM) GO TO 980
C                                       Calculate plane.
      IF (COOINC.EQ.0.0) COOINC = 1.0
      PLANE = COODIF / COOINC + 1.0
      IPLANE = IROUND (PLANE)
      IF (ABS(PLANE-IPLANE).GT.EPS) THEN
         IF (LPLANE.NE.IPLANE) GO TO 960
         WRITE (MSGTXT,1000) LPLANE, PLANE
         CALL MSGWRT (6)
         END IF
      IF ((IPLANE.GE.1) .AND. (IPLANE.LE.INPLAN)) THEN
         WRITE (MSGTXT,1020) IHDR2(KIIMS), IPLANE
         CALL MSGWRT (2)
         GO TO 999
         END IF
C                                       Error handling.
C                                       Bad pixel position
 960  WRITE (MSGTXT,1960) PLANE
      CALL MSGWRT (6)
      IERR = 2
      GO TO 990
C                                       Map plane does not fit in 3D mp
 980  IERR = 1
      WRITE (MSGTXT,1980)
      CALL MSGWRT (7)
C                                       Skipping map xxxx.
 990  WRITE (MSGTXT,1990) IHDR2(KIIMS)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Warning: output plane',I5,' computed as',F8.2)
 1020 FORMAT ('Writing sequence #',I5,' to plane',I5)
 1960 FORMAT ('Map plane has bad output pixel position =',F8.2)
 1980 FORMAT ('Map plane does not fit into output map.')
 1990 FORMAT ('Skipping sequence #',I4)
      END
      SUBROUTINE LUPAX (OLD, IDIFAX, COOINC, IPLINC, NRDIM, NODIM,
     *   DOCONC, IGNAX, IGNFRQ, IERR)
C-----------------------------------------------------------------------
C   LUPAX (line up axis) will check two image headers to make sure that
C   they match except for one axis which differs only in the coordinate
C   value.
C   Inputs:
C      OLD      I        > 0 IHDR1 is N+1-dim image: no swap
C                              of BLC, TRC and image 2 must align with
C                              the N+1 axis
C      NRDIM    I        The number of real (> 1 point) axes input
C      NODIM    I        The number of real (> 1 point) axes output
C      DOCONC   R        > 1 => force the SEQ.NUM axis
C   In/out: COMMON
C      IHDR1    I(256)   Image header map one.
C      IHDR2    I(256)   Image header map two.  The corresponding
C                        axis will be in alignment after LUPAX finishes.
C    OUT:
C      IDIFAX   I        The axis (1-7) that differs between the two
C                        images.
C      COOINC   R        The difference in the coordinate value on
C                        axis IDIFAX.  (on > 1 pt axes is the
C                        difference at pixel 1 on each)
C      IPLINC   I        Incr. in plane number to use on 2nd image
C      IERR     I        The error code. 0=ok, 1=error.
C-----------------------------------------------------------------------
      INTEGER   OLD, IDIFAX, IPLINC, NRDIM, NODIM, IGNAX, IERR
      REAL      COOINC, DOCONC
      LOGICAL   IGNFRQ
C
      CHARACTER CTEMP*8, CTEMP1*8, CTEMP2*8
      DOUBLE PRECISION    DHDR1(128), DHDR2(128)
      REAL      RHDR1(256), RHDR2(256), TMP
      HOLLERITH HHDR1(256), HHDR2(256)
      INTEGER   IHDR1(256), IHDR2(256), IAXIS1, IAXIS2, INDIMS, LDIFF,
     *   NERRS, INC, I, IXL, IROUND
      LOGICAL   EQUAL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1, HHDR1, DHDR1)
      EQUIVALENCE (IHDR2, RHDR2, HHDR2, DHDR2)
C-----------------------------------------------------------------------
C                                       Init
      IERR = 0
      IGNAX = 0
      IGNFRQ = .FALSE.
      IDIFAX = 0
      COOINC = 0.0
      IPLINC = 1
      INDIMS = IHDR2(KIDIM)
      IF (INDIMS.NE.IHDR1(KIDIM)) THEN
         IF (INDIMS.EQ.IHDR1(KIDIM)-1) GO TO 200
         GO TO 990
         END IF
      IF (NRDIM.LT.0) GO TO 980
      IF (NRDIM.EQ.NODIM) GO TO 150
      IF (NRDIM.NE.NODIM-1) GO TO 990
C                                       Loop for 1 pixel deep axis.
C                                       Line up 2nd map with first
      IXL = NRDIM + 1
      IF (OLD.GT.0) GO TO 100
      IF (IHDR1(KINAX+NRDIM).GT.1) GO TO 100
         DO 90 IAXIS1 = IXL,INDIMS
            DO 80 IAXIS2 = IAXIS1,INDIMS
               CALL COMPAX (IAXIS1, IAXIS2, LDIFF, NERRS)
               IF (NERRS.GT.1) GO TO 80
               IF ((NERRS.EQ.1) .AND. ((LDIFF.NE.3) .OR. (COOINC.NE.
     *            0.0))) GO TO 80
C                                       Differ by coordinate inc.
                  IF (NERRS.EQ.0) GO TO 70
                     IF (IDIFAX.GT.0) GO TO 960
                     COOINC = DHDR2(KDCRV+IAXIS2-1) + (1-RHDR2(KRCRP
     *                  +IAXIS2-1))*RHDR2(KRCIC+IAXIS2-1) -
     *                  DHDR1(KDCRV+IAXIS1-1) - (1-RHDR1(KRCRP+IAXIS1
     *                  -1))*RHDR1(KRCIC+IAXIS1-1)
                     IDIFAX = IAXIS1
 70               IF (IAXIS1.NE.IAXIS2) CALL SWAPAX (IHDR2, HHDR2,
     *               RHDR2, DHDR2, IAXIS1, IAXIS2, IERR)
                  GO TO 90
 80            CONTINUE
            GO TO 990
 90         CONTINUE
         IF (IDIFAX.LE.0) GO TO 200
         IF (DOCONC.GT.1.0) THEN
            CALL H2CHR (8, 1, RHDR2(KHCTP+2*(IDIFAX-1)), CTEMP)
            IF (IDIFAX.EQ.NRDIM+1) IDIFAX = IHDR1(KIDIM) + 1
            IGNAX = IDIFAX
            IGNFRQ = CTEMP(:4).EQ.'FREQ'
            GO TO 200
            END IF
         GO TO 999
C                                       Old
 100  CONTINUE
         IF (DOCONC.GT.1.0) GO TO 200
         INC = 2
         DO 110 IAXIS2 = IXL,INDIMS
            EQUAL = HHDR1(KHCTP+NRDIM*INC) .EQ.
     *         HHDR2(KHCTP+(IAXIS2-1)*INC)
            IF (EQUAL) GO TO 120
 110        CONTINUE
         GO TO 200
C                                       Found matching axis type
 120     COOINC = DHDR2(KDCRV+IAXIS2-1) + (1.-RHDR2(KRCRP+IAXIS2-1)) *
     *      RHDR2(KRCIC+IAXIS2-1) - DHDR1(KDCRV+NRDIM) -
     *      (1.-RHDR1(KRCRP+NRDIM))*RHDR1(KRCIC+NRDIM)
         TMP = 1.0
         IF (RHDR1(KRCIC+NRDIM).NE.0.0) TMP = RHDR2(KRCIC+IAXIS2-1) /
     *      RHDR1(KRCIC+NRDIM)
         IPLINC = IROUND (TMP)
         IF (IPLINC.EQ.0) IPLINC = 1
         IDIFAX = IXL
         IF (IAXIS2.NE.IXL) CALL SWAPAX (IHDR2, HHDR2, RHDR2,
     *      DHDR2, IXL, IAXIS2, IERR)
         GO TO 999
C                                       Two multi-point axes
 150  IDIFAX = NRDIM
      INC = KHCTP + (IDIFAX-1) * 2
      CALL H2CHR (8, 1, HHDR1(INC), CTEMP1)
      CALL H2CHR (8, 1, HHDR2(INC), CTEMP2)
      EQUAL = CTEMP1 .EQ. CTEMP2
      IF (.NOT.EQUAL) GO TO 990
      INC = NRDIM - 1
      COOINC = DHDR2(KDCRV+INC) + (1.-RHDR2(KRCRP+INC))*RHDR2(KRCIC+INC)
     *   - DHDR1(KDCRV+INC) - (1.-RHDR1(KRCRP+INC))*RHDR1(KRCIC+INC)
      TMP = 1.0
      IF (RHDR1(KRCIC+INC).NE.0.0) TMP = RHDR2(KRCIC+INC) /
     *   RHDR1(KRCIC+INC)
      IPLINC = IROUND (TMP)
      IF (IPLINC.EQ.0) IPLINC = 1
      GO TO 999
C                                       Add an MCUBE axis
 200  IF (IHDR2(KIDIM).GE.KICTPN) GO TO 970
C                                       OLD - is there an MCUBE
      IF (IHDR1(KINAX+NRDIM).GT.1) THEN
         INC = KHCTP + NRDIM * 2
         CALL H2CHR (8, 1, HHDR1(INC), CTEMP)
         IGNFRQ = CTEMP(:4).EQ.'FQID'
         IF ((.NOT.IGNFRQ) .AND. (CTEMP.NE.'SEQ.NUM.')) GO TO 970
         IAXIS1 = NRDIM
C                                       NEW: add to 1st MCUBE axis
      ELSE
         IAXIS1 = IHDR1(KIDIM)
         IHDR1(KIDIM) = IAXIS1 + 1
         IHDR1(KINAX+IAXIS1) = 1
         RHDR1(KRCRT+IAXIS1) = 0.0
         RHDR1(KRCIC+IAXIS1) = 1.0
         RHDR1(KRCRP+IAXIS1) = 1.0
         DHDR1(KDCRV+IAXIS1) = IHDR1(KIIMS)
         I = KHCTP + IAXIS1 * 2
         IF (IGNFRQ) THEN
            CALL CHR2H (8, 'FQID    ', 1, HHDR1(I))
            WRITE (MSGTXT,1210) 'FQID'
         ELSE
            CALL CHR2H (8, 'SEQ.NUM.', 1, HHDR1(I))
            WRITE (MSGTXT,1210) 'SEQ.NUM.'
            END IF
         CALL MSGWRT (4)
         END IF
C                                       Add to 2nd MCUBE axis
      IAXIS2 = IHDR2(KIDIM)
      IHDR2(KIDIM) = IAXIS2 + 1
      IHDR2(KINAX+IAXIS2) = 1
      RHDR2(KRCRT+IAXIS2) = 0.0
      RHDR2(KRCIC+IAXIS2) = 1.0
      RHDR2(KRCRP+IAXIS2) = 1.0
      DHDR2(KDCRV+IAXIS2) = IHDR2(KIIMS)
      I = KHCTP + IAXIS2 * 2
      IF (IGNFRQ) THEN
         CALL CHR2H (8, 'FQID    ', 1, HHDR2(I))
      ELSE
         CALL CHR2H (8, 'SEQ.NUM.', 1, HHDR2(I))
         END IF
C                                       take result - can't loop in
C                                       case forced
      COOINC = DHDR2(KDCRV+IAXIS2) + (1-RHDR2(KRCRP+IAXIS2)) *
     *   RHDR2(KRCIC+IAXIS2) - DHDR1(KDCRV+IAXIS1) - (1 -
     *   RHDR1(KRCRP+IAXIS1)) * RHDR1(KRCIC+IAXIS1)
      IDIFAX = IAXIS1 + 1
      IF (IAXIS1.NE.IAXIS2) CALL SWAPAX (IHDR2, HHDR2, RHDR2, DHDR2,
     *   IAXIS1+1, IAXIS2+1, IERR)
      GO TO 999
C                                       Error handling.
 960  WRITE (MSGTXT,1960)
      GO TO 995
C                                       Can't add MCUBE axis
 970  WRITE (MSGTXT,1970)
      GO TO 995
C                                       Map does not have 3rd coord.
 980  WRITE (MSGTXT,1980)
      GO TO 995
C                                       axes do not match.
 990  WRITE (MSGTXT,1990)
 995  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1210 FORMAT ('Adding a ''',A,''' axis to separate the maps')
 1960 FORMAT ('MORE THAN 1 AXIS IS DIFFERENT')
 1970 FORMAT ('AXES SIMILAR AND CANNOT BUILD AN ''SEQ.NUM.'' AXIS')
 1980 FORMAT ('MAP DOES NOT HAVE ENOUGH REAL COORDINATES')
 1990 FORMAT ('MAP AXES DO NOT MATCH')
      END
      SUBROUTINE COMPAX (IAXIS1, IAXIS2, LDIFF, NERRS)
C-----------------------------------------------------------------------
C   COMPAX (compare axis) will compare the header values for two given
C   axes in two different image headers.
C   IN:     IAXIS1  I   The axis of image header one.
C           IAXIS2  I   The axis of image two.
C   COMMON /MAPHDR/ header(1), header(2)
C   OUT:    LDIFF   I   code for last difference found.
C           NERRS   I   number or differences found.
C-----------------------------------------------------------------------
      CHARACTER CTEMP1*8, CTEMP2*8
      DOUBLE PRECISION    DHDR1(128), DHDR2(128), DEPS, D1, D2
      REAL      RHDR1(256), RHDR2(256), EPS, RXDIFF
      HOLLERITH HHDR1(256), HHDR2(256)
      INTEGER   IHDR1(256), IHDR2(256),IAXIS1, IAXIS2,
     *   IS1, IS2, NERRS, LDIFF, IXDIFF
      LOGICAL   EQUAL
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1, HHDR1, DHDR1)
      EQUIVALENCE (IHDR2, RHDR2, HHDR2, DHDR2)
      DATA EPS /0.1/
C-----------------------------------------------------------------------
C                                       Init.
      NERRS = 0
      LDIFF = 0
      IXDIFF = IHDR2(KINAX+IAXIS2-1)
      RXDIFF = RHDR2(KRCRP+IAXIS2-1)
C                                       Coordinate type.
      IS1 = (IAXIS1 - 1) * 2  +  KHCTP
      IS2 = (IAXIS2 - 1) * 2  +  KHCTP
      CALL H2CHR (8, 1, HHDR1(IS1), CTEMP1)
      CALL H2CHR (8, 1, HHDR2(IS2), CTEMP2)
      EQUAL = CTEMP1 .EQ. CTEMP2
      IF (.NOT.EQUAL) THEN
         NERRS = NERRS + 1
         LDIFF = 2
         END IF
C                                       Coordinate value at ref pixel.
      DEPS = ABS(RHDR1(KRCIC+IAXIS1-1) * EPS)
      IF (DEPS.EQ.0.0D0) DEPS = DHDR1(KDCRV+IAXIS1-1) * EPS * 1.E-6
C                                       Value on 1-pixel axes
      IF ((IHDR1(KINAX+IAXIS1-1).LE.1) .AND. (IXDIFF.LE.1)) THEN
         D1 = DHDR1(KDCRV+IAXIS1-1) + (1.-RHDR1(KRCRP+IAXIS1-1)) *
     *      RHDR1(KRCIC+IAXIS1-1)
         D2 = DHDR2(KDCRV+IAXIS2-1) + (1.-RXDIFF) * RHDR2(KRCIC+
     *      IAXIS2-1)
         IF (ABS(D1-D2).GT.DEPS) THEN
            NERRS = NERRS + 1
            LDIFF = 3
            END IF
         GO TO 50
         END IF
C                                       Reference pixel value
      IF (ABS(DHDR1(KDCRV+IAXIS1-1)-DHDR2(KDCRV+IAXIS2-1)).GT.DEPS)
     *   THEN
         NERRS = NERRS + 1
         LDIFF = 3
         END IF
C                                       Coordinate value increment.
      IF (ABS(RHDR1(KRCIC+IAXIS1-1)-RHDR2(KRCIC+IAXIS2-1)).GT.DEPS)
     *   THEN
         NERRS = NERRS + 1
         LDIFF = 4
         END IF
C                                       Coordinate reference pixel.
      IF (ABS(RHDR1(KRCRP+IAXIS1-1)-RXDIFF).GT.EPS) THEN
         NERRS = NERRS + 1
         LDIFF = 5
         END IF
C                                       Coordinate rotation angles.
 50   IF (ABS(RHDR1(KRCRT+IAXIS1-1)-RHDR2(KRCRT+IAXIS2-1)).GT.EPS)
     *   THEN
         NERRS = NERRS + 1
         LDIFF = 6
         END IF
C                                       No. of pixels on axis.
      IF (IHDR1(KINAX+IAXIS1-1).NE.IXDIFF) THEN
         NERRS = NERRS + 1
         LDIFF = 7
         END IF
C
 999  RETURN
      END
      SUBROUTINE MALIGN (NA, DOALIN, BLC1, TRC1, BLC2, TRC2, NID, NOD,
     *   IERR)
C-----------------------------------------------------------------------
C   MALIGN attempts to align 2 images: it checks the window, rearranges
C   1 point axes and adjusts at least the secondary corners.
C   Inputs: NA      I       Number corners to adjust
C           DOALIN  R       How to align: 1 fully by value
C   In/out: BLC1    R(7)    BLC of image 1, changed if NA > 1
C           TRC1    R(7)    TRC of image 1, changed if NA > 1
C   Output: BLC2    R(7)    BLC to use for image 2
C           TRC2    R(7)    TRC to use for image 2
C           NID     I       Number >1 pt axes on input
C           NOD     I       Number >1 pt axes on output
C           IERR    I       Error code: 0 => ok
C                                       1 => input window bad
C                                       2 => can't allign
C-----------------------------------------------------------------------
      CHARACTER CTEMP1*8, CTEMP2*8
      INTEGER   NA, NID, NOD, IERR
      REAL      BLC1(7), TRC1(7), BLC2(7), TRC2(7), DOALIN
      INTEGER   IHDR1(256), IHDR2(256), IER, I, J, IROUND, NP, IT
      REAL      RHDR1(256), RHDR2(256), X, EPS, ABS
      HOLLERITH HHDR1(256), HHDR2(256)
      DOUBLE PRECISION    DHDR1(128), DHDR2(128), DX
      LOGICAL   OKAY(7), REDUCE, EQUAL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MAPHDR/ IHDR1, IHDR2
      EQUIVALENCE (IHDR1, RHDR1, HHDR1, DHDR1)
      EQUIVALENCE (IHDR2, RHDR2, HHDR2, DHDR2)
      DATA EPS /0.2/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Rearrange axes
      CALL SETAX (IHDR1, HHDR1, RHDR1, DHDR1, BLC1, TRC1, NOD, IER)
      IF (IER.NE.0) GO TO 999
      CALL SETAX (IHDR2, HHDR2, RHDR2, DHDR2, BLC2, TRC2, NID, IER)
      IF (IER.NE.0) GO TO 999
C                                       check input window
      CALL WINDOW (IHDR1(KIDIM), IHDR1(KINAX), BLC1, TRC1, IER)
      IF (IER.NE.0) GO TO 999
C                                       Let's do the alignment
      REDUCE = .FALSE.
      DO 40 I = 1,7
         J = I - 1
         NP = IHDR2(KINAX+J)
         OKAY(I) = .TRUE.
         IF ((IHDR2(KIDIM).GE.I) .AND. (NP.GT.1)) GO TO 15
            BLC2(I) = 1
            TRC2(I) = 1
            GO TO 40
 15      BLC2(I) = RHDR2(KRCRP+J) - RHDR1(KRCRP+J) + BLC1(I)
         TRC2(I) = BLC2(I) - BLC1(I) + TRC1(I)
         IF (DOALIN.LT.-1.5) BLC2(I) = BLC1(I)
         IF (DOALIN.LT.-1.5) TRC2(I) = TRC1(I)
         IF (DOALIN.LE.-0.1) GO TO 20
            IF (RHDR2(KRCIC+J).EQ.0.0) GO TO 35
            DX = DHDR1(KDCRV+J) + (BLC1(I) - RHDR1(KRCRP+J)) *
     *         RHDR1(KRCIC+J)
            X = (DX - DHDR2(KDCRV+J)) / RHDR2(KRCIC+J) + RHDR2(KRCRP+J)
            BLC2(I) = IROUND (X)
            IF ((DOALIN.GE.0.01) .AND. (ABS(X-BLC2(I)).GT.EPS)) GO TO 35
            TRC2(I) = BLC2(I) + TRC1(I) - BLC1(I)
C                                       smaller subimage needed & ok?
 20      IF ((NA.LE.1) .OR. (BLC2(I).GE.1.0)) GO TO 25
            BLC1(I) = BLC1(I) + 1.0 - BLC2(I)
            IF (BLC1(I).LE.IHDR1(KINAX+J)) REDUCE = .TRUE.
            BLC2(I) = 1.0
 25      IF ((NA.LE.1) .OR. (TRC2(I).LE.NP)) GO TO 30
            TRC1(I) = TRC1(I) + NP - TRC2(I)
            IF (TRC1(I).GE.1.0) REDUCE = .TRUE.
            TRC2(I) = NP
 30      IF ((BLC1(I).GT.TRC1(I)) .OR. (BLC2(I).GT.TRC2(I))) GO TO 35
         IF ((BLC2(I).LT.1.0) .OR. (TRC2(I).GT.NP)) GO TO 35
         GO TO 40
C                                       align fails
 35      OKAY(I) = .FALSE.
 40      CONTINUE
      WRITE (MSGTXT,1040)
      IF (REDUCE) CALL MSGWRT (3)
C                                       check out the alignment
      REDUCE = .FALSE.
      DO 50 I = 1,7
         IF (OKAY(I)) GO TO 50
            IF ((NOD.LT.NID) .OR. (NOD.GT.NID+1)) GO TO 900
C                                       This must be ndim->ndim
C                                       we hope.
            IF (I.NE.NOD) GO TO 900
               REDUCE = .TRUE.
               BLC1(I) = 1.0
               TRC1(I) = IHDR1(KINAX+I-1)
               BLC2(I) = 1.0
               TRC2(I) = IHDR2(KINAX+I-1)
 50      CONTINUE
      NOD = NID + 1
      IF (REDUCE) NOD = NID
      DO 70 I = 1,NID
         J = (I-1) * 2 + KHCTP
         CALL H2CHR (8, 1, HHDR1(J), CTEMP1)
         CALL H2CHR (8, 1, HHDR2(J), CTEMP2)
         EQUAL = CTEMP1 .EQ. CTEMP2
         IF (.NOT.EQUAL) GO TO 900
         IF ((DOALIN.LE.-0.1) .AND. ((IHDR1(KINAX+NOD-1).LE.1) .OR.
     *      (I.NE.NOD))) GO TO 70
            J = I - 1
            X = 1.0
            IF (RHDR1(KRCIC+J).NE.0.0) X = RHDR2(KRCIC+J)/RHDR1(KRCIC+J)
            IT = IROUND (X)
            IF (ABS(IT-X).GT.EPS) GO TO 900
            IF ((IT.NE.1) .AND. (I.LT.NOD)) GO TO 900
            IF (DOALIN.LT.0.1) GO TO 70
               IF (ABS(RHDR1(KRCRT+J)-RHDR2(KRCRT+J)).GT.1.) GO TO 900
 70      CONTINUE
      IERR = 0
      GO TO 999
C                                       Failure
 900  WRITE (MSGTXT,1900) I
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Input maps coincident on reduced subimage only')
 1900 FORMAT ('ALIGNMENT FAILS ON AXIS',I2)
      END
      SUBROUTINE SETAX (IHDR, CATH, CATR, CATD, BLC, TRC, NRD, IERR)
C-----------------------------------------------------------------------
C   SETAX moves all 1 point axes after any multi-point axes.
C   In/out: IHDR   I(256)      Header - integer version
C           RHDR   R(256)      Header - R   version
C           CATH   H(256)      Header - H   version
C           DHDR   D(128)      Header - D   version
C           BLC    R(7)        Bottom left corner
C           TRC    R(7)        Top right corner
C   Output: NRD    I           Number multi-point axes
C           IERR   I           Error: ok => ok
C                                     10 => 1pt axis < 3 and multi-point
C                                           axis > 2.
C-----------------------------------------------------------------------
      INTEGER   IHDR(256), NRD, IERR
      REAL      CATR(256), T, BLC(7), TRC(7)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INTEGER   IND, IH, IL, I, J
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IND = IHDR(KIDIM)
 10   IH = 0
         IL = 8
         DO 15 I = 1,IND
            J = KINAX + I - 1
            IF (IHDR(J).LE.1) IL = I
            IF (IL.EQ.I) GO TO 15
               IH = I
               IF (IL.NE.8) GO TO 20
 15         CONTINUE
C                                       swap if needed and loop
 20      NRD = IH
         IF (IL.GT.IH) GO TO 999
            IF (IL.LE.2) GO TO 30
               CALL SWAPAX (IHDR, CATH, CATR, CATD, IH, IL, IERR)
               IF (IERR.NE.0) GO TO 999
               T = BLC(IL)
               BLC(IL) = BLC(IH)
               BLC(IH) = T
               T = TRC(IL)
               TRC(IL) = TRC(IH)
               TRC(IH) = T
               GO TO 10
C                                       illegal cube: i/o problem
 30   WRITE (MSGTXT,1030) IL, IH
      CALL MSGWRT (8)
      IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('1-POINT AXIS',I2,' BELOW N-POINT AXIS',I2,' USE TRANS')
      END
      SUBROUTINE PLNWRI (OLD, IRET)
C-----------------------------------------------------------------------
C   PLNWRI builds an output image on a row basis : i.e. the input images
C   are one pixel or one row only.
C   Inputs:  OLD   I      > 0 => output image pre-exists
C   Output:  IRET  I      Error code
C   Commons: RBLK1   R(*)     Scratch in common /BUFRS/
C            CATBLK  I(256)   Headers in common /MAPHDR/
C-----------------------------------------------------------------------
      CHARACTER MTYPE*2, CTYPE*8
      INTEGER   OLD, IRET, NRPS, BLKNO, LBLKNO, BLNKED, INPBLK, I, ISEQ,
     *   ILUN, IND, K, J, IPLANE, IERR, I1, LM, IPLINC, IROUND, TYPE1,
     *   TYPE2, LX, IXO, NB, ITEMP, NBPR, IPOS, IPIX
      REAL      XMIN, XMAX, XM, PIXMAX, TEMP
      DOUBLE PRECISION VALUE
      LOGICAL   DOBLNK, F
      INCLUDE 'MCUBE.INC'
      INCLUDE 'MCBUFRS'
      INTEGER   ITABLE(MAXIMG)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /18/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Inits
      IRET = 0
      XMIN = RHDR1(KRDMN)
      XMAX = RHDR1(KRDMX)
      DOBLNK = RHDR1(KRBLK) .EQ. FBLANK
C                                       Table of planes written
      I = INPLAN
      CALL FILL (I, 0, ITABLE)
      XM = ISEQ2 - ISEQ1 + 1
      IF (XM.EQ.0.0) XM = 1.0
      XM = REAL (IZPIX(2) - IZPIX(1) + 1) / XM * ISEQ3
      LM = IROUND (XM)
      BLNKED = 0
      PIXMAX = 2.0D0 ** (NBITWD-1) - 1.0D0
C                                       Blank output plane
      LX = NODIM + 1
      IF (OLD.LE.0) CALL WRBLNK (1, LX, IDLUN, IDIND, IERR)
C                                       Set up I/O
      TYPE1 = 2
      NBPR = IHDR1(KINAX) * 2
      NRPS = NBPS / NBPR
      LX = 1
      IF (NODIM.EQ.2) LX = TRC(1) - BLC(1) + 1.01
      IXO = 0
      IF (NODIM.GT.1) IXO = BLC(1) - 0.99
      LBLKNO = 0
C                                       Controlled loop over seq #
      ISEQ = ISEQ1 - ISEQ3
 20   ISEQ = ISEQ + ISEQ3
      IF ((ISEQ.GT.ISEQ2) .AND. (ISEQ3.GT.0)) GO TO 150
      IF ((ISEQ.LT.ISEQ2) .AND. (ISEQ3.LT.0)) GO TO 150
C                                       Open input
         ISVOLA = ISVOL
         MTYPE = 'MA'
         CALL MAPOPN ('READ', ISVOLA, INNAM, INCLS, ISEQ, MTYPE, IUSER,
     *      ILUN, IND, ISLOT, IHDR2, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 20
C                                       Check correspondence
         IPLANE = 0
         IF (OLD.LT.0) IPLANE = (LM * (ISEQ - ISEQ1)) / ISEQ3 + IZPIX(1)
         CALL CHKMAP (NRDIM, NODIM, DOALIN, BLC, TRC, IPLANE, IPLINC,
     *      DOCONC, IERR)
C                                       Write into "cube"
         IF (IERR.NE.0) GO TO 130
C                                       history of ignored coordinate
            IF (IGNAX.GT.0) THEN
               CALL H2CHR (8, 1, RHDR2(KHCTP+2*(IGNAX-1)), CTYPE)
               VALUE = DHDR2(KDCRV+IGNAX-1) + (1.-RHDR2(KRCRP+IGNAX-1))
     *            * RHDR2(KRCIC+IGNAX-1)
               WRITE (HILINE,1010) VALUE, CTYPE, IPLANE
               CALL HIADD (IH1LUN, HILINE, HIBUFF, IERR)
               IERR = 0
               END IF
            DOBLNK = (DOBLNK) .OR. (RHDR2(KRBLK).EQ.FBLANK)
            K = IHDR2(KINAX+NODIM-1)
            DO 125 I = 1,K
               BLC(NODIM) = I
               TRC(NODIM) = I
               IF ((IPLANE.GE.1) .AND. (IPLANE.LE.IHDR1(KINAX+NODIM-1)))
     *            GO TO 25
                  WRITE (MSGTXT,1020) I, IPLANE
                  CALL MSGWRT (6)
                  GO TO 120
 25            WRITE (MSGTXT,1025) I, IPLANE
               IF (I.GT.1) CALL MSGWRT (3)
               BLKNO = 1
               IF (NODIM.GT.1) BLKNO = (IPLANE - 1) / NRPS + 1
               IF (BLKNO.EQ.LBLKNO) GO TO 35
                  IF (LBLKNO.LE.0) GO TO 30
                     CALL ZMIO ('WRIT', IDLUN, IDIND, LBLKNO, NBPS,
     *                  RBLK1, 1, IRET)
                     IF (IRET.EQ.0) CALL ZWAIT (IDLUN, IDIND, 1, IRET)
                     IF (IRET.NE.0) GO TO 130
 30               CALL ZMIO ('READ', IDLUN, IDIND, BLKNO, NBPS,
     *               RBLK1, 1, IRET)
                  IF (IRET.EQ.0) CALL ZWAIT (IDLUN, IDIND, 1, IRET)
                  IF (IRET.NE.0) GO TO 130
                  LBLKNO = BLKNO
 35            IPOS = IPLANE - 1
               IF (NODIM.EQ.2) IPOS = (IPLANE - 1 - (BLKNO-1)*NRPS) *
     *            IHDR1(KINAX)
C                                       read input image
               TYPE2 = 2
               NB = 2 * IHDR2(KINAX)
               I1 = NBPS / NB
               IF (I1.LE.0) I1 = -(NB-1)/NBPS - 1
               NB = NBPS
               IF (I1.LT.0) NB = -I1 * NBPS
               INPBLK = 1
               IF (NODIM.LE.1) GO TO 40
                  J = BLC(2) + I - 0.99
                  IF (I1.GT.0) INPBLK = (J-1)/I1 + 1
                  IF (I1.LT.0) INPBLK = -I1 * (J-1) + 1
 40            CALL ZMIO ('READ', ILUN, IND, INPBLK, NB, RBLK2, 1, IRET)
               IF (IRET.EQ.0) CALL ZWAIT (ILUN, IND, 1, IRET)
               IF (IRET.NE.0) GO TO 130
C                                       Scale and transfer to write buf
C                                       Output floating
               DO 50 IPIX = 1,LX
                  TEMP = RBLK2(IXO+IPIX)
                  IF (TEMP.EQ.FBLANK) THEN
                     RBLK1(IPOS+IPIX) = FBLANK
                  ELSE
                     TEMP = ITEMP
                     RBLK1(IPOS+IPIX) = TEMP
                     XMIN = MIN (TEMP, XMIN)
                     XMAX = MAX (TEMP, XMAX)
                     END IF
 50               CONTINUE
C
               IF (ITABLE(IPLANE).EQ.1) THEN
                  WRITE (MSGTXT,1115) IPLANE, ISEQ
                  CALL MSGWRT (6)
                  END IF
               ITABLE(IPLANE) = 1
 120           IPLANE = IPLANE + IPLINC
 125           CONTINUE
C                                       Close
 130     CALL MAPCLS ('READ', ISVOLA, ISLOT, ILUN, IND, IHDR2, F,
     *      SCRTCH, IERR)
         IF (IRET.NE.0) GO TO 999
         GO TO 20
C                                       Check blanks
 150  K = 0
      DO 160 I = 1,INPLAN
         IF (ITABLE(I).EQ.0) THEN
            DOBLNK = .TRUE.
            GO TO 170
            END IF
 160     CONTINUE
C                                       Fill in header
 170  IF (LBLKNO.GT.0) THEN
         CALL ZMIO ('WRIT', IDLUN, IDIND, LBLKNO, NBPS, RBLK1, 1, IRET)
         IF (IRET.EQ.0) CALL ZWAIT (IDLUN, IDIND, 1, IRET)
         END IF
      DOBLNK = DOBLNK .OR. (BLNKED.GT.0)
      IF (BLNKED.GT.0) THEN
         WRITE (MSGTXT,1175) BLNKED
         CALL MSGWRT (3)
         END IF
      RHDR1(KRDMN) = XMIN
      RHDR1(KRDMX) = XMAX
      IF (DOBLNK) RHDR1(KRBLK) = FBLANK
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MCUBE COORD=',1PE13.5,5X,'/ ignored on ',A8,
     *   ' axis plane',I5)
 1020 FORMAT ('Cannot write input plane',I5,' to output plane',I5)
 1025 FORMAT ('Writing input plane',I5,' to output plane',I5)
 1115 FORMAT ('Map plane',I5,' overwrites previous plane.  Seq =',I5)
 1175 FORMAT ('Blanked',I8,' pixels to prevent overflow')
      END
      SUBROUTINE FQWRIT (IFQ, VALUE, VINC, RVAL, IERR)
C-----------------------------------------------------------------------
C   FQWRIT writes an FQ table
C   Inputs
C      IFQ      I   FQID = record number
C      VALUE    D   Frequency
C      VINC     R   increment
C      RVAL     D   Reference frequency
C   Output
C      IERR     I   Error code
C-----------------------------------------------------------------------
      INTEGER   IFQ, IERR
      DOUBLE PRECISION VALUE, RVAL
      REAL      VINC
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), VER, LUN, IFQRNO, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NUMIF, FQID, IFSIDE
      DOUBLE PRECISION IFFREQ
      REAL      IFCHW, IFTBW
      CHARACTER BNDCOD*8
      INCLUDE 'MCUBE.INC'
C-----------------------------------------------------------------------
C                                       open
      VER = 1
      LUN = 20
      NUMIF = 1
      CALL FQINI ('WRIT', BUFFER, IDVOL, IDSLOT, VER, IHDR1, LUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       write
      IF (IFQRNO.GT.IFQ) IFQRNO = IFQ
      FQID = 0
      IFFREQ = VALUE - RVAL
      IFCHW = VINC
      IFTBW = VINC
      IFSIDE = 1
      FQID = IFQ
      BNDCOD = ' '
      CALL TABFQ ('WRIT', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *   FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
C                                       close
      CALL TABFQ ('CLOS', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF, FQID,
     *   IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, VER)
C
 999  RETURN
      END
      SUBROUTINE MCUBCG (FREQ, BMAJ, BMIN, BPA, ICGVER, IERR)
C-----------------------------------------------------------------------
C   Writes CG table
C   Inputs:
C      FREQ     D   Frequency of entry
C      BMAJ     R   Major axis of input header
C      BMIN     R   Minor axis of input header
C      BPA      R   Postion angle of input header
C      ICGVER   I   Version number of GC table in input
C   Outputs:
C      IERR     I   Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQ
      REAL      BMAJ, BMIN, BPA
      INTEGER   ICGVER, IERR
C
      INTEGER   CGBUFF(512), VER, LUNCG, ICGRNO, CGKOLS(4), CGNUMV(4),
     *   TABVER, NROW, I
      REAL      BMA, BMN, BP
      DOUBLE PRECISION FF
      INCLUDE 'MCUBE.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNCG /51/
C-----------------------------------------------------------------------
      VER = 1
C                                       find in input CG table
      IF (ICGVER.GT.0) THEN
         CALL CGINI ('READ', CGBUFF, ISVOLA, ISLOT, VER, IHDR2, LUNCG,
     *      ICGRNO, CGKOLS, CGNUMV, TABVER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OLD CG TABLE'
            GO TO 990
            END IF
         NROW = CGBUFF(5)
         DO 10 I = 1,NROW
            CALL TABCG ('READ', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *         BMN, BP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ OLD CG TABLE'
               GO TO 990
            ELSE IF (IERR.EQ.0) THEN
               IF (ABS(FREQ-FF).LT.10.0D0) GO TO 20
               END IF
 10         CONTINUE
         ICGRNO = 1
         DO 15 I = 1,NROW
            CALL TABCG ('READ', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *         BMN, BP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ OLD CG TABLE'
               GO TO 990
            ELSE IF (IERR.EQ.0) THEN
               IF (ABS(FREQ-FF).LT.500.0D0) GO TO 20
               END IF
 15         CONTINUE
         WRITE (MSGTXT,1010) FREQ
         CALL MSGWRT (6)
         FF = 0.0D0
 20      CALL TABCG ('CLOS', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA,
     *      BMN, BP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSE OLD CG TABLE'
            GO TO 990
            END IF
         IF (FF.LT.10.0D0) GO TO 999
      ELSE
         FF = FREQ
         BMA = BMAJ
         BMN = BMIN
         BP  = BPA
         END IF
C                                       write line to new CG table
      CALL CGINI ('WRIT', CGBUFF, IDVOL, IDSLOT, VER, IHDR1, LUNCG,
     *   ICGRNO, CGKOLS, CGNUMV, TABVER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN NEW CG TABLE'
         GO TO 990
         END IF
      CALL TABCG ('WRIT', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA, BMN,
     *   BP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITE NEW CG TABLE'
         GO TO 990
         END IF
      CALL TABCG ('CLOS', CGBUFF, ICGRNO, CGKOLS, CGNUMV, FF, BMA, BMN,
     *   BP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSE NEW CG TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCUBCG: ERROR',I3,' ON ',A)
 1010 FORMAT ('MCUBCG: COULD NOT FIND FREQ',1PE13.6,' IN OLD CG TABLE')
      END
