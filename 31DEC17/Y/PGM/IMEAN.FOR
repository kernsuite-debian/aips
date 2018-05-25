LOCAL INCLUDE 'IMEAN.INC'
      REAL      DOHIST, PRUSER, SEQIN, DISKIN, BLC(7), TRC(7), DINVER,
     *   XNBOXS, RANGE(2), PIXAVG, PIXSTD, DOMODL, DOCAT, XLABEL, XDOPR,
     *   XDOTV, XGRCH, EXCESS(50)
      HOLLERITH XNAMEI(3), XCLASI(2), XFUNTP, XOUTFL(12)
      CHARACTER NAMEIN*12, CLASIN*6, FUNTYP*2, OUTFIL*48
      COMMON /INPARM/ DOHIST, PRUSER, XNAMEI, XCLASI, SEQIN, DISKIN,
     *   BLC, TRC, DINVER, XNBOXS, RANGE, XFUNTP, DOMODL, DOCAT, XLABEL,
     *   XDOPR, XOUTFL, XDOTV, XGRCH, PIXAVG, PIXSTD, EXCESS
      COMMON /CHPARM/ NAMEIN, CLASIN, FUNTYP, OUTFIL
LOCAL END
LOCAL INCLUDE 'GDATA.INC'
      DOUBLE PRECISION DATA(1024)
      INTEGER   NITTER, ITTER
      COMMON /GDATA/ DATA, NITTER, ITTER
LOCAL END
      PROGRAM IMEAN
C-----------------------------------------------------------------------
C! Task to calculate the mean and rms of a region in a map
C# Map-util Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2000, 2002-2004, 2006-2007, 2009-2015
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
C   Program to calculate the mean and rms of a region in a map.
C   The region is to be specified by BLC and TRC, which could be
C   set by a cursor setting on the display.
C   AIPS inputs:
C      DOHIST     R         True (1.0) means do histogram plot file.
C      USERID     R         User selection number
C      INNAME     R(3)      Catalog name of input image 12 chars.
C      INCLASS    R(3)      Catalog class of input image.(6 chars)
C      INSEQ      R         Sequence # of input image.
C      INDISK     R         Disk volume containing input image.
C      BLC        R(7)      'Bottom Left Corner' position
C      TRC        R(7)      'Top Right Corner'   position.
C      NBOXES     R         Number of boxes in histogram.
C      PIXRANGE   R(2)      Min and max values in histogram. If R(2)
C                           <= R(1) then map min and max are used.
C      FUNCTYPE   R         If 'LG' use Log10 scale for # pixels, else
C                           use linear
C      DOTV       R         > 0 => TV, else plot file
C      GRCHAN     R         graphics channel to use
C      TVCORN     R(4)      TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER MAXHIS
      PARAMETER (MAXHIS = 16384)
C
      CHARACTER ARRAY*20, PFILE*48, PGMNAM*6, STAT*4, BUNITS*8,
     *   LLCHAR(5)*4, MMCHAR(5)*4, XXCHAR*4, MTYPE*2, MSGBUF*80,
     *   ADATE*12, ATIME*8, PREFIX*5, KEYWRD(2)*8, BUN2*4, TUNITS*8
      INTEGER   IVOL, USID, SCRTCH(256), IRET, IERR, IROUND, LUN1,
     *   FIND1, CNO1, ICPNT, IPBLK(256), INBUF(MABFSS), BLKOF0, NX, NY,
     *   ININD, WIN(4), BLKOF, DEPTH(5), DEPMIN(5), I2TMP1, DEPMAX(5),
     *   ICH, ILEN, NPARMS, ISEQ, IDOHST, INBOXS, NA, MAXDEX, IVER, I3B,
     *   I4B, I5B, I6B, I7B, I3E, I4E, I5E, I6E, I7E, NXW, NYW, NBUF,
     *   IXMIN, IYMIN, IXMAX, IYMAX, I1, I2, I3, I4, I5, I6, I7, IY, IX,
     *   J, INDEX, IPLUN, IPFIND, I, INC, JJ, TXFIND, TXLUN, LENOUT,
     *   JTRIM, GRCHN, TVCHN, TVCORN(4), ID(3), IT(3), RIMAX, IPVT(3),
     *   JNPARM, INFO, JNPTS, RILOW, RIHIGH, RNP, LABEL, LOCS(2),
     *   NUMKEY, KEYTYP(2), MSGSAV, I1B, I2B, I1E, I2E, DOPRNT, LTYPE
      REAL      PINT, AREA, ZMIN, ZMAX, RBUF(MABFSS), X, Y, BLOG, BMAX,
     *   XPLANE, ARANGE(2), XRCHAR, PRANGE(2), RINT, RRANGE(2), RMAX,
     *   VALUE(2), RS, XC, YC, RC, TEMP, RX1, RX2, SS, XINT, XRANGE(2)
      DOUBLE PRECISION    S, S2, ZBAR, Z, SKYPOS(3), PARMS(3), ZRMS,
     *   FJAC(3,3), TOL, FVEC(1024), PHIS(1026), RHIS(1026), RHS, NZ,
     *   PTOT, XHIS(MAXHIS), TOTPIX, INCPIX
      LOGICAL   F, RQUICK, T, SAVE, DOLOG, FLUXFL, APPEND, TXOPEN,
     *   DOTV, PFLAG, PLOPEN, CIRCLE, DOINV, DOIT, ADOIT, XDOIT,
     *   DOACTN
      INCLUDE 'IMEAN.INC'
      INCLUDE 'GDATA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (SCRTCH, INBUF, RBUF)
      DATA LUN1, IPLUN, BLKOF0, TXLUN /16, 26, 1, 10/
      DATA PGMNAM /'IMEAN '/
      DATA T, F, APPEND /.TRUE.,.FALSE.,.TRUE./
      DATA LLCHAR, MMCHAR /'LL  ','RA  ','RA--','GLON','ELON',
     *   'MM  ','DEC ','DEC-','GLAT','ELAT'/
C-----------------------------------------------------------------------
C                                       Start up:
      PLOPEN = .FALSE.
      TXOPEN = .FALSE.
      INFO = 1
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NPARMS = 46
      CALL GTPARM (PGMNAM, NPARMS, RQUICK, DOHIST, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         IF (IRET.EQ.1) GO TO 999
            IRET = 16
            WRITE (MSGTXT,1000)
            CALL MSGWRT (9)
         END IF
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (2, 1, XFUNTP, FUNTYP)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
C                                       Interpret input
      DOPRNT = IROUND (XDOPR)
      IF (DOPRNT.EQ.0) OUTFIL = ' '
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DISKIN)
      PRUSER = NLUSER
      USID = NLUSER
      DOLOG = FUNTYP.EQ.'LG'
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (ABS(LTYPE).GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100) * 100 - LTYPE
      ELSE
         LABEL = (LABEL/100) * 100 + LTYPE
         END IF
      XLABEL = LABEL
      DOINV = DINVER.GT.0.0
      DOACTN = DOCAT.GT.0.0
C                                       circular aperture?
      JJ = IROUND (BLC(1))
      CIRCLE = JJ.EQ.-1
      IF (CIRCLE) THEN
         XC = TRC(1)
         YC = TRC(2)
         RC = BLC(2)
         BLC(1) = XC - RC
         BLC(2) = YC - RC
         TRC(1) = XC + RC
         TRC(2) = YC + RC
         END IF
C                                       Values for no histogram.
      SAVE = F
      STAT = 'READ'
      IDOHST = -1
      IF (DOHIST.GT.0.0) IDOHST = 1
      IF (DOHIST.GT.1.001) IDOHST = 2
      IF ((IDOHST.GT.0) .AND. (.NOT.DOTV)) STAT = 'HDWR'
      INBOXS = 0
C                                       Open input image:
      MTYPE = 'MA'
      CALL MAPOPN (STAT, IVOL, NAMEIN, CLASIN, ISEQ, MTYPE, USID, LUN1,
     *   FIND1, CNO1, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Check # axes:
      NA = CATBLK(KIDIM)
      CALL WINDOW (NA, CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 320
         END IF
C                                       write ACTNOISE?
      IF (DOCAT.LT.2.0) THEN
         IF (DOACTN) THEN
            TOTPIX = CATBLK(KINAX)
            TOTPIX = TOTPIX * CATBLK(KINAX+1)
            INCPIX = (TRC(1)-BLC(1)+1.0) * (TRC(2)-BLC(2)+1.0)
            IF (CIRCLE) INCPIX = INCPIX * PI / 4.0D0
            IF (DOINV) INCPIX = TOTPIX - INCPIX
            DOACTN = INCPIX.GT.0.2D0*TOTPIX
            END IF
         IF (DOACTN) THEN
            DO 10 I = 3,NA
               INCPIX = INCPIX * (TRC(I) - BLC(I) + 1.0)
               TOTPIX = TOTPIX * MAX (1, CATBLK(KINAX+I-1))
 10            CONTINUE
            DOACTN = INCPIX.GT.0.1D0*TOTPIX
            END IF
         END IF
      IF ((DOCAT.GT.0.0) .AND. (.NOT.DOACTN)) THEN
         MSGTXT = 'Window too small to write ACTNOISE keyword'
         CALL MSGWRT (6)
         END IF
C                                       Get physical unit factors:
      CALL H2CHR (8, 1, CATH(KHBUN), BUNITS)
      TUNITS = BUNITS
      CALL CHLTOU (8, TUNITS)
      XRANGE(1) = CATR(KRDMN)
      XRANGE(2) = CATR(KRDMX)
      XINT = (XRANGE(2) - XRANGE(1)) / (MAXHIS - 1.0)
      XRANGE(1) = XRANGE(1) - 0.5*XINT
      XRANGE(2) = XRANGE(2) + 0.5*XINT
C                                       Values for histogram.
      CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), ARANGE)
      RANGE(1) = ARANGE(1)
      RANGE(2) = ARANGE(2)
      INBOXS = XNBOXS + .5
      IF (INBOXS.LE.1) INBOXS = 101
      IF (INBOXS.GT.1024) INBOXS = 512
      XNBOXS = INBOXS
      MAXDEX = INBOXS + 2
      CALL DFILL (MAXDEX, 0.0D0, PHIS)
      PINT = (RANGE(2) - RANGE(1)) / (INBOXS-1)
      ARANGE(1) = RANGE(1) - 0.5*PINT
      ARANGE(2) = RANGE(2) + 0.5*PINT
      RNP = 512
      CALL DFILL (1026, 0.0D0, RHIS)
      CALL DFILL (MAXHIS, 0.0D0, XHIS)
C                                       Check header
      PIXSTD = -1.0
      PIXAVG = 0.0
      NUMKEY = 2
      KEYWRD(1) = 'ACTNOISE'
      KEYWRD(2) = 'ACTMEAN'
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CATKEY ('READ', IVOL, CNO1, KEYWRD, NUMKEY, LOCS, VALUE,
     *   KEYTYP, INBUF, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.0) .AND. (VALUE(1).GT.0.0) .AND. (VALUE(2).NE.0.0))
     *   THEN
         PIXSTD = VALUE(1)
         PIXAVG = VALUE(2)
         MSGTXT = 'Initial guess for PIXSTD taken from ACTNOISE in'
     *      // ' header'
         CALL MSGWRT (3)
      ELSE
         MSGTXT = 'Initial guess for PIXSTD taken from robust fit'
         CALL MSGWRT (3)
         CALL ROBGET (LUN1, FIND1, IERR)
         END IF
      WRITE (MSGTXT,1015) PIXAVG, PIXSTD
      CALL MSGWRT (2)
C                                       Noise histogram now
      RINT = 1.0
      RS = 0.0
      IF (PIXSTD.GT.0) THEN
         RINT = PIXSTD / 8.
         RRANGE(1) = PIXAVG - (RNP+0.5) * RINT
         RRANGE(2) = PIXAVG + (RNP+0.5) * RINT
         END IF
C                                       Add plot file to header.
      IF (IDOHST.GE.1) THEN
         SAVE = T
         IVER = 0
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', IVOL, CNO1, CATBLK, IPBLK, T, 'READ',
     *         IVER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020)
               CALL MSGWRT (7)
               IDOHST = -1
            ELSE
               PLOPEN = .TRUE.
               END IF
            END IF
         END IF
C                                       #pixels/row & #rows:
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX + 1)
      STAT = 'READ'
C                                       Set window dimensions:
      IF (DOINV) THEN
         I1B = 1
         I2B = 1
         I3B = 1
         I4B = 1
         I5B = 1
         I6B = 1
         I7B = 1
         I7E = MAX (1, CATBLK(KINAX+6))
         I6E = MAX (1, CATBLK(KINAX+5))
         I5E = MAX (1, CATBLK(KINAX+4))
         I4E = MAX (1, CATBLK(KINAX+3))
         I3E = MAX (1, CATBLK(KINAX+2))
         I2E = MAX (1, CATBLK(KINAX+1))
         I1E = MAX (1, CATBLK(KINAX))
      ELSE
         I1B = IROUND (BLC(1))
         I2B = IROUND (BLC(2))
         I3B = IROUND (BLC(3))
         I4B = IROUND (BLC(4))
         I5B = IROUND (BLC(5))
         I6B = IROUND (BLC(6))
         I7B = IROUND (BLC(7))
         I1E = IROUND (TRC(1))
         I2E = IROUND (TRC(2))
         I3E = IROUND (TRC(3))
         I4E = IROUND (TRC(4))
         I5E = IROUND (TRC(5))
         I6E = IROUND (TRC(6))
         I7E = IROUND (TRC(7))
         END IF
      WIN(1) = I1B
      WIN(2) = I2B
      WIN(3) = I1E
      WIN(4) = I2E
      NXW = WIN(3) - WIN(1) + 1
      NYW = WIN(4) - WIN(2) + 1
      NBUF = MABFSS * 2
C                                       Initialize sums:
      PTOT = 0.0D0
      S = 0.0D0
      S2 = 0.0D0
      NZ = 0.0D0
      ZMIN = CATR(KRDMX)
      IXMIN = 0
      IYMIN = 0
      ZMAX = CATR(KRDMN)
      IXMAX = 0
      IYMAX = 0
      XPLANE = (I7E - I7B + 1) * (I6E - I6B + 1) * (I5E - I5B + 1)
      XPLANE = XPLANE * (I4E - I4B + 1) * (I3E - I3B + 1)
      Z = 0.0
      DO 40 I7 = I7B,I7E
      DO 40 I6 = I6B,I6E
      DO 40 I5 = I5B,I5E
      DO 40 I4 = I4B,I4E
      DO 40 I3 = I3B,I3E
         DOIT = ((I7.LT.BLC(7)) .OR. (I7.GT.TRC(7))
     *      .OR. (I6.LT.BLC(6)) .OR. (I6.GT.TRC(6))
     *      .OR. (I5.LT.BLC(5)) .OR. (I5.GT.TRC(5))
     *      .OR. (I4.LT.BLC(4)) .OR. (I4.GT.TRC(4))
     *      .OR. (I3.LT.BLC(3)) .OR. (I3.GT.TRC(3)))
         DEPTH(1) = I3
         DEPTH(2) = I4
         DEPTH(3) = I5
         DEPTH(4) = I6
         DEPTH(5) = I7
         CALL COMOFF (NA, CATBLK(KINAX), DEPTH, BLKOF, IERR)
         BLKOF = BLKOF + BLKOF0
C                                       Initialize for read:
         CALL MINIT ('READ', LUN1, FIND1, NX, NY, WIN, INBUF, NBUF,
     *      BLKOF, IERR)
         IF (IERR.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 300
            END IF
C                                       Loop on the rows:
         DO 30 IY = I2B,I2E
C                                       Read next row:
            CALL MDISK ('READ', LUN1, FIND1, INBUF, ININD, IERR)
            IF (IERR.NE.0) THEN
               RQUICK = .FALSE.
               GO TO 300
               END IF
C                                       Process current row:
            IF (DOINV) THEN
               ADOIT = (DOIT) .OR. (IY.LT.BLC(2)) .OR. (IY.GT.TRC(2))
            ELSE
               ADOIT = .FALSE.
               END IF
            IF (CIRCLE) THEN
               TEMP = RC * RC - (IY - YC)**2
               TEMP = SQRT (MAX (0.0, TEMP))
               RX1 = XC - TEMP
               RX2 = XC + TEMP
            ELSE
               RX1 = BLC(1)
               RX2 = TRC(1)
               END IF
            DO 20 IX = I1B,I1E
               J = ININD + IX - I1B
               Z = RBUF(J)
               XDOIT = ADOIT
               IF (.NOT.XDOIT) THEN
                  IF (DOINV) THEN
                     XDOIT = (IX.LT.RX1) .OR. (IX.GT.RX2)
                  ELSE
                     XDOIT = (IX.GE.RX1) .AND. (IX.LE.RX2)
                     END IF
                  END IF
               IF ((XDOIT) .AND. (RBUF(J).NE.FBLANK)) THEN
                  IF (RBUF(J).EQ.0.0) THEN
                     NZ = NZ + 1.0D0
                  ELSE
                     IF (Z.LE.ZMIN) THEN
                        ZMIN = Z
                        IXMIN = IX
                        IYMIN = IY
                        CALL COPY (5, DEPTH, DEPMIN)
                        END IF
                     IF (Z.GE.ZMAX) THEN
                        ZMAX = Z
                        IXMAX = IX
                        IYMAX = IY
                        CALL COPY (5, DEPTH, DEPMAX)
                        END IF
                     S  = S + Z
                     S2 = S2 + Z * Z
                     PTOT = PTOT + 1.0D0
C                                      Collect histogram data.
                     IF (PIXSTD.GT.0.0) THEN
                        X = (Z - RRANGE(1)) / RINT + 2.0
C                                      Overflow, underflow, specl case.
                        IF (X.LT.1) X = 1
                        IF (X.GT.1026) X = 1026
                        INDEX = X
                        IF (Z.EQ.RRANGE(2)) INDEX = 1025
                        RHIS(INDEX) = RHIS(INDEX) + 1.0D0
                        END IF
C                                      Collect plot histogram data.
                     X = (Z - ARANGE(1)) / PINT + 2.0
C                                      Overflow, underflow, specl case.
                     IF (X.LT.1) X = 1
                     IF (X.GT.MAXDEX) X = MAXDEX
                     INDEX = X
                     IF (Z.EQ.ARANGE(2)) INDEX = MAXDEX - 1
                     PHIS(INDEX) = PHIS(INDEX) + 1.0D0
C                                       collect total histogram
                     X = (Z - XRANGE(1)) / XINT + 1.0
                     INDEX = X + 0.001
                     INDEX = MAX (1, MIN (MAXHIS, INDEX))
                     XHIS(INDEX) = XHIS(INDEX) + 1.0D0
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
      IF (PTOT.LE.0.0D0) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         CALL MSGWRT (7)
         RQUICK = .FALSE.
         GO TO 300
         END IF
      IRET = 0
C                                       analyze total histogram
C                                       make cumulative
      Z = 0.0D0
      DO 50 I = MAXHIS,1,-1
         Z = Z + XHIS(I)
         XHIS(I) = Z / PTOT * 100.0D0
 50      CONTINUE
      XRANGE(1) = XRANGE(1) + 0.5*XINT
      XRANGE(2) = XRANGE(2) - 0.5*XINT
      JJ = 1
      DO 60 I = MAXHIS,1,-1
 55      IF (XHIS(I).GT.JJ) THEN
            IF (I.EQ.MAXHIS) THEN
               EXCESS(JJ) = XRANGE(2)
            ELSE
               Z = I + (XHIS(I)-JJ) / (XHIS(I) - XHIS(I+1))
               EXCESS(JJ) = XRANGE(1) + Z * XINT
               END IF
            JJ = JJ + 1
            IF (JJ.GT.50) GO TO 65
            GO TO 55
            END IF
 60      CONTINUE
C                                       Get mean & rms:
 65   ZBAR = S / PTOT
      ZRMS = S2 / PTOT - ZBAR * ZBAR
      ZRMS = MAX (0.0D0, ZRMS)
      ZRMS = SQRT (ZRMS)
C                                       Look over histogram
      IF (PIXSTD.GE.0.0) THEN
         ITTER = 0
         NITTER = 100
         RIMAX = -1
         RMAX = -1
         RHS = RHIS(1) + RHIS(1026)
         DO 70 I = 2,1025
            RHS = RHS + RHIS(I)
            IF (RHIS(I).GT.RMAX) THEN
               RIMAX = I
               RMAX = RHIS(I)
               END IF
 70         CONTINUE
         IF ((RIMAX.LE.3) .OR. (RIMAX.GE.1024)) GO TO 95
         JNPTS = 0
         RILOW = 0
         RIHIGH = RIMAX + 1
         RMAX = 0.3 * RMAX
         DO 75 I = 1,1024
            I3 = MIN (RIMAX + I, 1025)
            I4 = MAX (RIMAX - I, 2)
            IF ((RHIS(I3).LT.RMAX) .AND. (RHIS(I4).LT.RMAX)) GO TO 80
            IF (RHIS(I4).GE.RMAX) RILOW = I4
            IF (RHIS(I3).GE.RMAX) RIHIGH = I3
 75         CONTINUE
         IF (RIHIGH-RILOW.GT.700) GO TO 95
         RS = 0.0
 80      DO 85 I = RILOW,RIHIGH
            JNPTS = JNPTS + 1
            DATA(JNPTS) = RHIS(I)
            RS = RS + RHIS(I)
 85         CONTINUE
         IF (JNPTS.LT.4) GO TO 95
         IF (RS.LT.0.1*RHS) GO TO 95
         PARMS(1) = RMAX / 0.3
         PARMS(2) = RIMAX - RILOW + 1
         PARMS(3) = 1.3 * (RIHIGH - RILOW + 1.0) / 2.0
C                                       call fitting routine
         I = 1
         CALL XGFUNC (JNPTS, 3, PARMS, FVEC, FJAC, I)
         TOL = 1.D-5
         JNPARM = 3
         CALL XGALMS (JNPTS, JNPARM, PARMS, FVEC, FJAC, 3,
     *      TOL, INFO, IPVT)
         IF ((INFO.LE.0) .OR. (INFO.GT.3)) THEN
            INFO = 1
         ELSE
            INFO = 0
            PARMS(2) = PARMS(2) - 1.0 + RILOW
            PARMS(2) = (PARMS(2) - 1.5) * RINT + RRANGE(1)
            PARMS(3) = PARMS(3) * RINT / SQRT (8.0 * LOG (2.0))
            PIXAVG = PARMS(2)
            PIXSTD = PARMS(3)
            END IF
         IF (INFO.NE.0) PARMS(1) = 0.0D0
C                                       Histogram fails
 95      IF (INFO.NE.0) THEN
            MSGTXT = 'Histogram fit for RMS fails: plot a histogram'
            CALL MSGWRT (6)
            PIXAVG = 0.0
            PIXSTD = -1.0
            END IF
         END IF
      IF (RQUICK) THEN
         CALL PTPARM (52, PIXAVG, SCRTCH, IERR)
         CALL RELPOP (0, SCRTCH, IERR)
         END IF
C                                       Open output file
      LENOUT = JTRIM (OUTFIL)
C                                       If no file, do not write
      IF (LENOUT.LE.0) THEN
         TXLUN = 0
      ELSE
         CALL ZTXOPN ('WRIT', TXLUN, TXFIND, OUTFIL(1:LENOUT),
     *      APPEND, IERR)
         IF (IERR.EQ.0) THEN
            TXOPEN = T
            IF (DOPRNT.EQ.-3) TXLUN = -TXLUN
         ELSE
            IRET = 4
            WRITE (MSGTXT,1128) IERR
            CALL MSGWRT (6)
            TXLUN = 0
            END IF
         END IF
C                                       Initial print out
      IF (IDOHST.LT.1) DOLOG = .FALSE.
C                                       Histogram
      IF (IDOHST.GE.1) THEN
         CALL ZPHFIL ('PL', IVOL, CNO1, IVER, PFILE, IERR)
         CALL GINIT (IVOL, CNO1, PFILE, 0, 7, NPARMS, DOHIST, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, IPBLK, IPLUN, IPFIND, IERR)
         IF (IERR.NE.0) GO TO 120
C                                       Get max
         BMAX = -10.0
         BLOG = ALOG10 (0.5)
         DO 110 I = 1,INBOXS
            IF (DOLOG) THEN
               IF (PHIS(I+1).LE.0.0D0) THEN
                  PHIS(I+1) = BLOG
               ELSE
                  PHIS(I+1) = DLOG10 (PHIS(I+1))
                  END IF
               END IF
            IF (PHIS(I+1).GT.BMAX) BMAX = PHIS(I+1)
 110        CONTINUE
         IF (DOMODL.LE.0.0) PARMS(1) = 0.0D0
         IF (IDOHST.EQ.1) THEN
            CALL HISTOY (BMAX, PHIS, INBOXS, ARANGE, DOLOG, LABEL, IVER,
     *         PARMS, IPBLK, IERR)
         ELSE
            CALL HISTOX (BMAX, PHIS, INBOXS, ARANGE, DOLOG, LABEL, IVER,
     *         PARMS, IPBLK, IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (5)
            END IF
         CALL GFINIS (IPBLK, IERR)
C                                       Successful plot file finished.
         IF (IERR.EQ.0) THEN
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (IVOL, CNO1, IVER, IPBLK, IERR)
               WRITE (MSGTXT,1115) IVER
               CALL MSGWRT (5)
               IERR = 0
               END IF
            END IF
C                                       Error doing histogram.
 120     IF (IERR.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1120)
            CALL MSGWRT (6)
            IF (.NOT.DOTV) THEN
               CALL ZCLOSE (IPLUN, IPFIND, IERR)
               CALL ZDESTR (IVOL, PFILE, IERR)
               END IF
            END IF
         END IF
C                                       Write to screen and log file
      MSGBUF = '**************************************************' //
     *   '******************************'
      CALL PWRITE (-1, TXLUN, TXFIND, MSGBUF)
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      MSGBUF = 'IMEAN run on ' // ADATE // ' at ' // ATIME
      CALL PWRITE (-1, TXLUN, TXFIND, MSGBUF)
      WRITE (MSGBUF,1125) NAMEIN, CLASIN, ISEQ, IVOL, WIN
      CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
C                                       Calculate flux density?
      FLUXFL = (TUNITS(:4).EQ.'JY/B') .OR. (TUNITS(:4).EQ.'JY/P')
C                                       unweighted print out
      IF (INFO.EQ.0) THEN
         MSGBUF = 'Mean and rms found by fitting peak in histogram:'
         CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         WRITE (MSGBUF,1127) PARMS(2), PARMS(3)
         CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         PIXAVG = PARMS(2)
         PIXSTD = PARMS(3)
         IF (DOACTN) THEN
            NUMKEY = 2
            KEYWRD(1) = 'ACTNOISE'
            VALUE(1) = PARMS(3)
            KEYWRD(2) = 'ACTMEAN'
            VALUE(2) = PARMS(2)
            LOCS(1) = 1
            LOCS(2) = 2
            KEYTYP(1) = 2
            KEYTYP(2) = 2
            CALL CATKEY ('WRIT', IVOL, CNO1, KEYWRD, NUMKEY, LOCS,
     *         VALUE, KEYTYP, INBUF, IERR)
            END IF
         END IF
      MSGBUF = 'Mean and rms found by including all data:'
      CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
      WRITE (MSGBUF,1126) ZBAR, ZRMS, BUNITS, PTOT
      CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
      WRITE (MSGBUF,1129) NZ
      IF (NZ.GT.0) CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
      SS = -1.E10
      IF (FLUXFL) THEN
         IF (TUNITS.EQ.'JY/BEAM ') BUNITS = 'JY/B'
         IF (TUNITS.EQ.'JY/PIXEL') BUNITS = 'JY/P'
         BUN2 = BUNITS(5:8)
         J = JTRIM(BUN2)
         J = MAX (1, J)
         AREA = 1.1331 * CATR(KRBMJ) * CATR(KRBMN)
         IF (TUNITS(:4).EQ.'JY/P') THEN
            WRITE (MSGBUF,1131) S, BUN2(:J)
            CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         ELSE IF (AREA.GT.0.0) THEN
            J = CATBLK(KIDIM)
            I1 = 0
            I2 = 0
            INC = 2
            DO 130 I = 1,J
               ICPNT = KHCTP+(I-1)*INC
               CALL H2CHR (4, 1, CATH(ICPNT), XXCHAR)
               DO 130 JJ = 1,5
                  IF (XXCHAR.EQ.LLCHAR(JJ)) I1 = I
                  IF (XXCHAR.EQ.MMCHAR(JJ)) I2 = I
 130           CONTINUE
            IF ((I1.GT.0) .AND. (I2.GT.0)) THEN
               XRCHAR = ABS (CATR(KRCIC+I1-1) * CATR(KRCIC+I2-1))
               IF (XRCHAR.GT.0.0) THEN
                  AREA = AREA / XRCHAR
                  SS = S / AREA
                  WRITE (MSGBUF,1130) SS, BUN2(:J), AREA
                  CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
                  END IF
               END IF
            END IF
         END IF
C                                       special print out
      IF ((TXLUN.LT.0) .AND. (.NOT.DOINV) .AND. (INFO.EQ.0)) THEN
         IF (SS.GT.-1.E5) THEN
            WRITE (MSGBUF,1135) I3B, I4B, I5B, I6B, I7B, I3E, I4E, I5E,
     *         I6E, I7E, ZMIN, ZMAX, SS, PIXSTD
         ELSE
            WRITE (MSGBUF,1136) I3B, I4B, I5B, I6B, I7B, I3E, I4E, I5E,
     *         I6E, I7E, ZMIN, ZMAX, PIXSTD
            END IF
         CALL PWRITE (-5, -TXLUN, TXFIND, MSGBUF)
         END IF
C                                          if > 2 axes print all
      IF (NA.GT.2) THEN
         I2TMP1 = NA - 2
         WRITE (MSGBUF,1140) ZMIN, IXMIN, IYMIN, (DEPMIN(I),
     *      I = 1,I2TMP1)
      ELSE
         WRITE (MSGBUF,1140) ZMIN, IXMIN, IYMIN
         END IF
      CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
      LOCNUM = 1
      CALL SETLOC (DEPMIN, T)
      X = IXMIN
      Y = IYMIN
      CALL XYVAL (X, Y, SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1141) IERR
         CALL MSGWRT (7)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL
     *      AXSTRN (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM),
     *      NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
         WRITE (MSGBUF,1142)
         ICH = 9
         DO 145 I = 1,2
            I2TMP1 = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I2TMP1, ILEN, ARRAY)
            MSGBUF(ICH:) = ARRAY(:ILEN)
            ICH = ICH + ILEN + 2
 145     CONTINUE
         CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         END IF
C                                       Secondary axes
      IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
         IF ((XPLANE.GE.1.9) .OR. (AXTYP(LOCNUM).EQ.2) .OR.
     *      (AXTYP(LOCNUM).EQ.3)) THEN
            WRITE (MSGBUF,1142)
            ICH = 9
            DO 155 I = 1,2
               IF (NCHLAB(I,LOCNUM).GT.0) THEN
                  MSGBUF(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
                  ICH = ICH + NCHLAB(I,LOCNUM) + 2
                  END IF
 155           CONTINUE
            CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
            END IF
         END IF
C                                       Maximum point
      IF (NA.GT.2) THEN
         I2TMP1 = NA - 2
         WRITE (MSGBUF,1160) ZMAX, IXMAX, IYMAX, (DEPMAX(I),
     *      I = 1,I2TMP1)
      ELSE
         WRITE (MSGBUF,1160) ZMAX, IXMAX, IYMAX
         END IF
      CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
      LOCNUM = 2
      CALL SETLOC (DEPMAX, T)
      X = IXMAX
      Y = IYMAX
      CALL XYVAL (X, Y, SKYPOS(1), SKYPOS(2), SKYPOS(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGBUF,1141) IERR
         CALL MSGWRT (7)
      ELSE
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         WRITE (MSGBUF,1142)
         ICH = 9
         DO 165 I = 1,2
            I2TMP1 = I-1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I2TMP1, ILEN, ARRAY)
               MSGBUF(ICH:) = ARRAY(:ILEN)
            ICH = ICH + ILEN + 2
 165        CONTINUE
         CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         END IF
C                                       Secondary axes
      IF ((NCHLAB(1,LOCNUM).GT.0) .OR. (NCHLAB(2,LOCNUM).GT.0)) THEN
         WRITE (MSGBUF,1142)
         ICH = 9
         DO 170 I = 1,2
            IF (NCHLAB(I,LOCNUM).GT.0) THEN
               MSGBUF(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
               ICH = ICH + NCHLAB(I,LOCNUM) + 2
               END IF
 170        CONTINUE
         CALL PWRITE (5, TXLUN, TXFIND, MSGBUF)
         END IF
C                                       Histogram to file
      IF (TXLUN.GT.0) THEN
         IF (DOLOG) THEN
            BLOG = ALOG10 (0.5)
            DO 175 I = 1,INBOXS
               IF (PHIS(I+1).EQ.BLOG) THEN
                  PHIS(I+1) = 0.0D0
               ELSE
                  PHIS(I+1) = 10.0 ** PHIS(I+1)
                  END IF
 175           CONTINUE
            END IF
         X = ARANGE(2) - ARANGE(1)
         X = MAX (X, ABS(ARANGE(1)), ABS(ARANGE(2)))
         Y = X
         CALL METSCL (LABEL, X, PREFIX, PFLAG)
         IF (Y.NE.0.0) Y = X / Y
         IF (Y.EQ.0) Y = 1.0
         PINT = PINT * Y
         ARANGE(1) = ARANGE(1) * Y
         ARANGE(2) = ARANGE(2) * Y
         WRITE (MSGBUF,1175) PHIS(1), ARANGE(1), PREFIX, BUNITS
         CALL PWRITE (-1, TXLUN, TXFIND, MSGBUF)
         DO 180 I = 1,INBOXS
            PRANGE(2) = ARANGE(1) + I * PINT
            PRANGE(1) = PRANGE(2) - PINT
            WRITE (MSGBUF,1176) PHIS(I+1), PRANGE, PREFIX, BUNITS
            CALL PWRITE (-1, TXLUN, TXFIND, MSGBUF)
 180        CONTINUE
         WRITE (MSGBUF,1180) PHIS(INBOXS+2), ARANGE(2), PREFIX, BUNITS
         CALL PWRITE (-1, TXLUN, TXFIND, MSGBUF)
         END IF
C                                       Close input file:
C                                       error after plot file
 300  IF ((IRET.EQ.0) .OR. (.NOT.PLOPEN)) GO TO 320
         CALL ZCLOSE (LUN1, FIND1, IERR)
         CALL DELEXT ('PL', IVOL, CNO1, 'READ', CATBLK, SCRTCH, IVER,
     *      IERR)
         GO TO 990
 320  CALL MAPCLS (STAT, IVOL, CNO1, LUN1, FIND1, CATBLK, F,
     *   SCRTCH, IERR)
C                                       That's all folks!
 990  TXLUN = ABS (TXLUN)
      IF ((TXOPEN) .AND. (TXLUN.GT.0)) CALL ZTXCLS (TXLUN, TXFIND, IERR)
      IF (.NOT.RQUICK) CALL PTPARM (52, PIXAVG, SCRTCH, IERR)
      CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('GET PARAMETER ERR=',I5)
 1010 FORMAT ('ERROR ON OPEN: IERR=', I5)
 1015 FORMAT ('Guess is Mean=',1PE11.4,'  Rms=',1PE11.4)
 1020 FORMAT ('EXT CREATE ERROR - WILL SKIP HISTOGRAM')
 1110 FORMAT ('ERROR. WILL TRY TO FINISH PARTIAL GRAPH.')
 1115 FORMAT ('Successful plot file (histogram) version',I7)
 1120 FORMAT ('ERROR DOING HISTOGRAM. DESTROYING PLOT FILE.')
 1125 FORMAT ('Image= ',A12,'.',A6,'.',I4,I2,3X,'xywind=',4I5)
 1126 FORMAT ('Mean=',1PE10.3,' Rms=',1PE10.3,1X,A8,' over',
     *   0PF13.0,' pixels')
 1127 FORMAT ('Mean=',1PE11.4,' Rms=',1PE11.4,'  **** from histogram')
 1128 FORMAT ('ERROR ON OUTPUT FILE OPEN: ERR=',I5)
 1129 FORMAT ('Ignored',F14.0,' pure-zero pixels')
 1130 FORMAT ('Flux density =',1PE12.4,' Jy',A,'  beam area =',
     *     0PF7.2, ' pixels')
 1131 FORMAT ('Flux density =',1PE12.4,' Jy',A)
 1135 FORMAT (I5,I3,3I2,I6,I3,3I2,4(1PE12.4))
 1136 FORMAT (I5,I3,3I2,I6,I3,3I2,2(1PE12.4),12X,1PE12.4)
 1140 FORMAT ('Minimum=',1PE11.4,' at',7I5)
 1141 FORMAT ('ERROR',I3,' CONVERTING TO SKY COORDINATES')
 1142 FORMAT ('Skypos: ')
 1160 FORMAT ('Maximum=',1PE11.4,' at',7I5)
 1175 FORMAT (F14.0,' pixels below  ',F10.4,11X,2A)
 1176 FORMAT (F14.0,' pixels between',2F10.4,1X,2A)
 1180 FORMAT (F14.0,' pixels above  ',F10.4,11X,2A)
      END
      SUBROUTINE HISTOY (BMAX, PHIS, NBOXES, RANGE, DOLOG, LABEL, IVER,
     *   PARMS, IPBLK, IERR)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a histogram.  Flux on Y axis, count on X axis.
C   Inputs:
C      BMAX     R        the maximum value in any of the boxes.
C      PHIS     D(NBOXES+2)   The number of entries in each range.
C                        PHIS(1) is the underflow, PHIS(NBOXES+2) is
C                        the overflow.
C      NBOXES   I        number of boxes or value ranges for histogram.
C      RANGE    R(2)     intensity range of histogram
C      DOLOG    L        T => use log(n) rather than linear scale
C      LABEL    I        Type of labeling
C      IVER     I        Plot file version number
C      PARMS    D(4)     Fit parameters (1) = 0 => bad
C   In/out:
C      IPBLK    I(256)   I/O buffer for open, initialized pl file.
C   Output:
C      IERR     I        error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      REAL      BMAX, RANGE(2)
      DOUBLE PRECISION PARMS(3), PHIS(*)
      INTEGER   NBOXES, LABEL, IVER, IPBLK(256), IERR
      LOGICAL   DOLOG
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, XYRATO, EXF, BFACT
      INTEGER   IDEPTH(5), I, LTYPE
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set character offsets.
      CALL GTICNT (LABEL, RANGE, I)
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = CH(2) + 3 * 1.333
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.2) THEN
         CH(3) = 4.0
         IF (NBOXES.GE.10) CH(3) = CH(3) + 1.0
         IF (NBOXES.GE.100) CH(3) = CH(3) + 1.0
         IF (NBOXES.GE.1000) CH(3) = CH(3) + 1.0
         END IF
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = 2.0
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = 0.0
      IF (DOLOG) BLC(1) = ALOG10 (0.5)
      BLC(2) = 0.0
      TRC(2) = NBOXES
      TRC(1) = BMAX * 1.05
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       Kludge to keep XYRATO small
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      DO 50 I = 1,10000
         IF (XYRATO.GT.0.50) GO TO 60
         FAC = FAC * 10.0
         XYRATO = XYRATO * 10.0
 50      CONTINUE
C
 60   TRC(1) = TRC(1) / FAC
      BLC(1) = BLC(1) / FAC
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (1, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Labeling.
      I = NBOXES + 2
      CALL HLABY (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER, PHIS(1),
     *   PHIS(I), LABEL, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Position at first data point.
      TXTMSG = 'End labeling, draw histogram'
      CALL GCOMNT (-1, TXTMSG, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (2, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      X = BLC(1)
      Y = BLC(2)
      CALL GPOS (X, Y, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop for rest of data points.
      DO 100 I = 1,NBOXES
         X = PHIS(I+1) / FAC
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         Y = I
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         X = BLC(1)
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Plot Gaussian fit
      IF (PARMS(1).GT.0.0) THEN
         TXTMSG = 'Draw gaussian fit'
         CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GLTYPE (4, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         EXF = NBOXES / 1000.0
         BFACT = BMAX
         IF (DOLOG) BFACT = 10.0 ** BMAX
         DO 200 I = 1,1001
            Y = RANGE(1) + (I-1.0) * (RANGE(2)-RANGE(1)) / 1000.0
            Y = (Y - PARMS(2)) / PARMS(3)
            Y = Y * Y / 2.0
            IF (Y.LE.69.) THEN
               X = BFACT * EXP (-Y)
            ELSE
               X = 0.
               END IF
            IF (DOLOG) X = ALOG10 (MAX (0.5, X))
            Y = (I - 1) * EXF
            X = X / FAC
            IF (I.EQ.1) THEN
               CALL GPOS (X, Y, IPBLK, IERR)
            ELSE
               CALL GVEC (X, Y, IPBLK, IERR)
               END IF
            IF (IERR.NE.0) GO TO 999
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE HLABY (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER,
     *   UNDER, OVER, LABEL, IPBLK, IERR)
C-----------------------------------------------------------------------
C   Write labeling for histogram.
C   Inputs:
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C      IPBLK   I(256)   I/O buffer for plot file.
C   Output:
C      IERR    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC, RANGE(2)
      INTEGER   NBOXES, IVER, LABEL, IPBLK(256), IERR
      LOGICAL   DOLOG
      DOUBLE PRECISION UNDER, OVER
C
      CHARACTER PREFIX*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      REAL      XINTER(21), DCX0, DCX, DCY, XNOINT, DIST, ODIST, XDIST,
     *   XMAX, TICSCL, YTICEL, YTICER, XVAL, XPOS, Y, TICLEN, XINT, X,
     *   FREQ, DCXM
      INTEGER   INOINT, INCHAR, I, IXO, M, ITRY, NXRA, NXDEC, NXLL,
     *   NXMM, NXFR, NXST, NAX, INC, IANGL, JSTOK, IT(3), ID(3),
     *   ICPNT, ITMP, LTYPE
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TICSCL /70.0/
      DATA CSTOK /'????','Beam','Ipol','Qpol','Upol','Vpol','Ppol',
     *   'Fpol','Pang','Spix','Optd','    '/
      DATA XINTER /.1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200.,
     *   500., 1000., 2000., 5000., 10000., 20000., 50000., 100000.,
     *   200000., 500000./
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.LE.1) GO TO 999
C                                       Tic positions.
      TICLEN = (TRC(2) - BLC(2)) / TICSCL
      YTICEL = BLC(2) + TICLEN
      YTICER = TRC(2) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(1) - BLC(1))
      XINT = 6.0
      IF (DOLOG) XINT = 9.0
      DO 20 I = 1,21
         XNOINT = AINT (DIST/XINTER(I))
         IF (XNOINT.LE.XINT) GO TO 30
 20      CONTINUE
      GO TO 110
C                                       Interval and no of inter found.
 30   XINT = XINTER(I)
      INOINT = XNOINT + 2.5
      XVAL = AINT (FAC*BLC(1)/XINT) * XINT
      IF (XVAL.GT.FAC*BLC(1)) XVAL = XVAL - XINT
         IXO = I
C                                       Loop for all tics.
      DO 100 I = 1,INOINT
         XVAL = XVAL + XINT
         XPOS = XVAL / FAC
         IF (XPOS.GT.TRC(1)) GO TO 110
C                                       top tic.
         CALL GPOS (XPOS, TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (XPOS, YTICER, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       bottom tic.
         CALL GPOS (XPOS, YTICEL, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (XPOS, BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Write value.
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.3) INCHAR = INCHAR - 2
            DCX = - INCHAR + 0.5
            DCY = -1.5
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Number of pixels
 110  DCY = -2.833
      IF (LTYPE.EQ.2) DCY = -1.5
      XPOS = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (XPOS, BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DOLOG) THEN
         MSGBUF = 'Log10 (number of pixels)'
         INCHAR = 24
      ELSE
         MSGBUF = 'Number of pixels'
         INCHAR = 16
         END IF
      DCX = -INCHAR/2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Write bucket numbers on right
      DCX0 = 0.5
      IF (LTYPE.GT.2) THEN
         DCY = - 0.5
         DCX0 = 1.5
         IF (NBOXES.GE.10) DCX0 = DCX0 + 1.0
         IF (NBOXES.GE.100) DCX0 = DCX0 + 1.0
         IF (NBOXES.GE.1000) DCX0 = DCX0 + 1.0
         END IF
      IF (NBOXES.LE.10) THEN
         M = 1
      ELSE IF (NBOXES.LE.20) THEN
         M = 2
      ELSE IF (NBOXES.LE.50) THEN
         M = 5
      ELSE IF (NBOXES.LE.100) THEN
         M = 10
      ELSE IF (NBOXES.LE.200) THEN
         M = 20
      ELSE IF (NBOXES.LE.500) THEN
         M = 50
      ELSE IF (NBOXES.LE.1000) THEN
         M = 100
      ELSE IF (NBOXES.LE.2000) THEN
         M = 200
         END IF
      TICLEN = (TRC(1)-BLC(1))/TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
      DO 150 I = 0,NBOXES,M
         Y = I - .5
         CALL GPOS (YTICER, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (TRC(1), Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1115) I
            CALL CHTRIM (MSGBUF, 4, MSGBUF, INCHAR)
            DCX = DCX0 - REAL(INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 150     CONTINUE
C                                       Label RHS bucket #
      Y = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (TRC(1), Y, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = DCX0 + 1.0
      DCY = 4
      MSGBUF = 'Box number'
      CALL GCHAR (10, 1, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Range =
      CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = -2.833
         IF (LTYPE.GT.2) DCY = DCY - 1.333
         WRITE (MSGBUF,1151) RANGE(1), RANGE(2), CTEMP
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Interval =
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         X = (RANGE(2) - RANGE(1)) / NBOXES
         WRITE (MSGBUF,1152) X, CTEMP
         DCY = DCY - 1.333
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
C                                       Underflow = overflow =
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         I = UNDER + 0.1
         M = OVER + 0.1
         WRITE (MSGBUF,1154) I, M
         DCY = DCY - 1.333
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         END IF
C                                       Determine label range
      DIST = RANGE(2) - RANGE(1)
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX, PFLAG)
      IF (PFLAG) GO TO 190
      XDIST = DIST / ODIST
      ODIST = XDIST * RANGE(1)
C                                       Get interval
      DO 160 ITRY = 1,21
         XNOINT = AINT (DIST/XINTER(ITRY))
         IF (XNOINT.LE.9.0) GO TO 170
160     CONTINUE
      GO TO 190
C                                       Left hand (value) tics
 170  XINT = XINTER(ITRY)
      DCY = -0.5
      DCX0 = -1.0
      DCXM = -0.5
      XMAX = MAX (ABS(RANGE(2)), ABS(RANGE(1))) * XDIST
      INOINT = XNOINT + 2.5
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GT.ODIST) XVAL = XVAL - XINT
      DO 175 I = 1,INOINT
         XVAL = XVAL + XINT
         Y = ((XVAL-ODIST)/DIST) * NBOXES
         IF (Y.GT.TRC(2)) GO TO 180
         CALL GPOS (YTICEL, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (BLC(1), Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (ITRY.GT.3) INCHAR = INCHAR - 2
            DCX = DCX0 - INCHAR
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 175     CONTINUE
C                                       Label with prefix
 180  DCX = DCXM - 2.0
      Y = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), Y, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1175) PREFIX, CTEMP
      CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
      DCY = INCHAR/2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (LTYPE.GE.7) GO TO 999
C                                       which axis is which?
 190  NXRA = 0
      NXDEC = 0
      NXLL = 0
      NXMM = 0
      NXFR = 0
      NXST = 0
      NAX = CATBLK(KIDIM)
      INC = 2
      DO 200 I = 1,NAX
         ICPNT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
         IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
         IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 200     CONTINUE
C                                       Source name, stokes, freq.
      CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = 0.0
      DCY = 0.5
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK),
     *   FREQ
      IF (NXFR.LE.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '  _'
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.1)
 1115 FORMAT (I4)
 1151 FORMAT ('Range =',1PE12.4,' to',1PE12.4,1X,A8)
 1152 FORMAT ('Interval =',1PE12.4,1X,A8)
 1154 FORMAT ('Underflow =',I10,' _Overflow =',I10)
 1175 FORMAT (A5,1X,A8)
 1200 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE HISTOX (BMAX, PHIS, NBOXES, RANGE, DOLOG, LABEL, IVER,
     *   PARMS, IPBLK, IERR)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a histogram.  Flux on X axis, count on Y axis.
C   Inputs:
C      BMAX     R        the maximum value in any of the boxes.
C      PHIS     D(NBOXES+2)   The number of entries in each range.
C                        PHIS(1) is the underflow, PHIS(NBOXES+2) is
C                        the overflow.
C      NBOXES   I        number of boxes or value ranges for histogram.
C      RANGE    R(2)     intensity range of histogram
C      DOLOG    L        T => use log(n) rather than linear scale
C      LABEL    I        Type of labeling
C      IVER     I        Plot file version number
C      PARMS    D(3)     Fit parameters (4) = 0 => use them
C   In/out:
C      IPBLK    I(256)   I/O buffer for open, initialized pl file.
C   Output:
C      IERR     I        error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      REAL      BMAX, RANGE(2)
      DOUBLE PRECISION PARMS(3), PHIS(*)
      INTEGER   NBOXES, LABEL, IVER, IPBLK(256), IERR
      LOGICAL   DOLOG
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, XYRATO, LOCRAN(2),
     *   EXF, BFACT
      INTEGER   IDEPTH(5), I, LTYPE
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set character offsets.
      LOCRAN(2) = BMAX
      LOCRAN(1) = 0.
      IF (DOLOG) LOCRAN(1) = ALOG10 (0.5)
      CALL GTICNT (LABEL, LOCRAN, I)
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = CH(2) + 3 * 1.333
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF (LTYPE.GT.2) CH(4) = CH(4) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(2) = 0.0
      IF (DOLOG) BLC(2) = ALOG10 (0.5)
      BLC(1) = 0.0
      TRC(1) = NBOXES
      TRC(2) = BMAX * 1.05
      XYRATO = (TRC(1) - BLC(1)) / (TRC(2) - BLC(2))
C                                       Kludge to keep XYRATO small
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      DO 50 I = 1,10000
         IF (XYRATO.GT.0.50) GO TO 60
         FAC = FAC * 10.0
         XYRATO = XYRATO * 10.0
 50      CONTINUE
C
 60   TRC(2) = TRC(2) / FAC
      BLC(2) = BLC(2) / FAC
      XYRATO = 1.0 / XYRATO
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (1, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Labeling.
      I = NBOXES + 2
      CALL HLABX (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER, PHIS(1),
     *   PHIS(I), LABEL, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      TXTMSG = 'End labeling, draw histogram'
      CALL GCOMNT (-1, TXTMSG, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (2, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Position at first data point.
      X = BLC(1)
      Y = BLC(2)
      CALL GPOS (X, Y, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop for rest of data points.
      DO 100 I = 1,NBOXES
         Y = PHIS(I+1) / FAC
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         X = I
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         Y = BLC(2)
         CALL GVEC (X, Y, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Gaussian fit
      IF (PARMS(1).GT.0.0) THEN
         TXTMSG = 'Draw gaussian fit'
         CALL GCOMNT (2, TXTMSG, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GLTYPE (4, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         EXF = NBOXES / 1000.0
         BFACT = BMAX
         IF (DOLOG) BFACT = 10.0 ** BMAX
         DO 200 I = 1,1001
            X = RANGE(1) + (I-1.0) * (RANGE(2)-RANGE(1)) / 1000.0
            X = (X - PARMS(2)) / PARMS(3)
            X = X * X / 2.0
            IF (X.LE.69.) THEN
               Y = BFACT * EXP (-X)
            ELSE
               Y = 0.
               END IF
            IF (DOLOG) Y = ALOG10 (MAX (0.5, Y))
            X = (I - 1) * EXF
            Y = Y / FAC
            IF (I.EQ.1) THEN
               CALL GPOS (X, Y, IPBLK, IERR)
            ELSE
               CALL GVEC (X, Y, IPBLK, IERR)
               END IF
            IF (IERR.NE.0) GO TO 999
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE HLABX (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER,
     *   UNDER, OVER, LABEL, IPBLK, IERR)
C-----------------------------------------------------------------------
C   Write labeling for histogram.
C   Inputs:
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C      IPBLK   I(256)   I/O buffer for plot file.
C   Output:
C      IERR    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC, RANGE(2)
      DOUBLE PRECISION UNDER, OVER
      INTEGER   NBOXES, IVER, LABEL, IPBLK(256), IERR
      LOGICAL   DOLOG
C
      CHARACTER PREFIX*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      REAL      XINTER(21), DCX, DCY, XNOINT, DIST, ODIST, XDIST, XMAX,
     *   TICSCL, YTICEL, YTICER, XVAL, YPOS, TICLEN, XINT, X, FREQ,
     *   DCXM
      INTEGER   INOINT, INCHAR, I, IXO, M, ITRY, NXRA, NXDEC, NXLL,
     *   NXMM, NXFR, NXST, NAX, INC, IANGL, JSTOK, IT(3), ID(3),
     *   ICPNT, ITMP, LTYPE
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TICSCL /70.0/
      DATA CSTOK /'????','Beam','Ipol','Qpol','Upol','Vpol','Ppol',
     *   'Fpol','Pang','Spix','Optd','    '/
      DATA XINTER /.1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200.,
     *   500., 1000., 2000., 5000., 10000., 20000., 50000., 100000.,
     *   200000., 500000./
C-----------------------------------------------------------------------
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.LE.1) GO TO 999
C                                       Tic positions.
      TICLEN = (TRC(1) - BLC(1)) / TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(2) - BLC(2))
      XINT = 9.0
      DO 20 I = 1,21
         XNOINT = AINT (DIST/XINTER(I))
         IF (XNOINT.LE.XINT) GO TO 30
 20      CONTINUE
      GO TO 110
C                                       Interval and no of inter found.
 30   XINT = XINTER(I)
      INOINT = XNOINT + 2.5
      XVAL = AINT (FAC*BLC(2)/XINT) * XINT
      IF (XVAL.GT.FAC*BLC(2)) XVAL = XVAL - XINT
      IXO = I
      DCXM = -0.5
C                                       Loop for all tics.
      DO 100 I = 1,INOINT
         XVAL = XVAL + XINT
         YPOS = XVAL / FAC
         IF (YPOS.GT.TRC(2)) GO TO 110
C                                       TOP tic.
         CALL GPOS (TRC(1), YPOS, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (YTICER, YPOS, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Left hand tic.
         CALL GPOS (YTICEL, YPOS, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (BLC(1), YPOS, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Write value.
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.3) INCHAR = INCHAR - 2
            DCX = - INCHAR - 1.0
            DCY = -0.5
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Number of pixels
 110  DCX = DCXM - 2.0
      YPOS = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), YPOS, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DOLOG) THEN
         MSGBUF = 'Log10 (number of pixels)'
         INCHAR = 24
      ELSE
         MSGBUF = 'Number of pixels'
         INCHAR = 16
         END IF
      DCY = INCHAR / 2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Write bucket numbers on top
      IF (NBOXES.LE.8) THEN
         M = 1
      ELSE IF (NBOXES.LE.16) THEN
         M = 2
      ELSE IF (NBOXES.LE.40) THEN
         M = 5
      ELSE IF (NBOXES.LE.80) THEN
         M = 10
      ELSE IF (NBOXES.LE.160) THEN
         M = 20
      ELSE IF (NBOXES.LE.400) THEN
         M = 50
      ELSE IF (NBOXES.LE.800) THEN
         M = 100
      ELSE IF (NBOXES.LE.1600) THEN
         M = 200
      END IF
      TICLEN = (TRC(2) - BLC(2)) / TICSCL
      YTICEL = BLC(2) + TICLEN
      YTICER = TRC(2) - TICLEN
      DCY = 0.5
      DO 150 I = 0,NBOXES,M
         X = I - .5
         CALL GPOS (X, YTICER, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (X, TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1115) I
            CALL CHTRIM (MSGBUF, 4, MSGBUF, INCHAR)
            DCX = 0.5 - REAL(INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 150     CONTINUE
C                                       Label RHS bucket #
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = -5.0
      DCY = 0.5
      IF (LTYPE.GT.2) DCY = DCY + 1.333
      MSGBUF = 'Box number'
      CALL GCHAR (10, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Range =
      CALL H2CHR (8, 1, CATH(KHBUN), CTEMP)
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = -2.833
         IF (LTYPE.GT.2) DCY = DCY - 1.333
         WRITE (MSGBUF,1151) RANGE(1), RANGE(2), CTEMP
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Interval =
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         X = (RANGE(2) - RANGE(1)) / NBOXES
         WRITE (MSGBUF,1152) X, CTEMP
         DCY = DCY - 1.333
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
C                                       Underflow = overflow =
         CALL GPOS (BLC(1), BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         I = UNDER + 0.1
         M = OVER + 0.1
         WRITE (MSGBUF,1154) I, M
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY - 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         END IF
C                                       Determine label range
      DIST = RANGE(2) - RANGE(1)
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX, PFLAG)
      IF (PFLAG) GO TO 190
      XDIST = DIST / ODIST
      ODIST = XDIST * RANGE(1)
C                                       Get interval
      DO 160 ITRY = 1,21
         XNOINT = AINT (DIST/XINTER(ITRY))
         IF (XNOINT.LE.9.0) GO TO 170
 160     CONTINUE
      GO TO 190
C                                       Bottom (value) tics
 170  XINT = XINTER(ITRY)
      DCY = -1.5
      XMAX = MAX (ABS(RANGE(2)), ABS(RANGE(1))) * XDIST
      INOINT = XNOINT + 2.5
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GT.ODIST) XVAL = XVAL - XINT
      DO 175 I = 1,INOINT
         XVAL = XVAL + XINT
         X = ((XVAL-ODIST)/DIST) * NBOXES
         IF (X.GT.TRC(1)) GO TO 180
         CALL GPOS (X, YTICEL, IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (X, BLC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (ITRY.GT.3) INCHAR = INCHAR - 2
            DCX = 0.5 - INCHAR
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 175     CONTINUE
C                                       Label with prefix
 180  DCY = -1.5
      IF (LTYPE.GT.2) DCY = -2.833
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, BLC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1175) PREFIX, CTEMP
      CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
      DCX = 0.5 - INCHAR / 2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (LTYPE.GE.7) GO TO 999
C                                       which axis is which?
 190  NXRA = 0
      NXDEC = 0
      NXLL = 0
      NXMM = 0
      NXFR = 0
      NXST = 0
      NAX = CATBLK(KIDIM)
      INC = 2
      DO 200 I = 1,NAX
         ICPNT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
         IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
         IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 200     CONTINUE
C                                       Source name, stokes, freq.
      CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = 0.0
      DCY = 1.833
      IF (LTYPE.GT.2) DCY = DCY + 1.333
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK),
     *   FREQ
      IF (NXFR.LE.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '  _'
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), IPBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, IPBLK, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.1)
 1115 FORMAT (I4)
 1151 FORMAT ('Range =',1PE12.4,' to',1PE12.4,1X,A8)
 1152 FORMAT ('Interval =',1PE12.4,1X,A8)
 1154 FORMAT ('Underflow =',I10,' _Overflow =',I10)
 1175 FORMAT (A5,1X,A8)
 1200 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE PWRITE (MSGLEV, TXLUN, TXFIND, TEXT)
C-----------------------------------------------------------------------
C! writes messages to log file and/or terminal and/or output file
C# Utility
C-----------------------------------------------------------------------
C   Inputs:
C      MSGLEV   I       Priority of Message (< 0 => NO MSGWRT)
C      TXFIND   I       Index in FTAB for LUN
C      TEXT     C*(*)   Message
C   In/out:
C      TXLUN    I       Logical unit number (0 IN => no file)
C                          0 out => error
C-----------------------------------------------------------------------
      INTEGER   MSGLEV, TXLUN, TXFIND
      CHARACTER TEXT*(*)
C
      INTEGER   IERR, TXLEN, JTRIM
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
      IF (MSGLEV.GE.0) THEN
         MSGTXT = TEXT
         CALL MSGWRT (MSGLEV)
         END IF
C                                       If output file opened
C                                       Log Informative Messages
      IF (TXLUN.GT.0) THEN
         TXLEN = JTRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXFIND, TEXT(1:TXLEN), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) TXLUN, IERR
            CALL MSGWRT (7)
            CALL ZTXCLS (TXLUN, TXFIND, IERR)
            TXLUN = 0
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTFILE WRITE ERROR ON LUN=',I5,', ERR=',I5)
      END
      SUBROUTINE XGALMS (M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   XGALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs: M      I       Number data points (adj. array dim.)
C           N      I       Number of unknowns (adj. array dim.)
C           LDFJAC I       Number points on first axis of FJAC (adj.
C                          array dim.)
C           TOL    D       Tolerance desired
C   In/out: X      D(N)    Initial guess/ answer
C           FVEC   D(M)    Function (Data - model) evaluation
C           FJAC   D(N,N)
C           INFO   I       Error code: 1 - 3 good, 0 bad input,
C                             4 orthogonal, 5 - 7 poor fit
C           IPVT   D(N)
C   See precursor remarks to LMSTR1 or LMSTR for details.
C-----------------------------------------------------------------------
      EXTERNAL  XGFUNC
      INTEGER   M, N, LDFJAC, LWA
      INTEGER   INFO, IPVT(N)
      DOUBLE PRECISION    TOL, X(M), FVEC(M), FJAC(N,N), WA(920)
      DATA LWA /920/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      CALL LMSTR1 (XGFUNC, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END
      SUBROUTINE XGFUNC (M, N, PARMS, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C     INPUTS:  M        I   Number of data points in slice (adj. array
C                           dim.).
C              N        I   No. of parameters (adj. array dim.;
C                           NGAUSS * 3)
C              PARMS    D(N)   parameters of gaussian components,
C                       GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C              IFLAG    I   1=calculate difference for current guess.
C                           2=calculate jacobian for current guess.
C    COMMON:   GDATA
C              DATA     R(?)   Origional slice data points.
C              IDOPOS   I(4)   -1 means hold corresponding position
C                       parameter constant.
C              IDOMAX   I(4)   -1 means hold corresponding maximum
C                       amplitude parameter constant.
C              IDOWTH   I(4)   -1 means hold corresponding half
C                       width parameter constant.
C              ITTER    I   number of calls to evaluate FVEC.
C    OUTPUTS:  FVEC     D(M)   Slice data points minus data points
C                       evaluated for current guess.
C              FJROW    D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M
      DOUBLE PRECISION    PARMS(N), FVEC(M), FJROW(N), AMP,
     *   POS, SIG, EFACT, RES2, TSIG2, X, HALFAC
      INTEGER   IFLAG, IDATA
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'GDATA.INC'
      DATA HALFAC /2.77258872D0/
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITTER) THEN
            IFLAG = -1
         ELSE
            DO 20 IDATA = 1,M
               FVEC(IDATA) = DATA(IDATA)
               IF (FVEC(IDATA).EQ.FBLANK) THEN
                  FVEC(IDATA) = 0.0D0
               ELSE
                  X = IDATA
                  AMP = PARMS(1)
                  IF (AMP.NE.0.0D0) THEN
                     POS = PARMS(2)
                     SIG = PARMS(3)
                     RES2 = (X - POS) / SIG
                     RES2 = HALFAC * RES2 * RES2
                     IF (RES2.LE.69.0D0) FVEC(IDATA) = FVEC(IDATA) -
     *                  AMP * EXP (-RES2)
                     END IF
                  END IF
   20          CONTINUE
            END IF
C                                       Calculate Jacobian.
      ELSE
         IDATA = IFLAG - 1
         X = IDATA
         FJROW(1) = 0.0D0
         FJROW(2) = 0.0D0
         FJROW(3) = 0.0D0
         AMP = PARMS(1)
         POS = PARMS(2)
         SIG = PARMS(3)
         RES2 = HALFAC * (X - POS) * (X - POS)
         TSIG2 = RES2 / (SIG * SIG)
         IF (TSIG2.LE.69.0D0) THEN
            EFACT = -EXP (-TSIG2)
            FJROW(1) = EFACT
            EFACT = 2.0D0 * EFACT * AMP / (SIG * SIG)
            FJROW(2) = HALFAC * EFACT * (X-POS)
            FJROW(3) = EFACT * RES2 / SIG
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE ROBGET (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   Robust mean and rms of a reasonable portion of the image
C   Inputs:
C      LUN    I   LUN of open image
C      FIND   I   FTAB pointer of open file
C   Outputs:
C      IERR   I   Error code
C      PIXAVG, PIXRMS returned in common
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   NWORDS, I, J
      LOGICAL   DOINV
      REAL      IMAGE(2), LBLC(7), LTRC(7), PIXVS(2)
      LONGINT   PIMAGE
      DOUBLE PRECISION XVAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IMEAN.INC'
      EQUIVALENCE (PIXVS, PIXAVG)
C-----------------------------------------------------------------------
C                                       local window
      DOINV = DINVER.GT.0.0
      IF (DOINV) THEN
         CALL RFILL (7, 0.0, LBLC)
         CALL RFILL (7, 0.0, LTRC)
      ELSE
         CALL RCOPY (7, BLC, LBLC)
         CALL RCOPY (7, TRC, LTRC)
         END IF
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), LBLC, LTRC, IERR)
      J = 8
 10   XVAL = 1.0D-3
      DO 20 I = 1,7
         XVAL = XVAL * (LTRC(I) - LBLC(I) + 1.0D0)
 20      CONTINUE
      IF ((XVAL.GT.KAPWRD) .AND. (J.GT.3)) THEN
         J = J - 1
         LTRC(J) = 1.0
         LBLC(J) = 1.0
         GO TO 10
         END IF
      IF (J.LT.8) THEN
         MSGTXT = 'ROBGET: guess of true mean and rms from sub-image'
     *      // ' only'
         CALL MSGWRT (6)
         END IF
      NWORDS = XVAL * 1.D3 + 2048.1
      NWORDS = (NWORDS - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'ROBGET', NWORDS, IMAGE, PIMAGE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ROBGET: COULD NOT GET DYNAMIC MEMORY'
         CALL MSGWRT (7)
         GO TO 999
         END IF
      CALL DOROB (LUN, FIND, LBLC, LTRC, DOINV, BLC, TRC,
     *   IMAGE(1+PIMAGE), PIXVS, IERR)
      CALL ZMEMRY ('FREE', 'ROBGET', NWORDS, IMAGE, PIMAGE, I)
C
 999  RETURN
      END
      SUBROUTINE DOROB (LUN, FIND, LBLC, LTRC, DOINV, BLC, TRC, IMAGE,
     *   PIXV, IERR)
C-----------------------------------------------------------------------
C   DOROB actually does the robust rms and mean
C   Inputs:
C      LUN    I      LUN of open image
C      FIND   I      FTAB pointer of open file
C      LBLC   R(7)   bottom left corner
C      LTRC   R(7)   top right corner
C      DOINV  L      do outside only
C      BLC    R(7)   inner region blc
C      TRC    R(7)   inner region trc
C   Outputs:
C      IMAGE  R(*)   work space
C      PIXV   R(2)   mean, rms
C      IERR   I      error code
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
      REAL      LBLC(7), LTRC(7), IMAGE(*), BLC(7), TRC(7), PIXV(2)
      LOGICAL   DOINV
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NITER
      PARAMETER (NITER=8)
C
      INTEGER   I2, I3, I4, I5, I6, I7, L1, L2, L3, L4, L5, L6, L7, IT,
     *   U1, U2, U3, U4, U5, U6, U7, NP, IP, NX, NY, NBUF, BLKOF, BIND,
     *   WIN(4), DEPTH(5), NBL, IBL
      REAL      BUFF(MABFSS), WS(NITER), VP, VM
      DOUBLE PRECISION SV, SSV, NV
      LOGICAL   DOIT, YDOIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
      L1 = LBLC(1) + 0.1
      L2 = LBLC(2) + 0.1
      L3 = LBLC(3) + 0.1
      L4 = LBLC(4) + 0.1
      L5 = LBLC(5) + 0.1
      L6 = LBLC(6) + 0.1
      L7 = LBLC(7) + 0.1
      U1 = LTRC(1) + 0.1
      U2 = LTRC(2) + 0.1
      U3 = LTRC(3) + 0.1
      U4 = LTRC(4) + 0.1
      U5 = LTRC(5) + 0.1
      U6 = LTRC(6) + 0.1
      U7 = LTRC(7) + 0.1
      NX = U1 - L1 + 1
      NY = U2 - L2 + 1
      WIN(1) = L1
      WIN(2) = L2
      WIN(3) = U1
      WIN(4) = U2
      NBUF = MABFSS * 2
      IP = 1
      NBL = TRC(1) - BLC(1)  + 1.01
      IBL = BLC(1) - 0.1
C                                       read in data
      DO 90 I7 = L7,U7
         DO 80 I6 = L6,U6
            DO 70 I5 = L5,U5
               DO 60 I4 = L4,U4
                  DO 50 I3 = L3,U3
                     DOIT = ((I7.GE.BLC(7)) .AND. (I7.LE.TRC(7))
     *                  .AND. (I6.GE.BLC(6)) .AND. (I6.LE.TRC(6))
     *                  .AND. (I5.GE.BLC(5)) .AND. (I5.LE.TRC(5))
     *                  .AND. (I4.GE.BLC(4)) .AND. (I4.LE.TRC(4))
     *                  .AND. (I3.GE.BLC(3)) .AND. (I3.LE.TRC(3)))
                     DEPTH(1) = I3
                     DEPTH(2) = I4
                     DEPTH(3) = I5
                     DEPTH(4) = I6
                     DEPTH(5) = I7
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH,
     *                  BLKOF, IERR)
                     BLKOF = BLKOF + 1
C                                       Initialize for read:
                     CALL MINIT ('READ', LUN, FIND, CATBLK(KINAX),
     *                  CATBLK(KINAX+1), WIN, BUFF, NBUF, BLKOF, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1000) IERR, 'INIT IMAGE I/O'
                        GO TO 990
                        END IF
                     DO 40 I2 = L2,U2
                        YDOIT = DOIT .AND. (I2.GE.BLC(2)) .AND.
     *                     (I2.LE.TRC(2)) .AND. (DOINV)
C                                       Read next row:
                        CALL MDISK ('READ', LUN, FIND, BUFF, BIND, IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1000) IERR, 'READ IMAGE ROW'
                           GO TO 990
                           END IF
                        CALL RCOPY (NX, BUFF(BIND), IMAGE(IP))
                        IF (YDOIT) CALL RFILL (NBL, FBLANK,
     *                     IMAGE(IP+IBL))
                        IP = IP + NX
 40                     CONTINUE
 50                  CONTINUE
 60               CONTINUE
 70            CONTINUE
 80         CONTINUE
 90      CONTINUE
      NP = IP - 1
C                                       ROBUST mean, rms
      VP = 1.E5
      VM = -VP
      DO 120 IT = 1,NITER
         SV = 0.0D0
         SSV = 0.0D0
         NV = 0.0D0
         DO 110 IP = 1,NP
            IF ((IMAGE(IP).NE.FBLANK) .AND. (IMAGE(IP).NE.0.0)) THEN
               IF ((IMAGE(IP).GT.VM) .AND. (IMAGE(IP).LT.VP)) THEN
                  SV = SV + IMAGE(IP)
                  SSV = SSV + IMAGE(IP) * IMAGE(IP)
                  NV = NV + 1.0D0
                  END IF
               END IF
 110        CONTINUE
         IF (NV.GT.0.0D0) THEN
            SV = SV / NV
            SSV = SSV / NV - SV * SV
            SSV = SQRT (MAX (0.0D0, SSV))
            IF (IT.LT.NITER) THEN
               VP = SV + WS(IT+1) * SSV
               VM = SV - WS(IT+1) * SSV
               END IF
         ELSE
            VP = 1.E4
            VM = -1.E4
            END IF
 120     CONTINUE
C                                       return answers
      PIXV(1) = SV
      PIXV(2) = SSV
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DOROB: ERROR',I5,' ON ',A)
      END
