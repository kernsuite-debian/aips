LOCAL INCLUDE 'BPLOT.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      USER, XSEQ, XDISK, XVER, XBCHAN, XECHAN, XBIF, XEIF,
     *   XFQ, XSUB, XTIME(8), XANT(50), XRANGE(2), FACTOR, XDO3C,
     *   XLTYPE, XDOTV, XCHAN, XYRATO
      HOLLERITH XINNAM(3), XINCLS(2), XTTYPE, XSTOKE, XTYPE, XSORT
      CHARACTER INNAM*12, INCLS*6, PTYPE*4, PSORT*2, TTYPE*2, DOSTOK*4
      COMMON /INPARM/ USER, XINNAM, XINCLS, XSEQ, XDISK, XTTYPE, XVER,
     *   XFQ, XSUB, XTIME, XBCHAN, XECHAN, XBIF, XEIF, XSTOKE, XANT,
     *   XTYPE, XSORT, XRANGE, FACTOR, XDO3C, XLTYPE, XDOTV, XCHAN,
     *   XYRATO
      COMMON /CHPARM/ INNAM, INCLS, PTYPE, PSORT, TTYPE, DOSTOK
      INTEGER   INSEQ, INDISK, INVERS, CNO, IUSER, BIF, EIF, BPOL, EPOL,
     *   BCHAN, ECHAN, PBUFF(256), FQID, SUBARR, NPARMS, PLUN, PFIND,
     *   LABEL, IANTS(MAXANT), DO3COL, NIANTS, LTYPE
      DOUBLE PRECISION TIME, TIMBEG, TIMEND, TPLBEG, TPLEND
      REAL      ARANGE(2)
      COMMON /BPLOTC/ TIME, TIMBEG, TIMEND, TPLBEG, TPLEND, PBUFF,
     *   INSEQ, INDISK, INVERS, CNO, IUSER, BIF, EIF, BPOL, EPOL, BCHAN,
     *   ECHAN, FQID, SUBARR, NPARMS, PLUN, PFIND, LABEL, IANTS, ARANGE,
     *   DO3COL, NIANTS, LTYPE
      INCLUDE 'INCS:DBPC.INC'
LOCAL END
      PROGRAM BPLOT
C-----------------------------------------------------------------------
C! Plots bandpass table profiles as function of time or antenna
C# EXT-appl Calibration Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1998, 2000-2004, 2006, 2008-2011, 2014-2015,
C;  Copyright (C) 2018
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
C   Smooths/interpolates bandpass table to regular intervals
C   Inputs from user
C      USERID.....Input file user number.  ignored
C                 32000 => any user.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      INVERS.....Specifies the version of the BP table to be read as
C                 input.   0 -> highest.
C                 The output version is always highest + 1.
C      BCHAN......Lowest channel of a range of channels to be plotted.
C                 0 => 1.
C      ECHAN......Highest channel number to be plotted.  0 => highest.
C      BIF........Lowest IF to be plotted.  0 => 1
C      EIF........Highest IF to be plotted.  0 => highest.
C      STOKES.....If the first character is R or L then only that
C                 polarization is plotted.  Otherwise both are plotted
C                 if present.
C      CODETYPE...'AMP', 'PHAS' to plot amplitude or phase of the
C                    bandpass.
C                 'DIFA', 'DIFP' to plot the amplitude or phase of the
C                    difference between the scalar average of the table
C                    entries in the plot and the present entry.
C      SORT.......Sort order for table.  If SORT = 'TA', then each plot
C                 will show all antennas at a particular time.
C                 Otherwise, each plot will show all times for a
C                 particular antenna.
C      PIXRANGE...Clip plotted values with PIXRANGE.  0 => self-scale
C                 with the max min for all solutions in the plot.
C      FACTOR.....Controls the plot scale for each profile.
C      LTYPE......Labelling type: 1 = border, 2 = no ticks, 3 or 7 =
C                 standard, 4 or 8 = relative to ref. pixel, 5 or 9 =
C                 relative to subimage (BLC, TRC) center, 6 or 10 =
C                 pixels. 7-10 all labels other than tick numbers and
C                 axis type are omitted.  Less than 0 is the same except
C                 that the plot file version number and create time are
C                 omitted.
C      DOTV.......> 0 => plot directly on the TV device, otherwise make
C                 plot files for later display on one or more devices
C                 (including the TV if desired).
C      GRCHAN.....Graphics channel (1 - 7) to use for line drawing.
C                 0 => 1.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'BPLOT.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL BPLOTI (IRET)
C                                       do plotting
      IF (IRET.EQ.0) then
         IF (TTYPE.EQ.'BP') THEN
            IF (DOSTOK.EQ.' ') THEN
               CALL BPLOTT (IRET)
            ELSE
               CALL BPLOTS (IRET)
               END IF
         ELSE IF (TTYPE.EQ.'BD') THEN
            CALL BDLOTT (IRET)
         ELSE IF (TTYPE.EQ.'PD') THEN
            CALL PDLOTT (IRET)
         ELSE IF (TTYPE.EQ.'CP') THEN
            CALL CPLOTT (IRET)
            END IF
         END IF
C                                       close down
      CALL DIE (IRET, PBUFF)
C
 999  STOP
      END
      SUBROUTINE BPLOTI (IERR)
C-----------------------------------------------------------------------
C   BPLOTI performs initialization for AIPS task BPLOT.  It gets the
C   adverbs, opens the catalog file for 'READ' (eventually), sorts and
C   opens the BP input file, and determines which FQs, times, and
C   antenna counts apply to the table.
C   Output:
C      IERR    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER INTYP*2, STAT*4, PRGN*6, KEYSBP(2)*24, LTYPBP*8,
     *   KEYSBD(2)*24, KEYSPD(2)*24, KEYSCP(2)*24, POLTYP*8
      INTEGER   IROUND, BPLUN, JERR, J, KEY(2,2), NKEY, KOLS(2), J1, J2,
     *   KEYSUB(2,2)
      REAL      FKEY(2,2), TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BPLUN, INTYP /27, 'UV'/
      DATA PRGN /'BPLOT '/
      DATA NKEY /2/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYSBP /'ANTENNA ', 'TIME '/
      DATA KEYSBD /'ANTENNA1', 'ANTENNA2'/
      DATA KEYSPD /'SUBARRAY', 'ANTENNA '/
      DATA KEYSCP /'SOURCE', 'SOURCE'/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARMS = 85
      IERR = 0
      CALL GTPARM (PRGN, NPARMS, RQUICK, USER, BPBUFF, JERR)
      IF (JERR.NE.0) THEN
         RQUICK = .TRUE.
         IERR = 8
         IF (JERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) JERR, 'GET INPUT ADVERBS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IERR, BPBUFF, JERR)
      IF (IERR.NE.0) GO TO 999
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      CALL H2CHR (2, 1, XTTYPE, TTYPE)
      IF ((TTYPE.NE.'BD') .AND. (TTYPE.NE.'PD') .AND.
     *   (TTYPE.NE.'CP')) TTYPE = 'BP'
      CALL H2CHR (4, 1, XSORT, PSORT)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      IUSER = NLUSER
      USER = NLUSER
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, BPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file write
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'WRIT', BPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ CATALOG HEADER'
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 1
      DO3COL = XDO3C + 0.5
      IF (XDO3C.GT.0.0) DO3COL = MAX (1, DO3COL)
C                                       Open BP file
      IF (TTYPE.EQ.'BP') THEN
         CALL BPREFM (INDISK, CNO, INVERS, CATBLK, BPLUN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL BPINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK, BPLUN,
     *      IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP,
     *      BCHNBP, NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN INPUT BP TABLE'
            GO TO 990
            END IF
         WPOLY = LTYPBP.NE.' '
C                                       Set column pointers for sort
         CALL FNDCOL (NKEY, KEYSBP, 24, .TRUE., BPBUFF, KOLS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'FIND BP COLUMNS'
            GO TO 990
            END IF
C                                       Sort to ant-time order
         IF (PSORT.EQ.'TA') THEN
            J1 = 2
            J2 = 1
         ELSE
            J1 = 1
            J2 = 2
            END IF
         IF ((BPBUFF(43).NE.KOLS(J1)) .OR. (BPBUFF(44).NE.KOLS(J2)))
     *         THEN
C                                       Close
            CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE BP TABLE'
               GO TO 990
               END IF
C                                       sort
            KEY(1,1) = KOLS(J1)
            KEY(2,1) = KOLS(J1)
            KEY(1,2) = KOLS(J2)
            KEY(2,2) = KOLS(J2)
            CALL TABSRT (INDISK, CNO, 'BP', INVERS, INVERS, KEY, KEYSUB,
     *         FKEY, BPBUFF, CATBLK, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'SORT BP TABLE'
               GO TO 990
               END IF
C                                       Re-open BP table for read
            CALL BPINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK,
     *         BPLUN, IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP,
     *         NCHNBP, BCHNBP, NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN SORTED BP TABLE'
               GO TO 990
               END IF
            END IF
C                                       BD table
      ELSE IF (TTYPE.EQ.'BD') THEN
         CALL BDINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK, BPLUN,
     *      IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN INPUT BD TABLE'
            GO TO 990
            END IF
C                                       Set column pointers for sort
         CALL FNDCOL (NKEY, KEYSBD, 24, .TRUE., BPBUFF, KOLS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'FIND BD COLUMNS'
            GO TO 990
            END IF
C                                       Sort to ant-time order
         J1 = 1
         J2 = 2
         IF ((BPBUFF(43).NE.KOLS(J1)) .OR. (BPBUFF(44).NE.KOLS(J2)))
     *         THEN
C                                       Close
            CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE BD TABLE'
               GO TO 990
               END IF
C                                       sort
            KEY(1,1) = KOLS(J1)
            KEY(2,1) = KOLS(J1)
            KEY(1,2) = KOLS(J2)
            KEY(2,2) = KOLS(J2)
            CALL TABSRT (INDISK, CNO, 'BD', INVERS, INVERS, KEY, KEYSUB,
     *         FKEY, BPBUFF, CATBLK, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'SORT BD TABLE'
               GO TO 990
               END IF
C                                       Re-open BD table for read
            CALL BDINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK,
     *         BPLUN, IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP,
     *         NCHNBP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN SORTED BD TABLE'
               GO TO 990
               END IF
            END IF
C                                       PD table
      ELSE IF (TTYPE.EQ.'PD') THEN
         CALL PDINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK, BPLUN,
     *      IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP,
     *      POLTYP, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN INPUT PD TABLE'
            GO TO 990
            END IF
C                                       Set column pointers for sort
         CALL FNDCOL (NKEY, KEYSPD, 24, .TRUE., BPBUFF, KOLS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'FIND PD COLUMNS'
            GO TO 990
            END IF
C                                       Sort to ant-time order
         J1 = 1
         J2 = 2
         IF ((BPBUFF(43).NE.KOLS(J1)) .OR. (BPBUFF(44).NE.KOLS(J2)))
     *         THEN
C                                       Close
            CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE PD TABLE'
               GO TO 990
               END IF
C                                       sort
            KEY(1,1) = KOLS(J1)
            KEY(2,1) = KOLS(J1)
            KEY(1,2) = KOLS(J2)
            KEY(2,2) = KOLS(J2)
            CALL TABSRT (INDISK, CNO, 'PD', INVERS, INVERS, KEY, KEYSUB,
     *         FKEY, BPBUFF, CATBLK, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'SORT PD TABLE'
               GO TO 990
               END IF
            CALL PDINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK,
     *         BPLUN, IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP,
     *         NCHNBP, POLTYP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN SORTED PD TABLE'
               GO TO 990
               END IF
            END IF
C                                       Re-open BD table for read
C                                       CP table
      ELSE IF (TTYPE.EQ.'CP') THEN
         CALL CPINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK, BPLUN,
     *      IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP, J, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN INPUT CP TABLE'
            GO TO 990
            END IF
C                                       Set column pointers for sort
         CALL FNDCOL (NKEY, KEYSCP, 24, .TRUE., BPBUFF, KOLS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'FIND CP COLUMNS'
            GO TO 990
            END IF
C                                       Sort to ant-time order
         J1 = 1
         J2 = 2
         IF (BPBUFF(43).NE.KOLS(J1)) THEN
C                                       Close
            CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE CP TABLE'
               GO TO 990
               END IF
C                                       sort
            KEY(1,1) = KOLS(J1)
            KEY(2,1) = KOLS(J1)
            KEY(1,2) = KOLS(J2)
            KEY(2,2) = KOLS(J2)
            CALL TABSRT (INDISK, CNO, 'CP', INVERS, INVERS, KEY, KEYSUB,
     *         FKEY, BPBUFF, CATBLK, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'SORT CP TABLE'
               GO TO 990
               END IF
            CALL CPINI ('READ', BPBUFF, INDISK, CNO, INVERS, CATBLK,
     *         BPLUN, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP, J, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN SORTED CP TABLE'
               GO TO 990
               END IF
            END IF
         END IF
C                                       number of records
      NBPINR = BPBUFF(5)
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, NCHNBP))
      ECHAN = IROUND (XECHAN)
      IF ((ECHAN.LE.BCHAN) .OR. (ECHAN.GT.NCHNBP)) ECHAN = NCHNBP
      XBCHAN = BCHAN
      XECHAN = ECHAN
      BIF = IROUND (XBIF)
      BIF = MAX (1, MIN (BIF, NIFBP))
      EIF = IROUND (XEIF)
      IF ((EIF.LT.BIF) .OR. (EIF.GT.NIFBP)) EIF = NIFBP
      XBIF = BIF
      XEIF = EIF
      CALL H2CHR (4, 1, XTYPE, PTYPE)
      IF ((PTYPE.NE.'PHAS') .AND. (PTYPE.NE.'DIFA') .AND.
     *   (PTYPE.NE.'DIFP') .AND. (PTYPE.NE.'ADIF') .AND.
     *   (PTYPE.NE.'PDIF') .AND. (PTYPE.NE.'REAL') .AND.
     *   (PTYPE.NE.'IMAG') .AND. (PTYPE.NE.'DIFR') .AND.
     *   (PTYPE.NE.'DIFI')) PTYPE = 'AMP '
      CALL CHR2H (4, PTYPE, 1, XTYPE)
      IF (PTYPE.EQ.'ADIF') PTYPE = 'DIFC'
      IF (PTYPE.EQ.'PDIF') PTYPE = 'DIFB'
      CALL H2CHR (4, 1, XSTOKE, STAT)
      DOSTOK = ' '
      IF ((NPOLBP.EQ.1) .OR. (STAT(:1).EQ.'R') .OR. (TTYPE.EQ.'CP'))
     *   THEN
         BPOL = 1
         EPOL = 1
         STAT = 'R'
      ELSE IF (STAT(:1).EQ.'L') THEN
         BPOL = 2
         EPOL = 2
         STAT = 'L'
      ELSE
         BPOL = 1
         EPOL = 1
         IF ((STAT(:1).EQ.'I') .AND. (TTYPE.EQ.'BP')) THEN
            STAT = 'R+L'
            DOSTOK = STAT
         ELSE IF ((STAT(:1).EQ.'V') .AND. (TTYPE.EQ.'BP')) THEN
            STAT = 'R-L'
            DOSTOK = STAT
         ELSE
            EPOL = 2
            STAT = 'RL'
            END IF
         END IF
      CALL CHR2H (4, STAT, 1, XSTOKE)
      IF (FACTOR.LE.0.0) FACTOR = 1.0
      ARANGE(1) = XRANGE(1)
      ARANGE(2) = XRANGE(2)
      TIMBEG = ((XTIME(4)/60.0 + XTIME(3)) / 60.0 + XTIME(2)) / 24.0 +
     *   XTIME(1)
      TIMEND = ((XTIME(8)/60.0 + XTIME(7)) / 60.0 + XTIME(6)) / 24.0 +
     *   XTIME(5)
      TPLBEG = TIMBEG
      TPLEND = TIMEND
      IF ((TIMEND.LE.TIMBEG) .OR. (TIMEND.LE.0.0) .OR. (TTYPE.EQ.'CP')
     *   .OR. (TTYPE.EQ.'PD')) THEN
         TIMEND = 9999.
         TIMBEG = -100.
         END IF
      IF (TPLEND.EQ.TPLBEG) THEN
         TPLEND = 9999.
         TPLBEG = -100.0
      ELSE IF (TPLEND.LT.TPLBEG) THEN
         TEMP = TPLEND
         TPLEND = TPLBEG
         TPLBEG = TEMP
         END IF
      IF (PTYPE(:3).NE.'DIF') THEN
         TPLBEG = TIMBEG
         TPLEND = TIMEND
         END IF
      FQID = IROUND (XFQ)
      IF (FQID.LE.0) FQID = 1
      SUBARR = IROUND (XSUB)
      IF (SUBARR.LE.0) SUBARR = 1
      LABEL = IROUND (XLTYPE)
      CALL FILL (MAXANT, 0, IANTS)
      CALL FILL (NANTBP, 1, IANTS)
      J1 = 0
      DO 10 J = 1,50
         J2 = IROUND (XANT(J))
         IF (J2.LT.0) THEN
            J1 = J2
         ELSE IF (J1.GE.0) THEN
            J1 = MAX (J1, J2)
            END IF
 10      CONTINUE
C                                       none negative, some > 0
      NIANTS = NANTBP
      IF (J1.GT.0) THEN
         NIANTS = 0
         CALL FILL (NANTBP, 0, IANTS)
         DO 20 J = 1,50
            J2 = IROUND (XANT(J))
            IF ((J2.GT.0) .AND. (J2.LE.NANTBP)) THEN
               IF (IANTS(J2).LE.0) NIANTS = NIANTS + 1
               IANTS(J2) = 1
               END IF
 20         CONTINUE
C                                       some negative
      ELSE IF (J1.LT.0) THEN
         DO 30 J = 1,50
            J2 = IROUND (ABS (XANT(J)))
            IF ((J2.GT.0) .AND. (J2.LE.NANTBP)) THEN
               IF (IANTS(J2).GT.0) NIANTS = NIANTS - 1
               IANTS(J2) = 0
               END IF
 30         CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPLOTI: ERROR',I5,' ON ',A)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
      END
      SUBROUTINE BPLOTT (IERR)
C-----------------------------------------------------------------------
C   BPLOTT uses the open BP table to generate the plots.
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER PFILE*48, CLABEL*4, STRING*12, FUNC*12
      INTEGER   I, J, J1, J2, NREC, SOURID, SUBA, ANTNO, FREQID, IP, IA,
     *   BPREF(2), JERR, ANTS(MAXANT), THEANT, IIF, IIC, IIP, MREC,
     *   LINE, TVCHN, GRCHN, TVCORN(2), IVER, IROUND, IL, ITT(4), NP,
     *   LP, ITYPE, NPLOT, NRECI, NRECC, KREC
      REAL      TIMES(4000), INTERV, WT, D, DMAX, DMIN, BPSUM(3,MAXCIF),
     *   BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), NSUM(MAXCIF), DX, DY, X0,
     *   Y0, SCALE, X, Y, SOFF, PLPTS(MAXCIF+100), XPLPTS(MAXCIF+100),
     *   PMAX, PMIN, XT, COL(3), COLV, DCOLV, YP, YH, PPMAX, PPMIN,
     *   LMAX, LMIN
      LOGICAL   DOTV, LBLANK, SPHASE
      DOUBLE PRECISION TTIME, DTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DTIME = 5. / 60.0 / 24.0
      J2 = 0
      NPLOT = 0
      LTYPE = MOD (ABS(LABEL), 100)
C                                       loop for next plot
 10   J1 = J2 + 1
C                                       find range of desired data
         NREC = 0
         KREC = 0
         MREC = 0
         DO 90 J = J1,NBPINR
            IBPRNO = J
            CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *         CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (TIME.GE.TIMBEG) .AND. (TIME.LE.TIMEND) .AND.
     *         (IANTS(ANTNO).GT.0)) THEN
               IF (NREC.EQ.0) THEN
                  J1 = J
                  KREC = 0
                  THEANT = ANTNO
                  CALL FILL (MAXANT, 0, ANTS)
                  TTIME = TIME
                  DMAX = -1.E10
                  DMIN = -DMAX
                  PPMAX = DMAX
                  PPMIN = DMIN
                  IF (PTYPE(:3).EQ.'DIF') THEN
                     I = NPOLBP * NCHNBP * NIFBP
                     CALL RFILL (I, 0.0, NSUM)
                     I = 3 * I
                     CALL RFILL (I, 0.0, BPSUM)
                     END IF
                  END IF
               IF (PSORT.EQ.'TA') THEN
                  IF (ANTS(ANTNO).GT.0) GO TO 100
                  IF (TIME-TTIME.GT.DTIME) GO TO 100
               ELSE
                  IF (ANTNO.NE.THEANT) GO TO 100
                  END IF
C                                       check for valid data
               DO 15 IIP = BPOL,EPOL
                  DO 14 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     WT = WEIGHT(I)
                     IF (WT.LE.0) GO TO 14
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     DO 13 IIC = BCHAN,ECHAN
                        I = I + 1
                        IF (BNDPAS(1,I).NE.FBLANK) GO TO 18
 13                     CONTINUE
 14                  CONTINUE
 15               CONTINUE
               J2 = J
               ANTS(ANTNO) = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  KREC = KREC + 1
                  TIMES(KREC) = TIME
                  END IF
               GO TO 90
 18            MREC = MREC + 1
               ANTS(ANTNO) = J
               J2 = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  NREC = NREC + 1
                  KREC = KREC + 1
                  TIMES(KREC) = TIME
                  END IF
C                                       average spectrum
               IF (PTYPE(:3).EQ.'DIF') THEN
                  DO 30 IIP = BPOL,EPOL
                     DO 25 IIF = BIF,EIF
                        I = IIF + (IIP - 1) * NIFBP
                        WT = WEIGHT(I)
                        IF (WT.LE.0) GO TO 25
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        DO 20 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF (BNDPAS(1,I).NE.FBLANK) THEN
                              BPSUM(1,I) = BPSUM(1,I) + WT*BNDPAS(1,I)
                              BPSUM(2,I) = BPSUM(2,I) + WT*BNDPAS(2,I)
                              BPSUM(3,I) = BPSUM(3,I) + WT * SQRT
     *                           (BNDPAS(1,I)**2 + BNDPAS(2,I)**2)
                              NSUM(I) = NSUM(I) + WT
                              END IF
 20                        CONTINUE
 25                     CONTINUE
 30                  CONTINUE
C                                       or find max/min
               ELSE IF (ARANGE(2).LE.ARANGE(1)) THEN
                  DO 50 IIP = BPOL,EPOL
                     DO 45 IIF = BIF,EIF
                        I = IIF + (IIP - 1) * NIFBP
                        WT = WEIGHT(I)
                        IF (WT.LE.0) GO TO 45
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        DO 40 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF (BNDPAS(1,I).NE.FBLANK) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'AMP ') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2)
                              ELSE IF (PTYPE.EQ.'REAL') THEN
                                 D =  BNDPAS(1,I)
                              ELSE IF (PTYPE.EQ.'IMAG') THEN
                                 D =  BNDPAS(2,I)
                              ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                           (BNDPAS(2,I).NE.0.0)) THEN
                                 D = ATAN2 (BNDPAS(2,I), BNDPAS(1,I)) *
     *                              RAD2DG
                                 END IF
                              DMAX = MAX (D, DMAX)
                              DMIN = MIN (D, DMIN)
                              IF (D.LT.0.0) D = D + 360.0
                              PPMAX = MAX (D, PPMAX)
                              PPMIN = MIN (D, PPMIN)
                              END IF
 40                        CONTINUE
 45                     CONTINUE
 50                  CONTINUE
                  END IF
               END IF
 90         CONTINUE
         IF (PSORT.EQ.'TA') NIANTS = 0
C                                       Found no more data
 100     IF (MREC.LE.0) THEN
            MSGTXT = 'Done with all selected data'
            CALL MSGWRT (2)
            IF (NPLOT.GT.0) THEN
               IERR = 0
            ELSE
               IERR = 1
               MSGTXT = 'But no plots were done'
               CALL MSGWRT (7)
               END IF
            GO TO 999
            END IF
C                                       found data but no plot
         IF (NREC.EQ.0) THEN
            IF (J2.LT.NBPINR) GO TO 10
            GO TO 300
            END IF
         NREC = KREC
C                                       average spectrum
         IF (PTYPE(:3).EQ.'DIF') THEN
            DO 120 IIP = BPOL,EPOL
               DO 115 IIF = BIF,EIF
                  I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) * NCHNBP +
     *               BCHAN - 1
                  DO 110 IIC = BCHAN,ECHAN
                     I = I + 1
                     WT = NSUM(I)
                     IF (WT.GT.0.0) THEN
                        BPSUM(3,I) = BPSUM(3,I) / WT
                        WT = SQRT (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                        IF (WT.GT.0.0) WT = BPSUM(3,I) / WT
                        BPSUM(1,I) = BPSUM(1,I) * WT
                        BPSUM(2,I) = BPSUM(2,I) * WT
                        END IF
 110                 CONTINUE
 115              CONTINUE
 120           CONTINUE
            END IF
C                                       now get scale
         IF ((PTYPE(:3).EQ.'DIF') .AND. (ARANGE(2).LE.ARANGE(1))) THEN
            DO 150 J = J1,J2
               IBPRNO = J
               CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *            NIFBP, NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA,
     *            ANTNO, CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS,
     *            IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
                  GO TO 990
                  END IF
               IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *            ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *            (TIME.GE.TPLBEG) .AND. (TIME.LE.TPLEND) .AND.
     *            (IANTS(ANTNO).GT.0)) THEN
                  DO 140 IIP = BPOL,EPOL
                     DO 135 IIF = BIF,EIF
                        I = IIF + (IIP - 1) * NIFBP
                        WT = WEIGHT(I)
                        IF (WT.LE.0) GO TO 135
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        DO 130 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                        (NSUM(I).GT.0.0)) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'DIFC') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2) - SQRT
     *                              (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                              ELSE IF (PTYPE.EQ.'DIFB') THEN
                                 IF (((BNDPAS(1,I).NE.0.0) .OR.
     *                              (BNDPAS(2,I).NE.0.0)) .AND.
     *                              ((BPSUM(1,I).NE.0.0) .OR.
     *                              (BPSUM(2,I).NE.0.0))) THEN
                                    D = (ATAN2 (BNDPAS(2,I),BNDPAS(1,I))
     *                                 - ATAN2 (BPSUM(2,I), BPSUM(1,I)))
     *                                 * RAD2DG
                                    IF (D.LT.-180.0) D = D + 360.0
                                    IF (D.GT.180.0) D = D - 360.0
                                    END IF
                              ELSE
                                 BNDPAS(1,I) = BNDPAS(1,I) - BPSUM(1,I)
                                 BNDPAS(2,I) = BNDPAS(2,I) - BPSUM(2,I)
                                 IF (PTYPE.EQ.'DIFA') THEN
                                    D =  SQRT (BNDPAS(1,I)**2 +
     *                                 BNDPAS(2,I)**2)
                                 ELSE IF (PTYPE.EQ.'DIFR') THEN
                                    D =  BNDPAS(1,I)
                                 ELSE IF (PTYPE.EQ.'DIFI') THEN
                                    D =  BNDPAS(2,I)
                                 ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                              (BNDPAS(2,I).NE.0.0)) THEN
                                    D = ATAN2 (BNDPAS(2,I), BNDPAS(1,I))
     *                                 * RAD2DG
                                    END IF
                                 END IF
                              DMAX = MAX (D, DMAX)
                              DMIN = MIN (D, DMIN)
                              IF (D.LT.0.0) D = D + 360.0
                              PPMAX = MAX (D, PPMAX)
                              PPMIN = MIN (D, PPMIN)
                              END IF
 130                       CONTINUE
 135                    CONTINUE
 140                 CONTINUE
                  END IF
 150           CONTINUE
            END IF
C                                       set scale, blank array
         I = MAXCIF + 100
         CALL RFILL (I, 0.0, XPLPTS)
         IF (ARANGE(2).GT.ARANGE(1)) THEN
            PMIN = XRANGE(1)
            PMAX = XRANGE(2)
            SPHASE = PMIN.GE.0.0
         ELSE
            IF (DMAX-DMIN.LE.PPMAX-PPMIN) THEN
               PMIN = DMIN
               PMAX = DMAX
               SPHASE = .FALSE.
            ELSE
               PMIN = PPMIN
               PMAX = PPMAX
               SPHASE = .TRUE.
               END IF
            XRANGE(1) = PMIN
            XRANGE(2) = PMAX
            END IF
         IF (PMIN.EQ.PMAX) THEN
            IF (PMIN.EQ.0.0) THEN
               PMAX = 1.
               PMIN = -1.
            ELSE
               PMAX = 1.03 * PMIN
               PMIN = 0.97 * PMIN
               END IF
            END IF
C                                       now init the plot
         DOTV = XDOTV.GT.0.0
         TVCHN = 0
         TVCORN(1) = 0
         TVCORN(2) = 0
         GRCHN = IROUND (XCHAN)
         PLUN = 26
         IVER = 0
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', INDISK, CNO, CATBLK, PBUFF, .TRUE.,
     *         'WRIT', IVER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CREATE PLOT FILE'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
C                                       time/antenna adverbs to match
C                                       one time
         CALL RFILL (8, 0.0, XTIME)
         IF (PSORT.EQ.'TA') THEN
            XTIME(1) = TTIME
            XTIME(5) = TTIME
C                                       one antenna
         ELSE
            XTIME(1) = TPLBEG
            XTIME(5) = TPLEND
            CALL RFILL (50, 0.0, XANT)
            XANT(1) = THEANT
            END IF
         CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
         ITYPE = 30
         CALL GINIT (INDISK, CNO, PFILE, 0, ITYPE, NPARMS, USER, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND, IERR)
         FUNC = 'GINIT'
         IF (IERR.NE.0) GO TO 900
         CALL BPLABL (IVER, PMIN, PMAX, NREC, TIMES, ANTS, IERR)
         FUNC = 'BPLABL'
         IF (IERR.NE.0) GO TO 900
         DY = 1000.0 / (NREC + FACTOR)
         NP = (EPOL-BPOL+1) * (EIF-BIF+1)
         DX = NP*(ECHAN-BCHAN+4)
         DX = 1000.0 / DX
         X0 = DX / 2.0
         Y0 = -DY / 2.0
         XT = MAX (X0, 17.5)
         DCOLV = 0.97 / MAX (1, NREC-1)
         IF (DO3COL.EQ.2) DCOLV = 0.97 / (PMAX - PMIN)
         COLV = 0.97
C                                       get the data part
         MREC = J2 - J1 + 1
         IF (PSORT.EQ.'TA') MREC = NREC
         IA = 0
         LINE = 0
         NRECC = 0
         NRECI = MAX (1, NREC / 20)
         DO 290 J = 1,MREC
            IBPRNO = J + J1 - 1
            IF (PSORT.EQ.'TA') THEN
 225           IA = IA + 1
               IBPRNO = ANTS(IA)
               IF (IBPRNO.LE.0) GO TO 225
               END IF
            CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *         CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (TIME.GE.TPLBEG) .AND. (TIME.LE.TPLEND) .AND.
     *         (IANTS(ANTNO).GT.0)) THEN
               NRECC = NRECC + 1
               IF (DO3COL.EQ.1) THEN
                  CALL COLOR3 (COLV, .FALSE., COL)
                  COLV = COLV - DCOLV
                  CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                  FUNC = 'G3VCOL'
                  IF (IERR.NE.0) GO TO 900
                  END IF
               Y0 = Y0 + DY
               IP = 0
               LINE = LINE + 1
               LMAX = -1.E10
               LMIN = 1.E10
               DO 240 IIP = BPOL,EPOL
                  DO 235 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     WT = WEIGHT(I)
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
                     DO 230 IIC = BCHAN,ECHAN
                        I = I + 1
                        IP = IP + 1
                        D = FBLANK
                        IF ((WT.GT.0.0) .AND. (BNDPAS(1,I).NE.FBLANK))
     *                     THEN
                           IF (PTYPE(:3).EQ.'DIF') THEN
                              IF (NSUM(I).GT.0.0) THEN
                                 D = 0.0
                                 IF (PTYPE.EQ.'DIFC') THEN
                                    D =  SQRT (BNDPAS(1,I)**2 +
     *                                 BNDPAS(2,I)**2) - SQRT
     *                                 (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                                 ELSE IF (PTYPE.EQ.'DIFB') THEN
                                    IF (((BNDPAS(1,I).NE.0.0) .OR.
     *                                 (BNDPAS(2,I).NE.0.0)) .AND.
     *                                 ((BPSUM(1,I).NE.0.0) .OR.
     *                                 (BPSUM(2,I).NE.0.0))) THEN
                                       D = (ATAN2 (BNDPAS(2,I),
     *                                    BNDPAS(1,I)) - ATAN2
     *                                    (BPSUM(2,I), BPSUM(1,I)))
     *                                    * RAD2DG
                                       IF (D.LT.-180.0) D = D + 360.0
                                       IF (D.GT.180.0) D = D - 360.0
                                       IF ((SPHASE) .AND. (D.LT.0.0))
     *                                    D = D + 360.0
                                       END IF
                                 ELSE
                                    BNDPAS(1,I) = BNDPAS(1,I)-BPSUM(1,I)
                                    BNDPAS(2,I) = BNDPAS(2,I)-BPSUM(2,I)
                                    IF (PTYPE.EQ.'DIFA') THEN
                                       D =  SQRT (BNDPAS(1,I)**2 +
     *                                    BNDPAS(2,I)**2)
                                    ELSE IF (PTYPE.EQ.'DIFR') THEN
                                       D =  BNDPAS(1,I)
                                    ELSE IF (PTYPE.EQ.'DIFI') THEN
                                       D =  BNDPAS(2,I)
                                    ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                                 (BNDPAS(2,I).NE.0.0)) THEN
                                       D = ATAN2 (BNDPAS(2,I),
     *                                    BNDPAS(1,I)) * RAD2DG
                                       IF ((SPHASE) .AND. (D.LT.0.0))
     *                                    D = D + 360.0
                                       END IF
                                    END IF
                                 END IF
                           ELSE
                              D = 0.0
                              IF (PTYPE.EQ.'AMP') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2)
                              ELSE IF (PTYPE.EQ.'REAL') THEN
                                 D =  BNDPAS(1,I)
                              ELSE IF (PTYPE.EQ.'IMAG') THEN
                                 D =  BNDPAS(2,I)
                              ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                           (BNDPAS(2,I).NE.0.0)) THEN
                                 D = ATAN2 (BNDPAS(2,I), BNDPAS(1,I))
     *                              * RAD2DG
                                 IF ((SPHASE) .AND. (D.LT.0.0))
     *                              D = D + 360.0
                                 END IF
                              END IF
                           END IF
                        IF ((D.NE.FBLANK) .AND. ((D.LT.PMIN) .OR.
     *                     (D.GT.PMAX))) D = FBLANK
                        IF (D.NE.FBLANK) THEN
                           LMAX = MAX (LMAX, D)
                           LMIN = MIN (LMIN, D)
                           END IF
                        PLPTS(IP) = D
 230                    CONTINUE
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
 235                 CONTINUE
 240              CONTINUE
               IF (ARANGE(2).GE.ARANGE(1)) THEN
                  LMAX = PMAX
                  LMIN = PMIN
                  END IF
               IF (DO3COL.EQ.2) DCOLV = 0.97 / (LMAX - LMIN)
C                                       Do the plot now
               SCALE = DY * FACTOR / (LMAX - LMIN)
C                                       Don't label if too crowded
               IF (MOD(NRECC-1,NRECI).EQ.0) THEN
                  IF ((LMAX.GE.0.0) .AND. (LMIN.LE.0.0)) THEN
                     SOFF = Y0 - LMIN * SCALE
                     IF (DO3COL.GE.2) COLV = LMAX * DCOLV
                  ELSE
                     SOFF = Y0 + 0.5 * DY * FACTOR
                     IF (DO3COL.GE.2) COLV = 0.5
                     END IF
                  CALL GLTYPE (1, PBUFF, IERR)
                  FUNC = 'GLTYPE'
                  IF (IERR.NE.0) GO TO 900
                  IF (DO3COL.GE.2) THEN
                     CALL COLOR3 (COLV, .FALSE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                     FUNC = 'G3VCOL'
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  FUNC = 'GPOS/VECs'
                  X = 0.0
                  DO 245 LP = 2,NP
                     X = X + (ECHAN-BCHAN+4) * DX
                     CALL GPOS (X+XT, SOFF, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X-XT, SOFF, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X-XT, SOFF, PBUFF, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 900
 245                 CONTINUE
                  CALL GPOS (1000.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (1000.0-XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GPOS (XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (0.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  FUNC = 'GCHAR'
                  IF ((LTYPE.EQ.3) .AND. (PSORT.NE.'TA')) THEN
                     Y = TIME
                     CALL TODHMS (Y, ITT)
                     WRITE (STRING,1239) ITT
                     Y = -10.5
                     CALL GCHAR (10, 0, Y, -0.5, STRING(:10), PBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 900
                  ELSE IF (LTYPE.GT.2) THEN
                     IL = LINE
                     IF (PSORT.EQ.'TA') IL = IA
                     WRITE (CLABEL,1240) IL
                     I = 4
                     IF (IL.GT.9) I = 3
                     IF (IL.GT.99) I = 2
                     IF (IL.GT.999) I = 1
                     Y = -(5-I) - 0.5
                     CALL GCHAR (5-I, 0, Y, -0.5, CLABEL(I:4), PBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  END IF
               CALL GLTYPE (2, PBUFF, IERR)
               FUNC = 'GLTYPE'
               IF (IERR.NE.0) GO TO 900
               LBLANK = .TRUE.
               X = X0
               DO 250 I = 1,IP
                  IF (PLPTS(I).EQ.FBLANK) THEN
                     LBLANK = .TRUE.
                     X = X + DX
                  ELSE
                     Y = Y0 + (PLPTS(I)-LMIN) * SCALE
                     IF ((LBLANK) .OR. (Y.LT.XPLPTS(I))) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                        IF (DO3COL.GE.2) THEN
                           YP = Y
                           COLV = (LMAX-PLPTS(I)) * DCOLV
                           CALL COLOR3 (COLV, .FALSE., COL)
                           CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                        IERR)
                           FUNC = 'G3VCOL'
                           IF (IERR.NE.0) GO TO 900
                           END IF
                     ELSE IF (DO3COL.GE.2) THEN
                        YH = (YP + Y) / 2.0
                        CALL G3VEC (X, YH, PBUFF, IERR)
                        YP = Y
                        COLV = (LMAX-PLPTS(I)) * DCOLV
                        CALL COLOR3 (COLV, .FALSE., COL)
                        CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                     IERR)
                        FUNC = 'G3VCOL'
                        IF (IERR.NE.0) GO TO 900
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.EQ.1) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        END IF
                     FUNC = 'GVEC'
                     IF (IERR.NE.0) GO TO 900
                     LBLANK = .FALSE.
                     X = X + DX
                     IF (Y.LT.XPLPTS(I)) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                        END IF
                     IF (IERR.NE.0) GO TO 900
                     END IF
 250              CONTINUE
               END IF
 290        CONTINUE
C                                       finish plot and continue?
         IF (PSORT.NE.'TA') NIANTS = NIANTS - 1
         GPHPAG = (DOTV) .AND. (J2.LT.NBPINR) .AND. (NIANTS.GT.0)
         CALL GFINIS (PBUFF, IERR)
C                                       Successful plot file finished.
         IF (IERR.EQ.0) THEN
            NPLOT = NPLOT + 1
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (INDISK, CNO, IVER, PBUFF, IERR)
               WRITE (MSGTXT,1290) IVER
               CALL MSGWRT (5)
               IERR = 0
               END IF
            END IF
C                                       loop
         IF ((J2.LT.NBPINR) .AND. (IERR.EQ.0)) GO TO 10
 300  IERR = MAX (0, IERR)
C                                       close
      CALL TABBP ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO, CHNBND, BPFREQ,
     *   FREQID, BPREF, WEIGHT, BNDPAS, JERR)
      GO TO 999
C                                       plotter error
 900  WRITE (MSGTXT,1900) IERR, FUNC
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO SAVE PARTIAL GRAPH'
      CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPLOTT: ERROR',I5,' ON ',A)
 1239 FORMAT (I1,'/',2(I2.2,':'),I2.2)
 1240 FORMAT (I4)
 1290 FORMAT ('Successful plot file version',I5,' written')
 1900 FORMAT ('PLOTTING ERROR',I5,' AT ',A)
      END
      SUBROUTINE BPLOTS (IERR)
C-----------------------------------------------------------------------
C   BPLOTS uses the open BP table to generate the plots of R+L, R-L
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER PFILE*48, CLABEL*4, STRING*12, FUNC*12
      INTEGER   I, J, J1, J2, NREC, SOURID, SUBA, ANTNO, FREQID, IP, IA,
     *   BPREF(2), JERR, ANTS(MAXANT), THEANT, IIF, IIC, MREC, II,
     *   LINE, TVCHN, GRCHN, TVCORN(2), IVER, IROUND, IL, ITT(4), NP,
     *   LP, ITYPE, NPLOT, NRECI, NRECC, KREC
      REAL      TIMES(4000), INTERV, WT, D, DMAX, DMIN, BPSUM(3,MAXCIF),
     *   BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), NSUM(MAXCIF), DX, DY, X0,
     *   Y0, SCALE, X, Y, SOFF, PLPTS(MAXCIF+100), XPLPTS(MAXCIF+100),
     *   PMAX, PMIN, XT, COL(3), COLV, DCOLV, YP, YH, PPMAX, PPMIN,
     *   LMAX, LMIN, WT1, WT2, LMUL
      LOGICAL   DOTV, LBLANK, SPHASE
      DOUBLE PRECISION TTIME, DTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DTIME = 5. / 60.0 / 24.0
      J2 = 0
      NPLOT = 0
      LMUL = 1.0
      IF (DOSTOK.EQ.'R-L') LMUL = -1.0
      LTYPE = MOD (ABS(LABEL), 100)
C                                       loop for next plot
 10   J1 = J2 + 1
C                                       find range of desired data
         NREC = 0
         MREC = 0
         KREC = 0
         DO 90 J = J1,NBPINR
            IBPRNO = J
            CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *         CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (TIME.GE.TIMBEG) .AND. (TIME.LE.TIMEND) .AND.
     *         (IANTS(ANTNO).GT.0)) THEN
               IF (NREC.EQ.0) THEN
                  J1 = J
                  KREC = 0
                  THEANT = ANTNO
                  CALL FILL (MAXANT, 0, ANTS)
                  TTIME = TIME
                  DMAX = -1.E10
                  DMIN = -DMAX
                  PPMAX = DMAX
                  PPMIN = DMIN
                  IF (PTYPE(:3).EQ.'DIF') THEN
                     I = NPOLBP * NCHNBP * NIFBP
                     CALL RFILL (I, 0.0, NSUM)
                     I = 3 * I
                     CALL RFILL (I, 0.0, BPSUM)
                     END IF
                  END IF
               IF (PSORT.EQ.'TA') THEN
                  IF (ANTS(ANTNO).GT.0) GO TO 100
                  IF (TIME-TTIME.GT.DTIME) GO TO 100
               ELSE
                  IF (ANTNO.NE.THEANT) GO TO 100
                  END IF
C                                       check for valid data
               DO 15 IIF = BIF,EIF
                  I = IIF + NIFBP
                  WT1 = WEIGHT(IIF)
                  WT2 = WEIGHT(I)
                  IF ((WT1.LE.0) .OR. (WT2.LE.0)) GO TO 15
                  I = (IIF - 1) * NCHNBP + BCHAN - 1
                  II = NCHNBP * NIFBP + I
                  DO 13 IIC = BCHAN,ECHAN
                     I = I + 1
                     II = II + 1
                     IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                  (BNDPAS(1,II).NE.FBLANK)) GO TO 18
 13                  CONTINUE
 15               CONTINUE
               J2 = J
               ANTS(ANTNO) = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  KREC = KREC + 1
                  TIMES(KREC) = TIME
                  END IF
               GO TO 90
 18            MREC = MREC + 1
               ANTS(ANTNO) = J
               J2 = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  NREC = NREC + 1
                  KREC = KREC + 1
                  TIMES(KREC) = TIME
                  END IF
C                                       average spectrum
               IF (PTYPE(:3).EQ.'DIF') THEN
                  DO 25 IIF = BIF,EIF
                     II = IIF + NIFBP
                     WT1 = WEIGHT(IIF)
                     WT2 = WEIGHT(II)
                     IF ((WT1.LE.0) .OR. (WT2.LE.0)) GO TO 25
                     I = (IIF - 1) * NCHNBP + BCHAN - 1
                     II = NCHNBP * NIFBP + I
                     WT = (WT1 + WT2) / 2.
                     DO 20 IIC = BCHAN,ECHAN
                        I = I + 1
                        II = II + 1
                        IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                     (BNDPAS(1,II).NE.FBLANK)) THEN
                           X = (BNDPAS(1,I) + LMUL*BNDPAS(1,II)) / 2.0
                           Y = (BNDPAS(2,I) + LMUL*BNDPAS(2,II)) / 2.0
                           BPSUM(1,I) = BPSUM(1,I) + WT * X
                           BPSUM(2,I) = BPSUM(2,I) + WT * Y
                           BPSUM(3,I) = BPSUM(3,I) +
     *                        WT * SQRT (X*X + Y*Y)
                           NSUM(I) = NSUM(I) + WT
                           END IF
 20                     CONTINUE
 25                  CONTINUE
C                                       or find max/min
               ELSE IF (ARANGE(2).LE.ARANGE(1)) THEN
                  DO 45 IIF = BIF,EIF
                     I = IIF + NIFBP
                     WT1 = WEIGHT(IIF)
                     WT2 = WEIGHT(I)
                     IF ((WT1.LE.0) .OR. (WT2.LE.0)) GO TO 45
                     I = (IIF - 1) * NCHNBP + BCHAN - 1
                     II = NCHNBP * NIFBP + I
                     DO 40 IIC = BCHAN,ECHAN
                        I = I + 1
                        II = II + 1
                        IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                     (BNDPAS(1,II).NE.FBLANK)) THEN
                           D = 0.0
                           X = (BNDPAS(1,I) + LMUL*BNDPAS(1,II)) / 2.0
                           Y = (BNDPAS(2,I) + LMUL*BNDPAS(2,II)) / 2.0
                           IF (PTYPE.EQ.'AMP ') THEN
                              D =  SQRT (X*X + Y*Y)
                           ELSE IF (PTYPE.EQ.'REAL') THEN
                              D =  X
                           ELSE IF (PTYPE.EQ.'IMAG') THEN
                              D =  Y
                           ELSE IF ((X.NE.0.0) .OR. (Y.NE.0.0)) THEN
                              D = ATAN2 (Y, X) * RAD2DG
                              END IF
                           DMAX = MAX (D, DMAX)
                           DMIN = MIN (D, DMIN)
                           IF (D.LT.0.0) D = D + 360.0
                           PPMAX = MAX (D, PPMAX)
                           PPMIN = MIN (D, PPMIN)
                           END IF
 40                     CONTINUE
 45                  CONTINUE
                  END IF
               END IF
 90         CONTINUE
         IF (PSORT.EQ.'TA') NIANTS = 0
C                                       Found no more data
 100     IF (MREC.LE.0) THEN
            MSGTXT = 'Done with all selected data'
            CALL MSGWRT (2)
            IF (NPLOT.GT.0) THEN
               IERR = 0
            ELSE
               IERR = 1
               MSGTXT = 'But no plots were done'
               CALL MSGWRT (7)
               END IF
            GO TO 999
            END IF
C                                       found data but no plot
         IF (NREC.EQ.0) THEN
            IF (J2.LT.NBPINR) GO TO 10
            GO TO 300
            END IF
         NREC = KREC
C                                       average spectrum
         IF (PTYPE(:3).EQ.'DIF') THEN
            DO 115 IIF = BIF,EIF
               I = (IIF - 1) * NCHNBP + BCHAN - 1
               DO 110 IIC = BCHAN,ECHAN
                  I = I + 1
                  WT = NSUM(I)
                  IF (WT.GT.0.0) THEN
                     BPSUM(3,I) = BPSUM(3,I) / WT
                     WT = SQRT (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                     IF (WT.GT.0.0) WT = BPSUM(3,I) / WT
                     BPSUM(1,I) = BPSUM(1,I) * WT
                     BPSUM(2,I) = BPSUM(2,I) * WT
                     END IF
 110              CONTINUE
 115           CONTINUE
            END IF
C                                       now get scale
         IF ((PTYPE(:3).EQ.'DIF') .AND. (ARANGE(2).LE.ARANGE(1))) THEN
            DO 150 J = J1,J2
               IBPRNO = J
               CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *            NIFBP, NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA,
     *            ANTNO, CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS,
     *            IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
                  GO TO 990
                  END IF
               IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *            ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *            (TIME.GE.TPLBEG) .AND. (TIME.LE.TPLEND) .AND.
     *            (IANTS(ANTNO).GT.0)) THEN
                  DO 135 IIF = BIF,EIF
                     I = IIF + NIFBP
                     WT1 = WEIGHT(IIF)
                     WT2 = WEIGHT(I)
                     IF ((WT1.LE.0.) .OR. (WT2.LE.0.)) GO TO 135
                     I = (IIF - 1) * NCHNBP + BCHAN - 1
                     II = NCHNBP * NIFBP + I
                     DO 130 IIC = BCHAN,ECHAN
                        I = I + 1
                        II = II + 1
                        IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                     (BNDPAS(1,II).NE.FBLANK) .AND.
     *                     (NSUM(I).GT.0.0)) THEN
                           D = 0.0
                           X = (BNDPAS(1,I) + LMUL*BNDPAS(1,II)) / 2.0
                           Y = (BNDPAS(2,I) + LMUL*BNDPAS(2,II)) / 2.0
                           IF (PTYPE.EQ.'DIFC') THEN
                              D =  SQRT (X*X + Y*Y) - SQRT
     *                           (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                           ELSE IF (PTYPE.EQ.'DIFB') THEN
                              IF (((X.NE.0.0) .OR. (Y.NE.0.0)) .AND.
     *                           ((BPSUM(1,I).NE.0.0) .OR.
     *                           (BPSUM(2,I).NE.0.0))) THEN
                                 D = RAD2DG * (ATAN2 (Y,X)
     *                              - ATAN2 (BPSUM(2,I), BPSUM(1,I)))
                                 IF (D.LT.-180.0) D = D + 360.0
                                 IF (D.GT.180.0) D = D - 360.0
                                 END IF
                           ELSE
                              X = X - BPSUM(1,I)
                              Y = Y - BPSUM(2,I)
                              IF (PTYPE.EQ.'DIFA') THEN
                                 D =  SQRT (X * X + Y * Y)
                              ELSE IF (PTYPE.EQ.'DIFR') THEN
                                 D =  X
                              ELSE IF (PTYPE.EQ.'DIFI') THEN
                                 D =  Y
                              ELSE IF ((X.NE.0.0) .OR. (Y.NE.0.0)) THEN
                                 D = ATAN2 (Y, X) * RAD2DG
                                 END IF
                              END IF
                           DMAX = MAX (D, DMAX)
                           DMIN = MIN (D, DMIN)
                           IF (D.LT.0.0) D = D + 360.0
                           PPMAX = MAX (D, PPMAX)
                           PPMIN = MIN (D, PPMIN)
                           END IF
 130                    CONTINUE
 135                 CONTINUE
                  END IF
 150           CONTINUE
            END IF
C                                       set scale, blank array
         I = MAXCIF + 100
         CALL RFILL (I, 0.0, XPLPTS)
         IF (ARANGE(2).GT.ARANGE(1)) THEN
            PMIN = XRANGE(1)
            PMAX = XRANGE(2)
            SPHASE = PMIN.GE.0.0
         ELSE
            IF (DMAX-DMIN.LE.PPMAX-PPMIN) THEN
               PMIN = DMIN
               PMAX = DMAX
               SPHASE = .FALSE.
            ELSE
               PMIN = PPMIN
               PMAX = PPMAX
               SPHASE = .TRUE.
               END IF
            XRANGE(1) = PMIN
            XRANGE(2) = PMAX
            END IF
         IF (PMIN.EQ.PMAX) THEN
            IF (PMIN.EQ.0.0) THEN
               PMAX = 1.
               PMIN = -1.
            ELSE
               PMAX = 1.03 * PMIN
               PMIN = 0.97 * PMIN
               END IF
            END IF
C                                       now init the plot
         DOTV = XDOTV.GT.0.0
         TVCHN = 0
         TVCORN(1) = 0
         TVCORN(2) = 0
         GRCHN = IROUND (XCHAN)
         PLUN = 26
         IVER = 0
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', INDISK, CNO, CATBLK, PBUFF, .TRUE.,
     *         'WRIT', IVER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CREATE PLOT FILE'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
C                                       time/antenna adverbs to match
C                                       one time
         CALL RFILL (8, 0.0, XTIME)
         IF (PSORT.EQ.'TA') THEN
            XTIME(1) = TTIME
            XTIME(5) = TTIME
C                                       one antenna
         ELSE
            XTIME(1) = TPLBEG
            XTIME(5) = TPLEND
            CALL RFILL (50, 0.0, XANT)
            XANT(1) = THEANT
            END IF
         CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
         ITYPE = 30
         CALL GINIT (INDISK, CNO, PFILE, 0, ITYPE, NPARMS, USER, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND, IERR)
         FUNC = 'GINIT'
         IF (IERR.NE.0) GO TO 900
         CALL BPLABL (IVER, PMIN, PMAX, NREC, TIMES, ANTS, IERR)
         FUNC = 'BPLABL'
         IF (IERR.NE.0) GO TO 900
         DY = 1000.0 / (NREC + FACTOR)
         NP = (EPOL-BPOL+1) * (EIF-BIF+1)
         DX = NP*(ECHAN-BCHAN+4)
         DX = 1000.0 / DX
         X0 = DX / 2.0
         Y0 = -DY / 2.0
         XT = MAX (X0, 17.5)
         DCOLV = 0.97 / MAX (1, NREC-1)
         IF (DO3COL.EQ.2) DCOLV = 0.97 / (PMAX - PMIN)
         COLV = 0.97
C                                       get the data part
         MREC = J2 - J1 + 1
         IF (PSORT.EQ.'TA') MREC = NREC
         IA = 0
         LINE = 0
         NRECC = 0
         NRECI = MAX (1, NREC / 20)
         DO 290 J = 1,MREC
            IBPRNO = J + J1 - 1
            IF (PSORT.EQ.'TA') THEN
 225           IA = IA + 1
               IBPRNO = ANTS(IA)
               IF (IBPRNO.LE.0) GO TO 225
               END IF
            CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *         CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (TIME.GE.TPLBEG) .AND. (TIME.LE.TPLEND) .AND.
     *         (IANTS(ANTNO).GT.0)) THEN
               NRECC = NRECC + 1
               IF (DO3COL.EQ.1) THEN
                  CALL COLOR3 (COLV, .FALSE., COL)
                  COLV = COLV - DCOLV
                  CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                  FUNC = 'G3VCOL'
                  IF (IERR.NE.0) GO TO 900
                  END IF
               Y0 = Y0 + DY
               IP = 0
               LINE = LINE + 1
               LMAX = -1.E10
               LMIN = 1.E10
               DO 235 IIF = BIF,EIF
                  I = IIF + NIFBP
                  WT1 = WEIGHT(IIF)
                  WT2 = WEIGHT(I)
                  I = (IIF - 1) * NCHNBP + BCHAN - 1
                  II = NCHNBP * NIFBP + I
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
                  DO 230 IIC = BCHAN,ECHAN
                     I = I + 1
                     II = II + 1
                     IP = IP + 1
                     D = FBLANK
                     IF ((WT1.GT.0.0) .AND. (BNDPAS(1,I).NE.FBLANK)
     *                  .AND. (BNDPAS(1,II).NE.FBLANK) .AND.
     *                  (WT2.GT.0.)) THEN
                        X = (BNDPAS(1,I) + LMUL*BNDPAS(1,II)) / 2.0
                        Y = (BNDPAS(2,I) + LMUL*BNDPAS(2,II)) / 2.0
                        IF (PTYPE(:3).EQ.'DIF') THEN
                           IF (NSUM(I).GT.0.0) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'DIFC') THEN
                                 D =  SQRT (X*X + Y*Y) - SQRT
     *                              (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                              ELSE IF (PTYPE.EQ.'DIFB') THEN
                                 IF (((X.NE.0.0) .OR. (Y.NE.0.0)) .AND.
     *                              ((BPSUM(1,I).NE.0.0) .OR.
     *                              (BPSUM(2,I).NE.0.0))) THEN
                                    D = RAD2DG * (ATAN2 (Y, X) -
     *                                 ATAN2 (BPSUM(2,I), BPSUM(1,I)))
                                    IF (D.LT.-180.0) D = D + 360.0
                                    IF (D.GT.180.0) D = D - 360.0
                                    IF ((SPHASE) .AND. (D.LT.0.0))
     *                                 D = D + 360.0
                                    END IF
                              ELSE
                                 X = X - BPSUM(1,I)
                                 Y = Y - BPSUM(2,I)
                                 IF (PTYPE.EQ.'DIFA') THEN
                                    D =  SQRT (X*X + Y*Y)
                                 ELSE IF (PTYPE.EQ.'DIFR') THEN
                                    D =  X
                                 ELSE IF (PTYPE.EQ.'DIFI') THEN
                                    D =  Y
                                 ELSE IF ((X.NE.0.0) .OR. (Y.NE.0.0))
     *                              THEN
                                    D = ATAN2 (Y, X) * RAD2DG
                                    IF ((SPHASE) .AND. (D.LT.0.0))
     *                                 D = D + 360.0
                                    END IF
                                 END IF
                              END IF
                        ELSE
                           D = 0.0
                           IF (PTYPE.EQ.'AMP') THEN
                              D =  SQRT (X*X + Y*Y)
                           ELSE IF (PTYPE.EQ.'REAL') THEN
                              D =  X
                           ELSE IF (PTYPE.EQ.'IMAG') THEN
                              D =  Y
                           ELSE IF ((X.NE.0.0) .OR. (Y.NE.0.0)) THEN
                              D = ATAN2 (Y, X) * RAD2DG
                              IF ((SPHASE) .AND. (D.LT.0.0))
     *                           D = D + 360.0
                              END IF
                           END IF
                        END IF
                     IF ((D.NE.FBLANK) .AND. ((D.LT.PMIN) .OR.
     *                  (D.GT.PMAX))) D = FBLANK
                     IF (D.NE.FBLANK) THEN
                        LMAX = MAX (LMAX, D)
                        LMIN = MIN (LMIN, D)
                        END IF
                     PLPTS(IP) = D
 230                 CONTINUE
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
 235              CONTINUE
               IF (ARANGE(2).GE.ARANGE(1)) THEN
                  LMAX = PMAX
                  LMIN = PMIN
                  END IF
               IF (DO3COL.EQ.2) DCOLV = 0.97 / (LMAX - LMIN)
C                                       Do the plot now
               SCALE = DY * FACTOR / (LMAX - LMIN)
C                                       Don't label if too crowded
               IF (MOD(NRECC-1,NRECI).EQ.0) THEN
                  IF ((LMAX.GE.0.0) .AND. (LMIN.LE.0.0)) THEN
                     SOFF = Y0 - LMIN * SCALE
                     IF (DO3COL.GE.2) COLV = LMAX * DCOLV
                  ELSE
                     SOFF = Y0 + 0.5 * DY * FACTOR
                     IF (DO3COL.GE.2) COLV = 0.5
                     END IF
                  CALL GLTYPE (1, PBUFF, IERR)
                  FUNC = 'GLTYPE'
                  IF (IERR.NE.0) GO TO 900
                  IF (DO3COL.GE.2) THEN
                     CALL COLOR3 (COLV, .FALSE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                     FUNC = 'G3VCOL'
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  FUNC = 'GPOS/VECs'
                  X = 0.0
                  DO 245 LP = 2,NP
                     X = X + (ECHAN-BCHAN+4) * DX
                     CALL GPOS (X+XT, SOFF, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X-XT, SOFF, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X-XT, SOFF, PBUFF, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 900
 245                 CONTINUE
                  CALL GPOS (1000.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (1000.0-XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GPOS (XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (0.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  FUNC = 'GCHAR'
                  IF ((LTYPE.EQ.3) .AND. (PSORT.NE.'TA')) THEN
                     Y = TIME
                     CALL TODHMS (Y, ITT)
                     WRITE (STRING,1239) ITT
                     Y = -10.5
                     CALL GCHAR (10, 0, Y, -0.5, STRING(:10), PBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 900
                  ELSE IF (LTYPE.GT.2) THEN
                     IL = LINE
                     IF (PSORT.EQ.'TA') IL = IA
                     WRITE (CLABEL,1240) IL
                     I = 4
                     IF (IL.GT.9) I = 3
                     IF (IL.GT.99) I = 2
                     IF (IL.GT.999) I = 1
                     Y = -(5-I) - 0.5
                     CALL GCHAR (5-I, 0, Y, -0.5, CLABEL(I:4), PBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  END IF
               CALL GLTYPE (2, PBUFF, IERR)
               FUNC = 'GLTYPE'
               IF (IERR.NE.0) GO TO 900
               LBLANK = .TRUE.
               X = X0
               DO 250 I = 1,IP
                  IF (PLPTS(I).EQ.FBLANK) THEN
                     LBLANK = .TRUE.
                     X = X + DX
                  ELSE
                     Y = Y0 + (PLPTS(I)-LMIN) * SCALE
                     IF ((LBLANK) .OR. (Y.LT.XPLPTS(I))) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                        IF (DO3COL.GE.2) THEN
                           YP = Y
                           COLV = (LMAX-PLPTS(I)) * DCOLV
                           CALL COLOR3 (COLV, .FALSE., COL)
                           CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                        IERR)
                           FUNC = 'G3VCOL'
                           IF (IERR.NE.0) GO TO 900
                           END IF
                     ELSE IF (DO3COL.GE.2) THEN
                        YH = (YP + Y) / 2.0
                        CALL G3VEC (X, YH, PBUFF, IERR)
                        YP = Y
                        COLV = (LMAX-PLPTS(I)) * DCOLV
                        CALL COLOR3 (COLV, .FALSE., COL)
                        CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                     IERR)
                        FUNC = 'G3VCOL'
                        IF (IERR.NE.0) GO TO 900
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.EQ.1) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        END IF
                     FUNC = 'GVEC'
                     IF (IERR.NE.0) GO TO 900
                     LBLANK = .FALSE.
                     X = X + DX
                     IF (Y.LT.XPLPTS(I)) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                        END IF
                     IF (IERR.NE.0) GO TO 900
                     END IF
 250              CONTINUE
               END IF
 290        CONTINUE
C                                       finish plot and continue?
         IF (PSORT.NE.'TA') NIANTS = NIANTS - 1
         GPHPAG = (DOTV) .AND. (J2.LT.NBPINR) .AND. (NIANTS.GT.0)
         CALL GFINIS (PBUFF, IERR)
C                                       Successful plot file finished.
         IF (IERR.EQ.0) THEN
            NPLOT = NPLOT + 1
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (INDISK, CNO, IVER, PBUFF, IERR)
               WRITE (MSGTXT,1290) IVER
               CALL MSGWRT (5)
               IERR = 0
               END IF
            END IF
C                                       loop
         IF ((J2.LT.NBPINR) .AND. (IERR.EQ.0)) GO TO 10
 300  IERR = MAX (0, IERR)
C                                       close
      CALL TABBP ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO, CHNBND, BPFREQ,
     *   FREQID, BPREF, WEIGHT, BNDPAS, JERR)
      GO TO 999
C                                       plotter error
 900  WRITE (MSGTXT,1900) IERR, FUNC
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO SAVE PARTIAL GRAPH'
      CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPLOTS: ERROR',I5,' ON ',A)
 1239 FORMAT (I1,'/',2(I2.2,':'),I2.2)
 1240 FORMAT (I4)
 1290 FORMAT ('Successful plot file version',I5,' written')
 1900 FORMAT ('PLOTTING ERROR',I5,' AT ',A)
      END
      SUBROUTINE BPLABL (IVER, PMIN, PMAX, NREC, TIMES, ANTS, IERR)
C-----------------------------------------------------------------------
C   Do the init for line drawing, ...
C   Inputs:
C      IVER    I      File version number
C      PMIN    R      Min value to plot
C      PMAX    R      Max value to plot
C      NREC    I      Number of rows
C      TIMES   R(*)   List of times of sample
C      ANTS    I(*)   List of sampled antennas
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      REAL      PMIN, PMAX, TIMES(*)
      INTEGER   IVER, NREC, ANTS(*), IERR
C
      REAL      BLC(7), CH(4), TRC(7), X, DX, DY, DCX, DCY, XMIN, YMIN
      INTEGER   IDEPTH(5), I, IIP, IIF, XINTER(10), NP, XINT, XLO, XHI,
     *   TIME1(4), TIME2(4), INCHAR
      CHARACTER STRING*80, NAMSTR*20, CTIME*8, CDATE*12
      INCLUDE 'BPLOT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA XINTER /2,5,10,20,50,100,200,500,1000,2000/
C-----------------------------------------------------------------------
C                                       number characters around
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.EQ.0) LTYPE = 3
      IF (LABEL.GE.0) THEN
         LABEL = (LABEL/100)*100 + LTYPE
      ELSE
         LABEL = (LABEL/100)*100 - LTYPE
         END IF
      CALL RFILL (4, 0.5, CH)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (TTYPE.EQ.'BP') THEN
         IF (LTYPE.GT.2) CH(1) = ALOG10 (REAL(NREC)) + 4.5
         IF ((LTYPE.EQ.3) .AND. (PSORT.NE.'TA')) CH(1) = 13.5
      ELSE
         IF (LTYPE.GT.2) CH(1) = 6.5
         END IF
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         IF (TTYPE.EQ.'BP') THEN
            CH(2) = CH(2) + 2 * 1.333
         ELSE
            CH(2) = CH(2) + 1.333
            END IF
         END IF
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(2) = 0.0
      BLC(1) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (XDOTV.GT.0.0) THEN
            XMIN = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CH(1)+CH(3))
            YMIN = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CH(2)+CH(4))
            XYRATO = 1.0
            IF (YMIN.GT.0.0) XYRATO = XMIN / YMIN
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (1, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       separate polarizations/IFs
      NP = (EPOL-BPOL+1) * (EIF-BIF+1)
      I = NP * (ECHAN-BCHAN+4)
      DX = 1000.0 / I
      IF (NP.GT.1) THEN
         X = 0.0
         DO 20 IIP = 2,NP
            X = X + (ECHAN-BCHAN+4) * DX
            CALL GPOS (X, 0.0, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (X, 1000.0, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
         END IF
      IF (LTYPE.LE.1) GO TO 999
C                                       Horizontal ticks
      DCY = -0.17
      IF (LTYPE.EQ.2) GO TO 100
      IIP = 9
      IF (NP.GT.1) IIP = 8 - NP
      DO 30 I = 1,10
         IIF = (ECHAN-BCHAN+1) / XINTER(I)
         IF (IIF.LE.IIP) GO TO 35
 30      CONTINUE
      GO TO 100
C                                       tick drawing
 35   XINT = XINTER(I)
      XLO = (BCHAN / XINT) * XINT
      IF (XLO.LT.BCHAN) XLO = XLO + XINT
      XHI = (ECHAN / XINT) * XINT
      DY = 25.
      DCY = -1.5
      DO 60 IIP = 1,NP
         DO 50 I = XLO,XHI,XINT
            X = DX * (2.0 + I - BCHAN) + (IIP-1) * (ECHAN-BCHAN+4) * DX
            WRITE (STRING,1035) I
            CALL CHTRIM (STRING, 4, STRING, IIF)
            DCX = -IIF + 0.5
            CALL GPOS (X, 1000.0, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (X, 1000.0-DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GPOS (X, DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (X, 0.0, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GCHAR (IIF, 0, DCX, DCY, STRING(:IIF), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
 50         CONTINUE
 60      CONTINUE
C                                       IF, POL labels
      X = DX
      DCY = -1.5
      DO 70 IIP = BPOL,EPOL
         DO 65 IIF = BIF,EIF
            CALL GPOS (X, TRC(2)-DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (STRING,1060) IIF, IIP
            DCX = 1.5
            CALL GICHAR (1, 4, 0, DCX, DCY, STRING(:4), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            X = X + (ECHAN-BCHAN+4) * DX
            CALL GPOS (X, TRC(2)-DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            DCX = -5.5
            CALL GICHAR (1, 4, 0, DCX, DCY, STRING(5:8), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
 65         CONTINUE
 70      CONTINUE
C                                       horizontal axis label
      DCY = DCY - 1.33
      CALL GPOS (500., 0., PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = -8.5
      CALL GCHAR (17, 0, DCX, DCY, 'SPECTRAL CHANNELS', PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       vertical axis label
      CALL GPOS (0.0, 500., PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = -CH(1) + 1.0
      IF (TTYPE.EQ.'BP') THEN
         IF (PSORT.EQ.'TA') THEN
            CALL GCHAR (14, 1, DCX, 6.0, 'ANTENNA NUMBER', PBUFF, IERR)
         ELSE IF (LTYPE.EQ.3) THEN
            CALL GCHAR (4, 1, DCX, 4.5, 'TIME', PBUFF, IERR)
         ELSE
            CALL GCHAR (11, 1, DCX, 4.5, 'TIME NUMBER', PBUFF, IERR)
            END IF
      ELSE IF (TTYPE.EQ.'BD') THEN
         CALL GCHAR (14, 1, DCX, 4.5, 'SECOND ANTENNA', PBUFF, IERR)
      ELSE IF (TTYPE.EQ.'PD') THEN
         CALL GCHAR (7, 1, DCX, 4.5, 'ANTENNA', PBUFF, IERR)
      ELSE IF (TTYPE.EQ.'CP') THEN
         CALL GCHAR (13, 1, DCX, 4.5, 'SOURCE NUMBER', PBUFF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       more labels
 100  IF (LTYPE.GE.7) GO TO 999
      CDATE = ' '
      CALL H2CHR (4, 1, XSTOKE, CDATE)
      IF (CDATE.EQ.'R+L') THEN
         CDATE = '(R+L)/2'
      ELSE IF (CDATE.EQ.'R-L') THEN
         CDATE = '(R-L)/2'
      ELSE IF (CDATE.EQ.'RL') THEN
         CDATE = 'R and L'
         END IF
C                                       flux range
      IF ((PTYPE.EQ.'AMP ') .OR. (PTYPE.EQ.'DIFA') .OR.
     *   (PTYPE.EQ.'DIFC') .OR. (PTYPE.EQ.'REAL') .OR.
     *   (PTYPE.EQ.'IMAG') .OR. (PTYPE.EQ.'DIFR') .OR.
     *   (PTYPE.EQ.'DIFI')) THEN
         IF (PTYPE.EQ.'DIFC') PTYPE = 'ADIF'
         WRITE (STRING,1100) PTYPE, PMIN, PMAX, BIF, EIF, CDATE
         IF (PTYPE.EQ.'ADIF') PTYPE = 'DIFC'
      ELSE
         IF (PTYPE.EQ.'DIFB') PTYPE = 'PDIF'
         WRITE (STRING,1101) PTYPE, PMIN, PMAX, BIF, EIF, CDATE
         IF (PTYPE.EQ.'PDIF') PTYPE = 'DIFB'
         END IF
      IF (ARANGE(2).LT.ARANGE(1)) STRING(12:29) = 'self-scaled'
      DCY = DCY - 1.33
      DCX = 0.0
      CALL GPOS (0.0, 0.0, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL REFRMT (STRING, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, STRING(:26), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       time/antenna range
      IF (TTYPE.EQ.'BP') THEN
         DCY = DCY - 1.33
         CALL GPOS (0.0, 0.0, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (PSORT.EQ.'TA') THEN
            IIP = 10000
            IIF = 0
            DO 110 I = 1,MAXANT
               IF (ANTS(I).GT.0) THEN
                  IIP = MIN (IIP, I)
                  IIF = MAX (IIF, I)
                  END IF
 110           CONTINUE
            WRITE (STRING,1110) IIP, IIF
         ELSE
            CALL TODHMS (TIMES(1), TIME1)
            CALL TODHMS (TIMES(NREC), TIME2)
            WRITE (STRING,1111) TIME1, TIME2
            END IF
         CALL REFRMT (STRING, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, STRING(:INCHAR), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       image name etc
      CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DCY = 0.5
      DCX = 0.0
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), STRING, IIF)
      STRING(IIF+1:) = ' '
      IIF = IIF + 4
      IF (TTYPE.EQ.'BP') THEN
         IF (PSORT.EQ.'TA') THEN
            X = (TIMES(1) + TIMES(NREC)) / 2.0
            CALL TODHMS (X, TIME1)
            WRITE (STRING(IIF:),1120) TTYPE, INVERS, TIME1
         ELSE
            IIP = 0
            DO 120 I = 1,MAXANT
               IF (ANTS(I).GT.0) IIP = I
 120           CONTINUE
            WRITE (STRING(IIF:),1121) TTYPE, INVERS, IIP
            END IF
      ELSE
         IIP = XANT(1) + 0.1
         WRITE (STRING(IIF:),1122) TTYPE, INVERS, IIP
         END IF
      CALL REFRMT (STRING, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, STRING(:INCHAR), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZDATE (TIME1)
         CALL ZTIME (TIME2)
         CALL TIMDAT (TIME2, TIME1, CTIME, CDATE)
         WRITE (STRING,1130) IVER, CDATE, CTIME
         CALL REFRMT (STRING, '_', INCHAR)
         IIF = 51
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, STRING(:INCHAR), PBUFF, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1035 FORMAT (I4)
 1060 FORMAT ('IF',I2.2,'POL',I1)
 1100 FORMAT (A4,' RANGE =',F8.4,'_',F8.4,' __IFs',2I3,' __Stokes ',A)
 1101 FORMAT (A4,' RANGE =',F8.2,'_',F8.2,' __IFs',2I3,' __Stokes ',A)
 1110 FORMAT ('ANTE RANGE =',2I3.2)
 1111 FORMAT ('TIME RANGE =',2(I2,'/',I2.2,':',I2.2,':',I2.2,1X))
 1120 FORMAT (A2,'VER',I4,' _TIME',I3,'/',2(I2.2,':'),I2.2)
 1121 FORMAT (A2,'VER',I4,' _ANTENNA',I3)
 1122 FORMAT (A2,'VER',I4,' _FIRST_ANTENNA',I3)
 1130 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE BDLOTT (IERR)
C-----------------------------------------------------------------------
C   BDLOTT uses the open BD table to generate the plots.
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER PFILE*48, CLABEL*4, FUNC*12
      INTEGER   I, J, J1, J2, NREC, SOURID, SUBA, ANT1, FREQID, IP, IA,
     *   JERR, ANTS(MAXANT), THEANT, IIF, IIC, IIP, MREC, LINE, TVCHN,
     *   GRCHN, TVCORN(2), IVER, IROUND, NP, LP, ITYPE, NPLOT, NRECI,
     *   NRECC, ANT2, OTHANT, KREC
      REAL      D, DMAX, DMIN, BPSUM(3,MAXCIF), WT, DX, DY, X0, Y0,
     *   SCALE, BNDPAS(2,MAXCIF), NSUM(MAXCIF), X, Y, SOFF, RTIME, PSGN,
     *   PLPTS(MAXCIF+100), XPLPTS(MAXCIF+100), PMAX, PMIN, XT, COL(3),
     *   COLV, DCOLV, YP, YH, PPMAX, PPMIN, LMAX, LMIN
      LOGICAL   DOTV, LBLANK, SPHASE
      DOUBLE PRECISION TTIME, DTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DTIME = 5. / 60.0 / 24.0
      THEANT = 0
      NPLOT = 0
C                                       loop for next plot
 10   THEANT = THEANT + 1
      NREC = 0
      MREC = 0
      KREC = 0
      IF (IANTS(THEANT).GT.0) THEN
         NIANTS = NIANTS - 1
C                                       find range of desired data
         DO 90 J = 1,NBPINR
            IBPRNO = J
            CALL TABBD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, RTIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *         BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BD TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (RTIME.GE.TIMBEG) .AND. (RTIME.LE.TIMEND) .AND.
     *         (IANTS(ANT1).GT.0) .AND. (IANTS(ANT2).GT.0) .AND.
     *         ((ANT1.EQ.THEANT) .OR. (ANT2.EQ.THEANT))) THEN
               IF (NREC.EQ.0) THEN
                  KREC = 0
                  CALL FILL (MAXANT, 0, ANTS)
                  TTIME = RTIME
                  DMAX = -1.E10
                  DMIN = -DMAX
                  PPMAX = DMAX
                  PPMIN = DMIN
                  IF (PTYPE(:3).EQ.'DIF') THEN
                     I = NPOLBP * NCHNBP * NIFBP
                     CALL RFILL (I, 0.0, NSUM)
                     I = 3 * I
                     CALL RFILL (I, 0.0, BPSUM)
                     END IF
                  END IF
C                                       check for valid data
               DO 15 IIP = BPOL,EPOL
                  DO 14 IIF = BIF,EIF
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     DO 13 IIC = BCHAN,ECHAN
                        I = I + 1
                        IF (BNDPAS(1,I).NE.FBLANK) GO TO 18
 13                     CONTINUE
 14                  CONTINUE
 15               CONTINUE
               J2 = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  KREC = KREC + 1
                  END IF
               GO TO 90
 18            MREC = MREC + 1
               IF (ANT1.EQ.THEANT) THEN
                  PSGN = +1.0
                  ANTS(ANT2) = J
               ELSE
                  PSGN = -1.0
                  ANTS(ANT1) = J
                  END IF
               J2 = J
               IF ((PTYPE(:3).NE.'DIF') .OR. ((TIME.GE.TPLBEG) .AND.
     *            (TIME.LE.TPLEND))) THEN
                  NREC = NREC + 1
                  KREC = KREC + 1
                  END IF
C                                       average spectrum
               IF (PTYPE(:3).EQ.'DIF') THEN
                  DO 30 IIP = BPOL,EPOL
                     DO 25 IIF = BIF,EIF
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        DO 20 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF (BNDPAS(1,I).NE.FBLANK) THEN
                              BPSUM(1,I) = BPSUM(1,I) + BNDPAS(1,I)
                              BPSUM(2,I) = BPSUM(2,I) + PSGN*BNDPAS(2,I)
                              BPSUM(3,I) = BPSUM(3,I) + SQRT
     *                           (BNDPAS(1,I)**2 + BNDPAS(2,I)**2)
                              NSUM(I) = NSUM(I) + 1.0
                              END IF
 20                        CONTINUE
 25                     CONTINUE
 30                  CONTINUE
C                                       or find max/min
               ELSE IF (ARANGE(2).LE.ARANGE(1)) THEN
                  DO 50 IIP = BPOL,EPOL
                     DO 45 IIF = BIF,EIF
                        I = IIF + (IIP - 1) * NIFBP
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        DO 40 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF (BNDPAS(1,I).NE.FBLANK) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'AMP ') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2)
                              ELSE IF (PTYPE.EQ.'REAL') THEN
                                 D =  BNDPAS(1,I)
                              ELSE IF (PTYPE.EQ.'IMAG') THEN
                                 D =  BNDPAS(2,I)
                              ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                           (BNDPAS(2,I).NE.0.0)) THEN
                                 D = ATAN2 (PSGN*BNDPAS(2,I),
     *                              BNDPAS(1,I)) * RAD2DG
                                 END IF
                              DMAX = MAX (D, DMAX)
                              DMIN = MIN (D, DMIN)
                              IF (D.LT.0.0) D = D + 360.0
                              PPMAX = MAX (D, PPMAX)
                              PPMIN = MIN (D, PPMIN)
                              END IF
 40                        CONTINUE
 45                     CONTINUE
 50                  CONTINUE
                  END IF
               END IF
 90         CONTINUE
         END IF
C                                       Found data
      IF ((MREC.GT.0) .AND. (NREC.GT.0)) THEN
         NREC = KREC
C                                       average spectrum
         IF (PTYPE(:3).EQ.'DIF') THEN
            DO 120 IIP = BPOL,EPOL
               DO 115 IIF = BIF,EIF
                  I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) * NCHNBP +
     *               BCHAN - 1
                  DO 110 IIC = BCHAN,ECHAN
                     I = I + 1
                     WT = NSUM(I)
                     IF (WT.GT.0.0) THEN
                        BPSUM(3,I) = BPSUM(3,I) / WT
                        WT = SQRT (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                        IF (WT.GT.0.0) WT = BPSUM(3,I) / WT
                        BPSUM(1,I) = BPSUM(1,I) * WT
                        BPSUM(2,I) = BPSUM(2,I) * WT
                        END IF
 110                 CONTINUE
 115              CONTINUE
 120           CONTINUE
            END IF
C                                       now get scale
         IF ((PTYPE(:3).EQ.'DIF') .AND. (ARANGE(2).LE.ARANGE(1))) THEN
            DO 150 J = 1,NANTBP
               IBPRNO = ANTS(J)
               IF (IBPRNO.GT.0) THEN
                  CALL TABBD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *               NIFBP, NCHNBP, NPOLBP, RTIME, SOURID, SUBA, ANT1,
     *               ANT2, FREQID, BNDPAS, IERR)
                  IF (IERR.GT.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'READ BD TABLE'
                     GO TO 990
                     END IF
                  DO 140 IIP = BPOL,EPOL
                     DO 135 IIF = BIF,EIF
                        I = IIF + (IIP - 1) * NIFBP
                        I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                     NCHNBP + BCHAN - 1
                        IF (ANT1.EQ.THEANT) THEN
                           PSGN = +1.0
                        ELSE
                           PSGN = -1.0
                           END IF
                        DO 130 IIC = BCHAN,ECHAN
                           I = I + 1
                           IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                        (NSUM(I).GT.0.0)) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'DIFC') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2) - SQRT
     *                              (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                              ELSE IF (PTYPE.EQ.'DIFB') THEN
                                 IF (((BNDPAS(1,I).NE.0.0) .OR.
     *                              (BNDPAS(2,I).NE.0.0)) .AND.
     *                              ((BPSUM(1,I).NE.0.0) .OR.
     *                              (BPSUM(2,I).NE.0.0))) THEN
                                    D =(ATAN2 (PSGN*BNDPAS(2,I),
     *                                 BNDPAS(1,I)) - ATAN2 (BPSUM(2,I),
     *                                 BPSUM(1,I))) * RAD2DG
                                    IF (D.LT.-180.0) D = D + 360.0
                                    IF (D.GT.180.0) D = D - 360.0
                                    END IF
                              ELSE
                                 BNDPAS(1,I) = BNDPAS(1,I) - BPSUM(1,I)
                                 BNDPAS(2,I) = PSGN*BNDPAS(2,I) -
     *                              BPSUM(2,I)
                                 IF (PTYPE.EQ.'DIFA') THEN
                                    D =  SQRT (BNDPAS(1,I)**2 +
     *                                 BNDPAS(2,I)**2)
                                 ELSE IF (PTYPE.EQ.'DIFR') THEN
                                    D =  BNDPAS(1,I)
                                 ELSE IF (PTYPE.EQ.'DIFI') THEN
                                    D =  BNDPAS(2,I)
                                 ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                              (BNDPAS(2,I).NE.0.0)) THEN
                                    D = ATAN2 (PSGN*BNDPAS(2,I),
     *                                 BNDPAS(1,I)) * RAD2DG
                                    END IF
                                 END IF
                              DMAX = MAX (D, DMAX)
                              DMIN = MIN (D, DMIN)
                              IF (D.LT.0.0) D = D + 360.0
                              PPMAX = MAX (D, PPMAX)
                              PPMIN = MIN (D, PPMIN)
                              END IF
 130                       CONTINUE
 135                    CONTINUE
 140                 CONTINUE
                  END IF
 150           CONTINUE
            END IF
C                                       set scale, blank array
         I = MAXCIF + 100
         CALL RFILL (I, 0.0, XPLPTS)
         IF (ARANGE(2).GT.ARANGE(1)) THEN
            PMIN = XRANGE(1)
            PMAX = XRANGE(2)
            SPHASE = PMIN.GE.0.0
         ELSE
            IF (DMAX-DMIN.LE.PPMAX-PPMIN) THEN
               PMIN = DMIN
               PMAX = DMAX
               SPHASE = .FALSE.
            ELSE
               PMIN = PPMIN
               PMAX = PPMAX
               SPHASE = .TRUE.
               END IF
            XRANGE(1) = PMIN
            XRANGE(2) = PMAX
            END IF
         IF (PMIN.EQ.PMAX) THEN
            IF (PMIN.EQ.0.0) THEN
               PMAX = 1.
               PMIN = -1.
            ELSE
               PMAX = 1.03 * PMIN
               PMIN = 0.97 * PMIN
               END IF
            END IF
C                                       now init the plot
         DOTV = XDOTV.GT.0.0
         TVCHN = 0
         TVCORN(1) = 0
         TVCORN(2) = 0
         GRCHN = IROUND (XCHAN)
         PLUN = 26
         IVER = 0
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', INDISK, CNO, CATBLK, PBUFF, .TRUE.,
     *         'WRIT', IVER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CREATE PLOT FILE'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
C                                       time/antenna adverbs to match
C                                       one time
         CALL RFILL (8, 0.0, XTIME)
C                                       one antenna
         XTIME(1) = TPLBEG
         XTIME(5) = TPLEND
         CALL RFILL (50, 0.0, XANT)
         XANT(1) = THEANT
         CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
         ITYPE = 30
         CALL GINIT (INDISK, CNO, PFILE, 0, ITYPE, NPARMS, USER, DOTV,
     *      TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND, IERR)
         FUNC = 'GINIT'
         IF (IERR.NE.0) GO TO 900
C                                       note COL not used here
         CALL BPLABL (IVER, PMIN, PMAX, NREC, COL, ANTS, IERR)
         FUNC = 'BPLABL'
         IF (IERR.NE.0) GO TO 900
         DY = 1000.0 / (NREC + FACTOR)
         NP = (EPOL-BPOL+1) * (EIF-BIF+1)
         DX = NP*(ECHAN-BCHAN+4)
         DX = 1000.0 / DX
         X0 = DX / 2.0
         Y0 = -DY / 2.0
         XT = MAX (X0, 17.5)
         DCOLV = 0.97 / MAX (1, NREC-1)
         IF (DO3COL.EQ.2) DCOLV = 0.97 / (PMAX - PMIN)
         COLV = 0.97
C                                       get the data part
         MREC = J2 - J1 + 1
         IF (PSORT.EQ.'TA') MREC = NREC
         IA = 0
         LINE = 0
         NRECC = 0
         NRECI = MAX (1, NREC / 20)
         DO 290 J = 1,NANTBP
            IBPRNO = ANTS(J)
            IF (IBPRNO.GT.0) THEN
               CALL TABBD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *            NIFBP, NCHNBP, NPOLBP, RTIME, SOURID, SUBA, ANT1,
     *            ANT2, FREQID, BNDPAS, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ BD TABLE'
                  GO TO 990
                  END IF
               NRECC = NRECC + 1
               IF (DO3COL.EQ.1) THEN
                  CALL COLOR3 (COLV, .FALSE., COL)
                  COLV = COLV - DCOLV
                  CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                  FUNC = 'G3VCOL'
                  IF (IERR.NE.0) GO TO 900
                  END IF
               IF (ANT1.EQ.THEANT) THEN
                  OTHANT = ANT2
                  PSGN = +1.0
               ELSE
                  OTHANT = ANT1
                  PSGN = -1.0
                  END IF
               Y0 = Y0 + DY
               IP = 0
               LINE = LINE + 1
               LMAX = -1.E10
               LMIN = 1.E10
               DO 240 IIP = BPOL,EPOL
                  DO 235 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
                     DO 230 IIC = BCHAN,ECHAN
                        I = I + 1
                        IP = IP + 1
                        D = FBLANK
                        IF ((BNDPAS(1,I).NE.FBLANK) .AND.
     *                     (BNDPAS(1,I).NE.0.0)) THEN
                           IF (PTYPE(:3).EQ.'DIF') THEN
                              IF (NSUM(I).GT.0.0) THEN
                                 D = 0.0
                                 IF (PTYPE.EQ.'DIFC') THEN
                                    D =  SQRT (BNDPAS(1,I)**2 +
     *                                 BNDPAS(2,I)**2) - SQRT
     *                                 (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                                 ELSE IF (PTYPE.EQ.'DIFB') THEN
                                    IF (((BNDPAS(1,I).NE.0.0) .OR.
     *                                 (BNDPAS(2,I).NE.0.0)) .AND.
     *                                 ((BPSUM(1,I).NE.0.0) .OR.
     *                                 (BPSUM(2,I).NE.0.0))) THEN
                                       D = (ATAN2 (PSGN*BNDPAS(2,I),
     *                                    BNDPAS(1,I)) - ATAN2
     *                                    (BPSUM(2,I), BPSUM(1,I)))
     *                                    * RAD2DG
                                       IF (D.LT.-180.0) D = D + 360.0
                                       IF (D.GT.180.0) D = D - 360.0
                                       IF ((SPHASE) .AND. (D.LT.0.0))
     *                                    D = D + 360.0
                                       END IF
                                 ELSE
                                    BNDPAS(1,I) = BNDPAS(1,I)-BPSUM(1,I)
                                    BNDPAS(2,I) = PSGN * BNDPAS(2,I)
     *                                 - BPSUM(2,I)
                                    IF (PTYPE.EQ.'DIFA') THEN
                                       D =  SQRT (BNDPAS(1,I)**2 +
     *                                    BNDPAS(2,I)**2)
                                    ELSE IF (PTYPE.EQ.'DIFR') THEN
                                       D =  BNDPAS(1,I)
                                    ELSE IF (PTYPE.EQ.'DIFI') THEN
                                       D =  BNDPAS(2,I)
                                    ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                                 (BNDPAS(2,I).NE.0.0)) THEN
                                       D = ATAN2 (PSGN * BNDPAS(2,I),
     *                                    BNDPAS(1,I)) * RAD2DG
                                       IF ((SPHASE) .AND. (D.LT.0.0))
     *                                    D = D + 360.0
                                       END IF
                                    END IF
                                 END IF
                           ELSE
                              D = 0.0
                              IF (PTYPE.EQ.'AMP') THEN
                                 D =  SQRT (BNDPAS(1,I)**2 +
     *                              BNDPAS(2,I)**2)
                              ELSE IF (PTYPE.EQ.'REAL') THEN
                                 D =  BNDPAS(1,I)
                              ELSE IF (PTYPE.EQ.'IMAG') THEN
                                 D =  BNDPAS(2,I)
                              ELSE IF ((BNDPAS(1,I).NE.0.0) .OR.
     *                           (BNDPAS(2,I).NE.0.0)) THEN
                                 D = ATAN2 (PSGN*BNDPAS(2,I),
     *                              BNDPAS(1,I)) * RAD2DG
                                 IF ((SPHASE) .AND. (D.LT.0.0))
     *                              D = D + 360.0
                                 END IF
                              END IF
                           END IF
                        IF ((D.NE.FBLANK) .AND. ((D.LT.PMIN) .OR.
     *                     (D.GT.PMAX))) D = FBLANK
                        IF (D.NE.FBLANK) THEN
                           LMAX = MAX (LMAX, D)
                           LMIN = MIN (LMIN, D)
                           END IF
                        PLPTS(IP) = D
 230                    CONTINUE
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
                     IP = IP + 1
                     PLPTS(IP) = FBLANK
 235                 CONTINUE
 240              CONTINUE
               IF (ARANGE(2).GE.ARANGE(1)) THEN
                  LMAX = PMAX
                  LMIN = PMIN
                  END IF
               IF (DO3COL.EQ.2) DCOLV = 0.97 / (LMAX - LMIN)
C                                       Do the plot now
               SCALE = DY * FACTOR / (LMAX - LMIN)
C                                       Don't label if too crowded
               IF (MOD(NRECC-1,NRECI).EQ.0) THEN
                  IF ((LMAX.GE.0.0) .AND. (LMIN.LE.0.0)) THEN
                     SOFF = Y0 - LMIN * SCALE
                     IF (DO3COL.GE.2) COLV = LMAX * DCOLV
                  ELSE
                     SOFF = Y0 + 0.5 * DY * FACTOR
                     IF (DO3COL.GE.2) COLV = 0.5
                     END IF
                  CALL GLTYPE (1, PBUFF, IERR)
                  FUNC = 'GLTYPE'
                  IF (IERR.NE.0) GO TO 900
                  IF (DO3COL.GE.2) THEN
                     CALL COLOR3 (COLV, .FALSE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                     FUNC = 'G3VCOL'
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  FUNC = 'GPOS/VECs'
                  X = 0.0
                  DO 245 LP = 2,NP
                     X = X + (ECHAN-BCHAN+4) * DX
                     CALL GPOS (X+XT, SOFF, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X-XT, SOFF, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X-XT, SOFF, PBUFF, IERR)
                        END IF
                     IF (IERR.NE.0) GO TO 900
 245                 CONTINUE
                  CALL GPOS (1000.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (1000.0-XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GPOS (XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (0.0, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  IF (LTYPE.GT.2) THEN
                     FUNC = 'GCHAR'
                     WRITE (CLABEL,1240) OTHANT
                     I = 1
                     IF (OTHANT.LE.9) I = 2
                     Y = -(3-I) - 0.5
                     CALL GCHAR (3-I, 0, Y, -0.5, CLABEL(I:4), PBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  END IF
               CALL GLTYPE (2, PBUFF, IERR)
               FUNC = 'GLTYPE'
               IF (IERR.NE.0) GO TO 900
               LBLANK = .TRUE.
               X = X0
               DO 250 I = 1,IP
                  IF (PLPTS(I).EQ.FBLANK) THEN
                     LBLANK = .TRUE.
                     X = X + DX
                  ELSE
                     Y = Y0 + (PLPTS(I)-LMIN) * SCALE
                     IF ((LBLANK) .OR. (Y.LT.XPLPTS(I))) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                        IF (DO3COL.GE.2) THEN
                           YP = Y
                           COLV = (LMAX-PLPTS(I)) * DCOLV
                           CALL COLOR3 (COLV, .FALSE., COL)
                           CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                        IERR)
                           FUNC = 'G3VCOL'
                           IF (IERR.NE.0) GO TO 900
                           END IF
                     ELSE IF (DO3COL.GE.2) THEN
                        YH = (YP + Y) / 2.0
                        CALL G3VEC (X, YH, PBUFF, IERR)
                        YP = Y
                        COLV = (LMAX-PLPTS(I)) * DCOLV
                        CALL COLOR3 (COLV, .FALSE., COL)
                        CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                     IERR)
                        FUNC = 'G3VCOL'
                        IF (IERR.NE.0) GO TO 900
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.EQ.1) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        END IF
                     FUNC = 'GVEC'
                     IF (IERR.NE.0) GO TO 900
                     LBLANK = .FALSE.
                     X = X + DX
                     IF (Y.LT.XPLPTS(I)) THEN
                        CALL GPOS (X, Y, PBUFF, IERR)
                     ELSE IF (DO3COL.GT.0.0) THEN
                        CALL G3VEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IERR)
                        XPLPTS(I) = Y
                        END IF
                     IF (IERR.NE.0) GO TO 900
                     END IF
 250              CONTINUE
               END IF
 290        CONTINUE
C                                       finish plot and continue?
         GPHPAG = (DOTV) .AND. (J2.LT.NBPINR) .AND. (NIANTS.GT.0)
         CALL GFINIS (PBUFF, IERR)
C                                       Successful plot file finished.
         IF (IERR.EQ.0) THEN
            NPLOT = NPLOT + 1
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (INDISK, CNO, IVER, PBUFF, IERR)
               WRITE (MSGTXT,1290) IVER
               CALL MSGWRT (5)
               IERR = 0
               END IF
            END IF
         END IF
C                                       loop
      IF ((THEANT.LT.NANTBP) .AND. (IERR.EQ.0)) GO TO 10
      IF (NPLOT.GT.0) THEN
         IERR = 0
      ELSE
         IERR = 1
         MSGTXT = 'But no plots were done'
         CALL MSGWRT (7)
         END IF
      IERR = MAX (0, IERR)
C                                       close
      CALL TABBD ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, RTIME, SOURID, SUBA, ANT1, ANT2, FREQID, BNDPAS, JERR)
      GO TO 999
C                                       plotter error
 900  WRITE (MSGTXT,1900) IERR, FUNC
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO SAVE PARTIAL GRAPH'
      CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BDLOTT: ERROR',I5,' ON ',A)
 1240 FORMAT (I2)
 1290 FORMAT ('Successful plot file version',I5,' written')
 1900 FORMAT ('BDLOTT PLOTTING ERROR',I5,' AT ',A)
      END
      SUBROUTINE PDLOTT (IERR)
C-----------------------------------------------------------------------
C   PDLOTT uses the open PD table to generate the plots.
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER PFILE*48, CLABEL*4, FUNC*12
      INTEGER   I, J, NREC, SUBA, ANTNO, FREQID, IP, IA, BPREF(2),
     *   ANTS(MAXANT), IIF, IIC, IIP, MREC, TVCHN, GRCHN, TVCORN(2),
     *   IVER, IROUND, IL, NP, LP, ITYPE, NPLOT, NRECI, NRECC, KREC
      REAL      TIMES(MAXANT), WT, D, DMAX, DMIN, BPSUM(3,MAXCIF),
     *   DTERMS(2,MAXCIF), NSUM(MAXCIF), DX, DY, X0, Y0, SCALE, X, Y,
     *   SOFF, PLPTS(MAXCIF+100), XPLPTS(MAXCIF+100), PMAX, PMIN, XT,
     *   COL(3), COLV, DCOLV, YP, YH, PPMAX, PPMIN, PHDIFF(MAXCIF),
     *   LMAX, LMIN
      LOGICAL   DOTV, LBLANK, SPHASE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NPLOT = 0
      IF (LABEL.GT.2) LABEL = 6
      IF (LABEL.LT.-2) LABEL = -6
C                                       find range of desired data
      NREC = 0
      MREC = 0
      KREC = 0
      DO 90 J = 1,NBPINR
         IBPRNO = J
         CALL TABPD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, ANTNO, SUBA, FREQID, BPREF, PHDIFF,
     *      DTERMS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ PD TABLE'
            GO TO 990
            END IF
         IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *      ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *      (IANTS(ANTNO).GT.0)) THEN
            IF (NREC.EQ.0) THEN
               CALL FILL (MAXANT, 0, ANTS)
               KREC = 0
               DMAX = -1.E10
               DMIN = -DMAX
               PPMAX = DMAX
               PPMIN = DMIN
               IF (PTYPE(:3).EQ.'DIF') THEN
                  I = NPOLBP * NCHNBP * NIFBP
                  CALL RFILL (I, 0.0, NSUM)
                  I = 3 * I
                  CALL RFILL (I, 0.0, BPSUM)
                  END IF
               END IF
C                                       check for valid data
            DO 15 IIP = BPOL,EPOL
               DO 14 IIF = BIF,EIF
                  I = IIF + (IIP - 1) * NIFBP
                  WT = 1.0
                  I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *               NCHNBP + BCHAN - 1
                  DO 13 IIC = BCHAN,ECHAN
                     I = I + 1
                     IF ((DTERMS(1,I).NE.FBLANK) .AND.
     *                  ((DTERMS(1,I).NE.0.) .OR. (DTERMS(2,I).NE.0.)))
     *                  GO TO 18
 13                  CONTINUE
 14               CONTINUE
 15            CONTINUE
            ANTS(ANTNO) = J
            KREC = KREC + 1
            TIMES(KREC) = ANTNO
            GO TO 90
 18         MREC = MREC + 1
            ANTS(ANTNO) = J
            NREC = NREC + 1
            KREC = KREC + 1
            TIMES(KREC) = ANTNO
C                                       average spectrum
            IF (PTYPE(:3).EQ.'DIF') THEN
               DO 30 IIP = BPOL,EPOL
                  DO 25 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     DO 20 IIC = BCHAN,ECHAN
                        I = I + 1
                        IF ((DTERMS(1,I).NE.FBLANK) .AND.
     *                     ((DTERMS(1,I).NE.0.) .OR.
     *                     (DTERMS(2,I).NE.0.))) THEN
                           BPSUM(1,I) = BPSUM(1,I) + DTERMS(1,I)
                           BPSUM(2,I) = BPSUM(2,I) + DTERMS(2,I)
                           BPSUM(3,I) = BPSUM(3,I) + SQRT
     *                        (DTERMS(1,I)**2 + DTERMS(2,I)**2)
                           NSUM(I) = NSUM(I) + 1.0
                           END IF
 20                     CONTINUE
 25                  CONTINUE
 30               CONTINUE
C                                       or find max/min
            ELSE IF (ARANGE(2).LE.ARANGE(1)) THEN
               DO 50 IIP = BPOL,EPOL
                  DO 45 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     DO 40 IIC = BCHAN,ECHAN
                        I = I + 1
                        IF ((DTERMS(1,I).NE.FBLANK) .AND.
     *                     ((DTERMS(1,I).NE.0.) .OR.
     *                     (DTERMS(2,I).NE.0.))) THEN
                           D = 0.0
                           IF (PTYPE.EQ.'AMP ') THEN
                              D =  SQRT (DTERMS(1,I)**2 +
     *                           DTERMS(2,I)**2)
                           ELSE IF (PTYPE.EQ.'REAL') THEN
                              D =  DTERMS(1,I)
                           ELSE IF (PTYPE.EQ.'IMAG') THEN
                              D =  DTERMS(2,I)
                           ELSE IF ((DTERMS(1,I).NE.0.0) .OR.
     *                        (DTERMS(2,I).NE.0.0)) THEN
                              D = ATAN2 (DTERMS(2,I), DTERMS(1,I)) *
     *                           RAD2DG
                              END IF
                           DMAX = MAX (D, DMAX)
                           DMIN = MIN (D, DMIN)
                           IF (D.LT.0.0) D = D + 360.0
                           PPMAX = MAX (D, PPMAX)
                           PPMIN = MIN (D, PPMIN)
                           END IF
 40                     CONTINUE
 45                  CONTINUE
 50               CONTINUE
               END IF
            END IF
 90      CONTINUE
C                                       Found no more data
      IF (MREC.LE.0) THEN
         MSGTXT = 'Done with all selected data'
         CALL MSGWRT (2)
         IF (NPLOT.GT.0) THEN
            IERR = 0
         ELSE
            IERR = 1
            MSGTXT = 'But no plots were done'
            CALL MSGWRT (7)
            END IF
         GO TO 999
         END IF
C                                       found data but no plot
      IF (NREC.EQ.0) GO TO 300
      NREC = KREC
C                                       average spectrum
      IF (PTYPE(:3).EQ.'DIF') THEN
         DO 120 IIP = BPOL,EPOL
            DO 115 IIF = BIF,EIF
               I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) * NCHNBP +
     *            BCHAN - 1
               DO 110 IIC = BCHAN,ECHAN
                  I = I + 1
                  WT = NSUM(I)
                  IF (WT.GT.0.0) THEN
                     BPSUM(3,I) = BPSUM(3,I) / WT
                     WT = SQRT (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                     IF (WT.GT.0.0) WT = BPSUM(3,I) / WT
                     BPSUM(1,I) = BPSUM(1,I) * WT
                     BPSUM(2,I) = BPSUM(2,I) * WT
                     END IF
 110              CONTINUE
 115           CONTINUE
 120        CONTINUE
         END IF
C                                       now get scale
      IF ((PTYPE(:3).EQ.'DIF') .AND. (ARANGE(2).LE.ARANGE(1))) THEN
         DO 150 J = 1,NBPINR
            IBPRNO = J
            CALL TABPD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, ANTNO, SUBA, FREQID, BPREF, PHDIFF,
     *         DTERMS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ PD TABLE'
               GO TO 990
               END IF
            IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *         ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *         (IANTS(ANTNO).GT.0)) THEN
               DO 140 IIP = BPOL,EPOL
                  DO 135 IIF = BIF,EIF
                     I = IIF + (IIP - 1) * NIFBP
                     I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *                  NCHNBP + BCHAN - 1
                     DO 130 IIC = BCHAN,ECHAN
                        I = I + 1
                        IF ((DTERMS(1,I).NE.FBLANK) .AND.
     *                     ((DTERMS(1,I).NE.0.) .OR.
     *                     (DTERMS(2,I).NE.0.)) .AND.
     *                     (NSUM(I).GT.0.0)) THEN
                           D = 0.0
                           IF (PTYPE.EQ.'DIFC') THEN
                              D =  SQRT (DTERMS(1,I)**2 +
     *                           DTERMS(2,I)**2) - SQRT
     *                           (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                           ELSE IF (PTYPE.EQ.'DIFB') THEN
                              IF (((DTERMS(1,I).NE.0.0) .OR.
     *                           (DTERMS(2,I).NE.0.0)) .AND.
     *                           ((BPSUM(1,I).NE.0.0) .OR.
     *                           (BPSUM(2,I).NE.0.0))) THEN
                                 D = (ATAN2 (DTERMS(2,I),DTERMS(1,I))
     *                              - ATAN2 (BPSUM(2,I), BPSUM(1,I)))
     *                              * RAD2DG
                                 IF (D.LT.-180.0) D = D + 360.0
                                 IF (D.GT.180.0) D = D - 360.0
                                 END IF
                           ELSE
                              DTERMS(1,I) = DTERMS(1,I) - BPSUM(1,I)
                              DTERMS(2,I) = DTERMS(2,I) - BPSUM(2,I)
                              IF (PTYPE.EQ.'DIFA') THEN
                                 D =  SQRT (DTERMS(1,I)**2 +
     *                              DTERMS(2,I)**2)
                              ELSE IF (PTYPE.EQ.'DIFR') THEN
                                 D =  DTERMS(1,I)
                              ELSE IF (PTYPE.EQ.'DIFI') THEN
                                 D =  DTERMS(2,I)
                              ELSE IF ((DTERMS(1,I).NE.0.0) .OR.
     *                           (DTERMS(2,I).NE.0.0)) THEN
                                 D = ATAN2 (DTERMS(2,I), DTERMS(1,I))
     *                              * RAD2DG
                                 END IF
                              END IF
                           DMAX = MAX (D, DMAX)
                           DMIN = MIN (D, DMIN)
                           IF (D.LT.0.0) D = D + 360.0
                           PPMAX = MAX (D, PPMAX)
                           PPMIN = MIN (D, PPMIN)
                           END IF
 130                    CONTINUE
 135                 CONTINUE
 140              CONTINUE
               END IF
 150        CONTINUE
         END IF
C                                       set scale, blank array
      I = MAXCIF + 100
      CALL RFILL (I, 0.0, XPLPTS)
      IF (ARANGE(2).GT.ARANGE(1)) THEN
         PMIN = XRANGE(1)
         PMAX = XRANGE(2)
         SPHASE = PMIN.GE.0.0
      ELSE
         IF (DMAX-DMIN.LE.PPMAX-PPMIN) THEN
            PMIN = DMIN
            PMAX = DMAX
            SPHASE = .FALSE.
         ELSE
            PMIN = PPMIN
            PMAX = PPMAX
            SPHASE = .TRUE.
            END IF
         XRANGE(1) = PMIN
         XRANGE(2) = PMAX
         END IF
      IF (PMIN.EQ.PMAX) THEN
         IF (PMIN.EQ.0.0) THEN
            PMAX = 1.
            PMIN = -1.
         ELSE
            PMAX = 1.03 * PMIN
            PMIN = 0.97 * PMIN
            END IF
         END IF
C                                       now init the plot
      DOTV = XDOTV.GT.0.0
      TVCHN = 0
      TVCORN(1) = 0
      TVCORN(2) = 0
      GRCHN = IROUND (XCHAN)
      PLUN = 26
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', INDISK, CNO, CATBLK, PBUFF, .TRUE.,
     *      'WRIT', IVER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CREATE PLOT FILE'
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
      CALL RFILL (8, 0.0, XTIME)
      XTIME(1) = TPLBEG
      XTIME(5) = TPLEND
      CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
      ITYPE = 30
      CALL GINIT (INDISK, CNO, PFILE, 0, ITYPE, NPARMS, USER, DOTV,
     *   TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND, IERR)
      FUNC = 'GINIT'
      IF (IERR.NE.0) GO TO 900
      CALL BPLABL (IVER, PMIN, PMAX, NREC, TIMES, ANTS, IERR)
      FUNC = 'BPLABL'
      IF (IERR.NE.0) GO TO 900
      DY = 1000.0 / (NREC + FACTOR)
      NP = (EPOL-BPOL+1) * (EIF-BIF+1)
      DX = NP*(ECHAN-BCHAN+4)
      DX = 1000.0 / DX
      X0 = DX / 2.0
      Y0 = -DY / 2.0
      XT = MAX (X0, 17.5)
      DCOLV = 0.97 / MAX (1, NREC-1)
      IF (DO3COL.EQ.2) DCOLV = 0.97 / (PMAX - PMIN)
      COLV = 0.97
C                                       get the data part
      MREC = NREC
      IA = 0
      NRECC = 0
      NRECI = MAX (1, NREC / 20)
      DO 290 J = 1,MREC
 225     IA = IA + 1
         IBPRNO = ANTS(IA)
         IF (IBPRNO.LE.0) GO TO 225
         CALL TABPD ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, ANTNO, SUBA, FREQID, BPREF, PHDIFF,
     *      DTERMS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ PD TABLE'
            GO TO 990
            END IF
         IF (((FREQID.EQ.FQID) .OR. (FREQID.LE.0)) .AND.
     *      ((SUBA.EQ.SUBARR) .OR. (SUBA.LE.0)) .AND.
     *      (IANTS(ANTNO).GT.0)) THEN
            NRECC = NRECC + 1
            IF (DO3COL.EQ.1) THEN
               CALL COLOR3 (COLV, .FALSE., COL)
               COLV = COLV - DCOLV
               CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
               FUNC = 'G3VCOL'
               IF (IERR.NE.0) GO TO 900
               END IF
            Y0 = Y0 + DY
            IP = 0
            LMAX = -1.E10
            LMIN = 1.E10
            DO 240 IIP = BPOL,EPOL
               DO 235 IIF = BIF,EIF
                  I = IIF + (IIP - 1) * NIFBP
                  I = (IIP - 1) * NCHNBP * NIFBP + (IIF - 1) *
     *               NCHNBP + BCHAN - 1
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
                  DO 230 IIC = BCHAN,ECHAN
                     I = I + 1
                     IP = IP + 1
                     D = FBLANK
                     IF ((DTERMS(1,I).NE.FBLANK) .AND.
     *                  ((DTERMS(1,I).NE.0.) .OR. (DTERMS(2,I).NE.0.)))
     *                  THEN
                        IF (PTYPE(:3).EQ.'DIF') THEN
                           IF (NSUM(I).GT.0.0) THEN
                              D = 0.0
                              IF (PTYPE.EQ.'DIFC') THEN
                                 D =  SQRT (DTERMS(1,I)**2 +
     *                              DTERMS(2,I)**2) - SQRT
     *                              (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                              ELSE IF (PTYPE.EQ.'DIFB') THEN
                                 IF (((DTERMS(1,I).NE.0.0) .OR.
     *                              (DTERMS(2,I).NE.0.0)) .AND.
     *                              ((BPSUM(1,I).NE.0.0) .OR.
     *                              (BPSUM(2,I).NE.0.0))) THEN
                                    D = (ATAN2 (DTERMS(2,I),
     *                                 DTERMS(1,I)) - ATAN2
     *                                 (BPSUM(2,I), BPSUM(1,I)))
     *                                 * RAD2DG
                                    IF (D.LT.-180.0) D = D + 360.0
                                    IF (D.GT.180.0) D = D - 360.0
                                    IF ((SPHASE) .AND. (D.LT.0.0))
     *                                 D = D + 360.0
                                    END IF
                              ELSE
                                 DTERMS(1,I) = DTERMS(1,I)-BPSUM(1,I)
                                 DTERMS(2,I) = DTERMS(2,I)-BPSUM(2,I)
                                 IF (PTYPE.EQ.'DIFA') THEN
                                    D =  SQRT (DTERMS(1,I)**2 +
     *                                 DTERMS(2,I)**2)
                                 ELSE IF (PTYPE.EQ.'DIFR') THEN
                                    D =  DTERMS(1,I)
                                 ELSE IF (PTYPE.EQ.'DIFI') THEN
                                    D =  DTERMS(2,I)
                                 ELSE IF ((DTERMS(1,I).NE.0.0) .OR.
     *                              (DTERMS(2,I).NE.0.0)) THEN
                                    D = ATAN2 (DTERMS(2,I),
     *                                 DTERMS(1,I)) * RAD2DG
                                    IF ((SPHASE) .AND. (D.LT.0.0))
     *                                 D = D + 360.0
                                    END IF
                                 END IF
                              END IF
                        ELSE
                           D = 0.0
                           IF (PTYPE.EQ.'AMP') THEN
                              D =  SQRT (DTERMS(1,I)**2 +
     *                           DTERMS(2,I)**2)
                           ELSE IF (PTYPE.EQ.'REAL') THEN
                              D =  DTERMS(1,I)
                           ELSE IF (PTYPE.EQ.'IMAG') THEN
                              D =  DTERMS(2,I)
                           ELSE IF ((DTERMS(1,I).NE.0.0) .OR.
     *                        (DTERMS(2,I).NE.0.0)) THEN
                              D = ATAN2 (DTERMS(2,I), DTERMS(1,I))
     *                           * RAD2DG
                              IF ((SPHASE) .AND. (D.LT.0.0))
     *                           D = D + 360.0
                              END IF
                           END IF
                        END IF
                     IF ((D.NE.FBLANK) .AND. ((D.LT.PMIN) .OR.
     *                  (D.GT.PMAX))) D = FBLANK
                     IF (D.NE.FBLANK) THEN
                        LMAX = MAX (LMAX, D)
                        LMIN = MIN (LMIN, D)
                        END IF
                     PLPTS(IP) = D
 230                 CONTINUE
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
                  IP = IP + 1
                  PLPTS(IP) = FBLANK
 235              CONTINUE
 240           CONTINUE
            IF (ARANGE(2).GE.ARANGE(1)) THEN
               LMAX = PMAX
               LMIN = PMIN
               END IF
            IF (DO3COL.EQ.2) DCOLV = 0.97 / (LMAX - LMIN)
C                                       Do the plot now
            SCALE = DY * FACTOR / (LMAX - LMIN)
C                                       Don't label if too crowded
            IF (MOD(NRECC-1,NRECI).EQ.0) THEN
               IF ((LMAX.GE.0.0) .AND. (LMIN.LE.0.0)) THEN
                  SOFF = Y0 - LMIN * SCALE
                  IF (DO3COL.GE.2) COLV = LMAX * DCOLV
               ELSE
                  SOFF = Y0 + 0.5 * DY * FACTOR
                  IF (DO3COL.GE.2) COLV = 0.5
                  END IF
               CALL GLTYPE (1, PBUFF, IERR)
               FUNC = 'GLTYPE'
               IF (IERR.NE.0) GO TO 900
               IF (DO3COL.GE.2) THEN
                  CALL COLOR3 (COLV, .FALSE., COL)
                  CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                  FUNC = 'G3VCOL'
                  IF (IERR.NE.0) GO TO 900
                  END IF
               FUNC = 'GPOS/VECs'
               X = 0.0
               DO 245 LP = 2,NP
                  X = X + (ECHAN-BCHAN+4) * DX
                  CALL GPOS (X+XT, SOFF, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  IF (DO3COL.GT.0.0) THEN
                     CALL G3VEC (X-XT, SOFF, PBUFF, IERR)
                  ELSE
                     CALL GVEC (X-XT, SOFF, PBUFF, IERR)
                     END IF
                  IF (IERR.NE.0) GO TO 900
 245              CONTINUE
               CALL GPOS (1000.0, SOFF, PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL GVEC (1000.0-XT, SOFF, PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL GPOS (XT, SOFF, PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL GVEC (0.0, SOFF, PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               FUNC = 'GCHAR'
               IF (LTYPE.GT.2) THEN
                  IL = IA
                  WRITE (CLABEL,1240) IL
                  I = 4
                  IF (IL.GT.9) I = 3
                  IF (IL.GT.99) I = 2
                  IF (IL.GT.999) I = 1
                  Y = -(5-I) - 0.5
                  CALL GCHAR (5-I, 0, Y, -0.5, CLABEL(I:4), PBUFF,
     *               IERR)
                  IF (IERR.NE.0) GO TO 900
                  END IF
               END IF
            CALL GLTYPE (2, PBUFF, IERR)
            FUNC = 'GLTYPE'
            IF (IERR.NE.0) GO TO 900
            LBLANK = .TRUE.
            X = X0
            DO 250 I = 1,IP
               IF (PLPTS(I).EQ.FBLANK) THEN
                  LBLANK = .TRUE.
                  X = X + DX
               ELSE
                  Y = Y0 + (PLPTS(I)-LMIN) * SCALE
                  IF ((LBLANK) .OR. (Y.LT.XPLPTS(I))) THEN
                     CALL GPOS (X, Y, PBUFF, IERR)
                     IF (DO3COL.GE.2) THEN
                        YP = Y
                        COLV = (LMAX-PLPTS(I)) * DCOLV
                        CALL COLOR3 (COLV, .FALSE., COL)
                        CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                     IERR)
                        FUNC = 'G3VCOL'
                        IF (IERR.NE.0) GO TO 900
                        END IF
                  ELSE IF (DO3COL.GE.2) THEN
                     YH = (YP + Y) / 2.0
                     CALL G3VEC (X, YH, PBUFF, IERR)
                     YP = Y
                     COLV = (LMAX-PLPTS(I)) * DCOLV
                     CALL COLOR3 (COLV, .FALSE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF,
     *                  IERR)
                     FUNC = 'G3VCOL'
                     IF (IERR.NE.0) GO TO 900
                     CALL G3VEC (X, Y, PBUFF, IERR)
                  ELSE IF (DO3COL.EQ.1) THEN
                     CALL G3VEC (X, Y, PBUFF, IERR)
                  ELSE
                     CALL GVEC (X, Y, PBUFF, IERR)
                     END IF
                  FUNC = 'GVEC'
                  IF (IERR.NE.0) GO TO 900
                  LBLANK = .FALSE.
                  X = X + DX
                  IF (Y.LT.XPLPTS(I)) THEN
                     CALL GPOS (X, Y, PBUFF, IERR)
                  ELSE IF (DO3COL.GT.0.0) THEN
                     CALL G3VEC (X, Y, PBUFF, IERR)
                     XPLPTS(I) = Y
                  ELSE
                     CALL GVEC (X, Y, PBUFF, IERR)
                     XPLPTS(I) = Y
                     END IF
                  IF (IERR.NE.0) GO TO 900
                  END IF
 250           CONTINUE
            END IF
 290     CONTINUE
C                                       finish plot and continue?
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, IERR)
C                                       Successful plot file finished.
      IF (IERR.EQ.0) THEN
         NPLOT = NPLOT + 1
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (INDISK, CNO, IVER, PBUFF, IERR)
            WRITE (MSGTXT,1290) IVER
            CALL MSGWRT (5)
            IERR = 0
            END IF
         END IF
C
 300  IERR = MAX (0, IERR)
C                                       close
      CALL TABPD ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, ANTNO, SUBA, FREQID, BPREF, PHDIFF, DTERMS, IERR)
      GO TO 999
C                                       plotter error
 900  WRITE (MSGTXT,1900) IERR, FUNC
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO SAVE PARTIAL GRAPH'
      CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PDLOTT: ERROR',I5,' ON ',A)
 1240 FORMAT (I4)
 1290 FORMAT ('Successful plot file version',I5,' written')
 1900 FORMAT ('PLOTTING ERROR',I5,' AT ',A)
      END
      SUBROUTINE CPLOTT (IERR)
C-----------------------------------------------------------------------
C   CPLOTT uses the open CP table to generate the plots.
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPLOT.INC'
      CHARACTER PFILE*48, CLABEL*4, FUNC*12, SNAME(20)*16,
     *   SOURCE*16
      INTEGER   I, J, NREC, IP, IA, ANTS(2000), IIF, IIC, MREC, TVCHN,
     *   GRCHN, TVCORN(2), IVER, IROUND, IL, NP, LP, ITYPE, NPLOT,
     *   NRECI, NRECC, SUID, KREC
      REAL      TIMES(20), WT, D, DMAX, DMIN, BPSUM(3,MAXCIF),
     *   VFLUX(4,MAXCIF), NSUM(MAXCIF), DX, DY, X0, Y0, SCALE, X, Y,
     *   SOFF, PLPTS(MAXCIF+100), XPLPTS(MAXCIF+100), PMAX, PMIN, XT,
     *   COL(3), COLV, DCOLV, YP, YH, PPMAX, PPMIN, LMAX, LMIN
      LOGICAL   DOTV, LBLANK, SPHASE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (LABEL.GT.2) LABEL = 6
      IF (LABEL.LT.-2) LABEL = -6
      NPLOT = 0
C                                       find range of desired data
      NREC = 0
      MREC = 0
      KREC = 0
      DO 90 J = 1,NBPINR
         IBPRNO = J
         CALL TABCP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, SOURCE, SUID, VFLUX, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ CP TABLE'
            GO TO 990
            END IF
         IF (NREC.EQ.0) THEN
            KREC = 0
            DMAX = -1.E10
            DMIN = -DMAX
            PPMAX = DMAX
            PPMIN = DMIN
            IF (PTYPE(:3).EQ.'DIF') THEN
               I = NCHNBP * NIFBP
               CALL RFILL (I, 0.0, NSUM)
               I = 3 * I
               CALL RFILL (I, 0.0, BPSUM)
               END IF
            END IF
C                                       check for valid data
         DO 14 IIF = BIF,EIF
            I = (IIF - 1) * NCHNBP + BCHAN - 1
            DO 13 IIC = BCHAN,ECHAN
               I = I + 1
               IF ((VFLUX(1,I).NE.FBLANK) .AND. (VFLUX(1,I).NE.0.0)
     *            .AND. ((VFLUX(2,I).NE.0.0) .OR. (VFLUX(3,I).NE.0.0)))
     *            GO TO 18
 13            CONTINUE
 14         CONTINUE
         KREC = KREC + 1
         ANTS(SUID) = J
         TIMES(KREC) = SUID
         SNAME(KREC) = SOURCE
         GO TO 90
 18      MREC = MREC + 1
         NREC = NREC + 1
         KREC = KREC + 1
         ANTS(SUID) = J
         TIMES(KREC) = SUID
         SNAME(KREC) = SOURCE
C                                       average spectrum
         IF (PTYPE(:3).EQ.'DIF') THEN
            DO 25 IIF = BIF,EIF
               I = (IIF - 1) * NCHNBP + BCHAN - 1
               DO 20 IIC = BCHAN,ECHAN
                  I = I + 1
                  IF ((VFLUX(1,I).NE.FBLANK) .AND. (VFLUX(1,I).GT.0.0))
     *               THEN
                     BPSUM(1,I) = BPSUM(1,I) + VFLUX(2,I) / VFLUX(1,I)
                     BPSUM(2,I) = BPSUM(2,I) + VFLUX(3,I) / VFLUX(1,I)
                     BPSUM(3,I) = BPSUM(3,I) + SQRT (VFLUX(2,I)**2 +
     *                  VFLUX(3,I)**2) / VFLUX(1,I)
                     NSUM(I) = NSUM(I) + 1.0
                     END IF
 20               CONTINUE
 25            CONTINUE
C                                       or find max/min
         ELSE IF (ARANGE(2).LE.ARANGE(1)) THEN
            DO 45 IIF = BIF,EIF
               I = (IIF - 1) * NCHNBP + BCHAN - 1
               DO 40 IIC = BCHAN,ECHAN
                  I = I + 1
                  IF ((VFLUX(1,I).NE.FBLANK) .AND. (VFLUX(1,I).GT.0.0))
     *               THEN
                     D = 0.0
                     IF (PTYPE.EQ.'AMP ') THEN
                        D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2) /
     *                     VFLUX(1,I)
                     ELSE IF (PTYPE.EQ.'REAL') THEN
                        D =  VFLUX(2,I)
                     ELSE IF (PTYPE.EQ.'IMAG') THEN
                        D =  VFLUX(3,I)
                     ELSE IF ((VFLUX(2,I).NE.0.0) .OR.
     *                  (VFLUX(3,I).NE.0.0)) THEN
                        D = ATAN2 (VFLUX(3,I), VFLUX(2,I)) * RAD2DG
                        END IF
                     DMAX = MAX (D, DMAX)
                     DMIN = MIN (D, DMIN)
                     IF (D.LT.0.0) D = D + 360.0
                     PPMAX = MAX (D, PPMAX)
                     PPMIN = MIN (D, PPMIN)
                     END IF
 40               CONTINUE
 45            CONTINUE
            END IF
 90      CONTINUE
C                                       Found no more data
      IF (MREC.LE.0) THEN
         MSGTXT = 'Done with all selected data'
         CALL MSGWRT (2)
         IF (NPLOT.GT.0) THEN
            IERR = 0
         ELSE
            IERR = 1
            MSGTXT = 'But no plots were done'
            CALL MSGWRT (7)
            END IF
         GO TO 999
         END IF
C                                       NREC assumed > 0
      NREC = KREC
C                                       average spectrum
      IF (PTYPE(:3).EQ.'DIF') THEN
         DO 115 IIF = BIF,EIF
            I = (IIF - 1) * NCHNBP + BCHAN - 1
            DO 110 IIC = BCHAN,ECHAN
               I = I + 1
               WT = NSUM(I)
               IF (WT.GT.0.0) THEN
                  BPSUM(3,I) = BPSUM(3,I) / WT
                  WT = SQRT (BPSUM(1,I)**2 + BPSUM(2,I)**2)
                  IF (WT.GT.0.0) WT = BPSUM(3,I) / WT
                  BPSUM(1,I) = BPSUM(1,I) * WT
                  BPSUM(2,I) = BPSUM(2,I) * WT
                  END IF
 110           CONTINUE
 115        CONTINUE
         END IF
C                                       now get scale
      IF ((PTYPE(:3).EQ.'DIF') .AND. (ARANGE(2).LE.ARANGE(1))) THEN
         DO 150 J = 1,NBPINR
            IBPRNO = J
            CALL TABCP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, SOURCE, SUID, VFLUX, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ CP TABLE'
               GO TO 990
               END IF
            DO 135 IIF = BIF,EIF
               I = (IIF - 1) * NCHNBP + BCHAN - 1
               DO 130 IIC = BCHAN,ECHAN
                  I = I + 1
                  IF ((VFLUX(1,I).NE.FBLANK) .AND. (NSUM(I).GT.0.0))
     *               THEN
                     D = 0.0
                     IF (PTYPE.EQ.'DIFC') THEN
                        D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2) /
     *                     VFLUX(1,I) - SQRT (BPSUM(1,I)**2 +
     *                     BPSUM(2,I)**2)
                     ELSE IF (PTYPE.EQ.'DIFB') THEN
                        IF (((VFLUX(2,I).NE.0.0) .OR.
     *                     (VFLUX(3,I).NE.0.0)) .AND.
     *                     ((BPSUM(1,I).NE.0.0) .OR.
     *                     (BPSUM(2,I).NE.0.0))) THEN
                           D = (ATAN2 (VFLUX(3,I),VFLUX(2,I))
     *                        - ATAN2 (BPSUM(2,I), BPSUM(1,I)))
     *                        * RAD2DG
                           IF (D.LT.-180.0) D = D + 360.0
                           IF (D.GT.180.0) D = D - 360.0
                           END IF
                     ELSE
                        VFLUX(2,I) = VFLUX(2,I)/VFLUX(1,I) - BPSUM(1,I)
                        VFLUX(3,I) = VFLUX(3,I)/VFLUX(1,I) - BPSUM(2,I)
                        IF (PTYPE.EQ.'DIFA') THEN
                           D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2)
                        ELSE IF (PTYPE.EQ.'DIFR') THEN
                           D =  VFLUX(2,I)
                        ELSE IF (PTYPE.EQ.'DIFI') THEN
                           D =  VFLUX(3,I)
                        ELSE IF ((VFLUX(2,I).NE.0.0) .OR.
     *                     (VFLUX(2,I).NE.0.0)) THEN
                           D = ATAN2 (VFLUX(3,I), VFLUX(2,I)) * RAD2DG
                           END IF
                        END IF
                     DMAX = MAX (D, DMAX)
                     DMIN = MIN (D, DMIN)
                     IF (D.LT.0.0) D = D + 360.0
                     PPMAX = MAX (D, PPMAX)
                     PPMIN = MIN (D, PPMIN)
                     END IF
 130              CONTINUE
 135           CONTINUE
 150        CONTINUE
         END IF
C                                       set scale, blank array
      I = MAXCIF + 100
      CALL RFILL (I, 0.0, XPLPTS)
      IF (ARANGE(2).GT.ARANGE(1)) THEN
         PMIN = XRANGE(1)
         PMAX = XRANGE(2)
         SPHASE = PMIN.GE.0.0
      ELSE
         IF (DMAX-DMIN.LE.PPMAX-PPMIN) THEN
            PMIN = DMIN
            PMAX = DMAX
            SPHASE = .FALSE.
         ELSE
            PMIN = PPMIN
            PMAX = PPMAX
            SPHASE = .TRUE.
            END IF
         XRANGE(1) = PMIN
         XRANGE(2) = PMAX
         END IF
      IF (PMIN.EQ.PMAX) THEN
         IF (PMIN.EQ.0.0) THEN
            PMAX = 1.
            PMIN = -1.
         ELSE
            PMAX = 1.03 * PMIN
            PMIN = 0.97 * PMIN
            END IF
         END IF
C                                       now init the plot
      DOTV = XDOTV.GT.0.0
      TVCHN = 0
      TVCORN(1) = 0
      TVCORN(2) = 0
      GRCHN = IROUND (XCHAN)
      PLUN = 26
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', INDISK, CNO, CATBLK, PBUFF, .TRUE.,
     *      'WRIT', IVER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CREATE PLOT FILE'
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
      CALL ZPHFIL ('PL', INDISK, CNO, IVER, PFILE, IERR)
      ITYPE = 30
      CALL GINIT (INDISK, CNO, PFILE, 0, ITYPE, NPARMS, USER, DOTV,
     *   TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND, IERR)
      FUNC = 'GINIT'
      IF (IERR.NE.0) GO TO 900
      CALL BPLABL (IVER, PMIN, PMAX, NREC, TIMES, ANTS, IERR)
      FUNC = 'BPLABL'
      IF (IERR.NE.0) GO TO 900
      DY = 1000.0 / (NREC + FACTOR)
      NP = (EPOL-BPOL+1) * (EIF-BIF+1)
      DX = NP*(ECHAN-BCHAN+4)
      DX = 1000.0 / DX
      X0 = DX / 2.0
      Y0 = -DY / 2.0
      XT = MAX (X0, 17.5)
      DCOLV = 0.97 / MAX (1, NREC-1)
      IF (DO3COL.EQ.2) DCOLV = 0.97 / (PMAX - PMIN)
      COLV = 0.97
C                                       get the data part
      MREC = NREC
      IA = 0
      NRECC = 0
      NRECI = MAX (1, NREC / 20)
      IA = 0
      DO 290 J = 1,NBPINR
C                                       get right record
 225     IA = IA + 1
         IBPRNO = ANTS(IA)
         IF (IBPRNO.LE.0) GO TO 225
C                                       read data
         CALL TABCP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, SOURCE, SUID, VFLUX, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ CP TABLE'
            GO TO 990
            END IF
         NRECC = NRECC + 1
         IF (DO3COL.EQ.1) THEN
            CALL COLOR3 (COLV, .FALSE., COL)
            COLV = COLV - DCOLV
            CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
            FUNC = 'G3VCOL'
            IF (IERR.NE.0) GO TO 900
            END IF
         Y0 = Y0 + DY
         IP = 0
         LMAX = -1.E10
         LMIN = 1.E10
         DO 235 IIF = BIF,EIF
            I = (IIF - 1) * NCHNBP + BCHAN - 1
            IP = IP + 1
            PLPTS(IP) = FBLANK
            DO 230 IIC = BCHAN,ECHAN
               I = I + 1
               IP = IP + 1
               D = FBLANK
               IF ((VFLUX(1,I).NE.FBLANK) .AND. (VFLUX(1,I).GT.0.0))
     *            THEN
                  IF (PTYPE(:3).EQ.'DIF') THEN
                     IF (NSUM(I).GT.0.0) THEN
                        D = 0.0
                        IF (PTYPE.EQ.'DIFC') THEN
                           D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2) /
     *                        VFLUX(1,I) - SQRT (BPSUM(1,I)**2 +
     *                        BPSUM(2,I)**2)
                        ELSE IF (PTYPE.EQ.'DIFB') THEN
                           IF (((VFLUX(2,I).NE.0.0) .OR.
     *                        (VFLUX(3,I).NE.0.0)) .AND.
     *                        ((BPSUM(1,I).NE.0.0) .OR.
     *                        (BPSUM(2,I).NE.0.0))) THEN
                              D = (ATAN2 (VFLUX(3,I), VFLUX(2,I)) -
     *                           ATAN2 (BPSUM(2,I), BPSUM(1,I))) *
     *                           RAD2DG
                              IF (D.LT.-180.0) D = D + 360.0
                              IF (D.GT.180.0) D = D - 360.0
                              IF ((SPHASE) .AND. (D.LT.0.0)) D = D+360.
                              END IF
                        ELSE
                           VFLUX(2,I) = VFLUX(2,I) / VFLUX(1,I) -
     *                        BPSUM(1,I)
                           VFLUX(3,I) = VFLUX(3,I) / VFLUX(1,I) -
     *                        BPSUM(2,I)
                           IF (PTYPE.EQ.'DIFA') THEN
                              D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2)
                           ELSE IF (PTYPE.EQ.'DIFR') THEN
                              D =  VFLUX(2,I)
                           ELSE IF (PTYPE.EQ.'DIFI') THEN
                              D =  VFLUX(3,I)
                           ELSE IF ((VFLUX(2,I).NE.0.0) .OR.
     *                        (VFLUX(3,I).NE.0.0)) THEN
                              D = ATAN2 (VFLUX(3,I), VFLUX(2,I)) *
     *                           RAD2DG
                              IF ((SPHASE) .AND. (D.LT.0.0))
     *                            D = D + 360.0
                              END IF
                           END IF
                        END IF
                  ELSE
                     D = 0.0
                     IF (PTYPE.EQ.'AMP') THEN
                        D =  SQRT (VFLUX(2,I)**2 + VFLUX(3,I)**2) /
     *                     VFLUX(1,I)
                     ELSE IF (PTYPE.EQ.'REAL') THEN
                        D =  VFLUX(2,I)
                     ELSE IF (PTYPE.EQ.'IMAG') THEN
                        D =  VFLUX(3,I)
                     ELSE IF ((VFLUX(2,I).NE.0.0) .OR.
     *                  (VFLUX(3,I).NE.0.0)) THEN
                        D = ATAN2 (VFLUX(3,I), VFLUX(2,I)) * RAD2DG
                        IF ((SPHASE) .AND. (D.LT.0.0))
     *                     D = D + 360.0
                        END IF
                     END IF
                  END IF
               IF ((D.NE.FBLANK) .AND. ((D.LT.PMIN) .OR. (D.GT.PMAX)))
     *            D = FBLANK
               IF (D.NE.FBLANK) THEN
                  LMAX = MAX (LMAX, D)
                  LMIN = MIN (LMIN, D)
                  END IF
               PLPTS(IP) = D
 230           CONTINUE
            IP = IP + 1
            PLPTS(IP) = FBLANK
            IP = IP + 1
            PLPTS(IP) = FBLANK
 235        CONTINUE
         IF (ARANGE(2).GE.ARANGE(1)) THEN
            LMAX = PMAX
            LMIN = PMIN
            END IF
         IF (DO3COL.EQ.2) DCOLV = 0.97 / (LMAX - LMIN)
C                                       Do the plot now
         SCALE = DY * FACTOR / (LMAX - LMIN)
C                                       Don't label if too crowded
         IF (MOD(NRECC-1,NRECI).EQ.0) THEN
            IF ((LMAX.GE.0.0) .AND. (LMIN.LE.0.0)) THEN
               SOFF = Y0 - LMIN * SCALE
               IF (DO3COL.GE.2) COLV = LMAX * DCOLV
            ELSE
               SOFF = Y0 + 0.5 * DY * FACTOR
               IF (DO3COL.GE.2) COLV = 0.5
               END IF
            CALL GLTYPE (1, PBUFF, IERR)
            FUNC = 'GLTYPE'
            IF (IERR.NE.0) GO TO 900
            IF (DO3COL.GE.2) THEN
               CALL COLOR3 (COLV, .FALSE., COL)
               CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
               FUNC = 'G3VCOL'
               IF (IERR.NE.0) GO TO 900
               END IF
            FUNC = 'GPOS/VECs'
            X = 0.0
            DO 245 LP = 2,NP
               X = X + (ECHAN-BCHAN+4) * DX
               CALL GPOS (X+XT, SOFF, PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               IF (DO3COL.GT.0.0) THEN
                  CALL G3VEC (X-XT, SOFF, PBUFF, IERR)
               ELSE
                  CALL GVEC (X-XT, SOFF, PBUFF, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 900
 245           CONTINUE
            CALL GPOS (1000.0, SOFF, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL GVEC (1000.0-XT, SOFF, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL GPOS (XT, SOFF, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL GVEC (0.0, SOFF, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
            FUNC = 'GCHAR'
            IF (LTYPE.GT.2) THEN
               IL = IA
               WRITE (CLABEL,1240) IL
               I = 4
               IF (IL.GT.9) I = 3
               IF (IL.GT.99) I = 2
               IF (IL.GT.999) I = 1
               Y = -(5-I) - 0.5
               CALL GCHAR (5-I, 0, Y, -0.5, CLABEL(I:4), PBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            END IF
         CALL GLTYPE (2, PBUFF, IERR)
         FUNC = 'GLTYPE'
         IF (IERR.NE.0) GO TO 900
         LBLANK = .TRUE.
         X = X0
         DO 250 I = 1,IP
            IF (PLPTS(I).EQ.FBLANK) THEN
               LBLANK = .TRUE.
               X = X + DX
            ELSE
               Y = Y0 + (PLPTS(I)-LMIN) * SCALE
               IF ((LBLANK) .OR. (Y.LT.XPLPTS(I))) THEN
                  CALL GPOS (X, Y, PBUFF, IERR)
                  IF (DO3COL.GE.2) THEN
                     YP = Y
                     COLV = (LMAX-PLPTS(I)) * DCOLV
                     CALL COLOR3 (COLV, .FALSE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                     FUNC = 'G3VCOL'
                     IF (IERR.NE.0) GO TO 900
                     END IF
               ELSE IF (DO3COL.GE.2) THEN
                  YH = (YP + Y) / 2.0
                  CALL G3VEC (X, YH, PBUFF, IERR)
                  YP = Y
                  COLV = (LMAX-PLPTS(I)) * DCOLV
                  CALL COLOR3 (COLV, .FALSE., COL)
                  CALL G3VCOL (COL(1), COL(2), COL(3), PBUFF, IERR)
                  FUNC = 'G3VCOL'
                  IF (IERR.NE.0) GO TO 900
                  CALL G3VEC (X, Y, PBUFF, IERR)
               ELSE IF (DO3COL.EQ.1) THEN
                  CALL G3VEC (X, Y, PBUFF, IERR)
               ELSE
                  CALL GVEC (X, Y, PBUFF, IERR)
                  END IF
               FUNC = 'GVEC'
               IF (IERR.NE.0) GO TO 900
               LBLANK = .FALSE.
               X = X + DX
               IF (Y.LT.XPLPTS(I)) THEN
                  CALL GPOS (X, Y, PBUFF, IERR)
               ELSE IF (DO3COL.GT.0.0) THEN
                  CALL G3VEC (X, Y, PBUFF, IERR)
                  XPLPTS(I) = Y
               ELSE
                  CALL GVEC (X, Y, PBUFF, IERR)
                  XPLPTS(I) = Y
                  END IF
               IF (IERR.NE.0) GO TO 900
               END IF
 250        CONTINUE
 290     CONTINUE
C                                       finish plot and continue?
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, IERR)
C                                       Successful plot file finished.
      IF (IERR.EQ.0) THEN
         NPLOT = NPLOT + 1
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (INDISK, CNO, IVER, PBUFF, IERR)
            WRITE (MSGTXT,1290) IVER
            CALL MSGWRT (5)
            IERR = 0
            END IF
         END IF
      IERR = MAX (0, IERR)
C                                       close
      CALL TABCP ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   SOURCE, SUID, VFLUX, IERR)
      GO TO 999
C                                       plotter error
 900  WRITE (MSGTXT,1900) IERR, FUNC
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO SAVE PARTIAL GRAPH'
      CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL GFINIS (PBUFF, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPLOTT: ERROR',I5,' ON ',A)
 1240 FORMAT (I4)
 1290 FORMAT ('Successful plot file version',I5,' written')
 1900 FORMAT ('PLOTTING ERROR',I5,' AT ',A)
      END
