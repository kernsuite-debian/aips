      SUBROUTINE IMREF1 (DISK, CNO, VER, ATTVER, CATBLK, LUN, ROWF,
     *   IRET)
C-----------------------------------------------------------------------
C! Checks existence of IM table ver 1, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000
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
C   Routine to change the format of the IM table 1 to latest version.
C   N.B. A few projects in the early days of the VLBA correlator had
C   the wrong IM table format (TABREV = 1), this routine updates them.
C
C   NOTE: routine uses LUNs 45 &46 as a temporary logical unit number.
C   Inputs:
C      DISK     I        Volume number
C      CNO      I        Catalogue number
C      VER      I        Version # of old format table
C      ATTVER   I        Version # of output table, may be same as VER
C      CATBLK   I(256)   Catalogue header
C      LUN      I        LUN to use
C   Output:
C      ROWF     I        Start row in output table
C      IRET     I        Error, 0 => OK
C   The common set by DIMV.INC is used on input if the input IM table
C   lacks some of the critical keywords and is altered by this routine.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, ATTVER, ROWF, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFF1(512), BUFF2(512), OUTVER, OLUN, IDUM, NROW,
     *   IPT, I, J, K, IOFF
      LOGICAL   TABLE, EXIST, FITASC, CHSTAT
      CHARACTER UTYPE*2, CTEMP*12, STAT*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGLB.INC'
C                                       Include for Interferometer
C                                       Model table handling
C                                       routines. Version #1
C                                       IM definitions
      INTEGER   MAXLMC, NKEYLM, MAXWLM, MAXLEC, MAXLCD, MAXLOL
C                                       # cols in LM table
      PARAMETER (MAXLMC=14)
C                                       # keywords in LM table
      PARAMETER (NKEYLM=12)
C                                       max # words in header array
      PARAMETER (MAXWLM=20)
C                                       Max order of polynomial
C                                       for geometrical parameters
      PARAMETER (MAXLOL=10)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXLEC=8+13*MAXIF+8*MAXLOL)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXLCD=MAXLEC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER   LTLM, LINT, LSOU, LANO, LARR, LFRE, LIFR, LFVR,
     *   LPDL, LGDL, LPRT, LGRT, LPDSP1, LPDT1
      PARAMETER (LTLM=1)
      PARAMETER (LINT=2)
      PARAMETER (LSOU=3)
      PARAMETER (LANO=4)
      PARAMETER (LARR=5)
      PARAMETER (LFRE=6)
      PARAMETER (LIFR=7)
      PARAMETER (LFVR=8)
      PARAMETER (LPDL=9)
      PARAMETER (LGDL=10)
      PARAMETER (LPRT=11)
      PARAMETER (LGRT=12)
      PARAMETER (LPDSP1=13)
      PARAMETER (LPDT1=14)
C                                       IM1 specific keywords
      INTEGER   ILMRNO, NXPOLZ, NXOLY
      REAL      RXVNUM
C                                       IM1 specific column variables
      INTEGER   NXSTA, IXRRAY, IXQID, IXRC
      REAL      TXNT, IXR, FXEQVR(MAXIF), DXSP(MAXIF), DXISP(MAXIF)
      DOUBLE PRECISION TXME, PXELAY(MAXIF,MAXLOL), GXELAY(MAXLOL),
     *   PXATE(MAXIF,MAXLOL), GXATE(MAXLOL)
C                                       Generic table variables
      INTEGER   LKEY, LLEC, LDATP(128,2), LCOL, LPOINT
      INTEGER   LMKOLS(MAXLMC), LMNUMV(MAXLMC), LDTYP(MAXLMC)
      INTEGER   LLOCS(NKEYLM), LEYTYP(NKEYLM)
      INTEGER   LEYVAL(MAXWLM)
      INTEGER   LECI(MAXLEC)
      REAL      LEYVAR(MAXWLM)
      REAL      LECORD(MAXLEC)
      HOLLERITH LEYVAH(MAXWLM)
      HOLLERITH LECH(MAXLEC)
      DOUBLE PRECISION LECD(MAXLCD), KEYVAD
      CHARACTER  LEYW(NKEYLM)*8
      EQUIVALENCE (LEYVAL, LEYVAR, LEYVAH)
      EQUIVALENCE (LECD, LECORD, LECI, LECH)
C                                       local commons only
      COMMON /LMTABV/ LKEY, LLEC, LDATP, LCOL, LPOINT, LMKOLS, LMNUMV,
     *   LDTYP, LLOCS, LEYTYP, LEYVAL
      COMMON /LMVALS/ TXME, PXELAY, GXELAY, PXATE, GXATE, TXNT, IXR,
     *   FXEQVR, DXSP, DXISP, RXVNUM, ILMRNO, NXPOLZ, NXOLY, NXSTA,
     *   IXRRAY, IXQID, IXRC
      INCLUDE 'INCS:DIMV.INC'
C                                       Header leywords
      DATA LEYW /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'NO_BAND ',
     *   'NO_CHAN', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL',
     *   'NO_POL', 'NPOLY', 'REVISION', 'TABREV' /
C
      DATA OLUN /45/
C-----------------------------------------------------------------------
C                                       Check existance
      CALL ISTAB ('IM', DISK, CNO, VER, LUN, BUFF1, TABLE, EXIST,
     *   FITASC, IRET)
      IRET = 0
      IF (.NOT.EXIST) GO TO 999
C                                       Copy old file
      OUTVER = 0
      I = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('IM', VER, OUTVER, LUN, OLUN, DISK, DISK, CNO, CNO,
     *   CATBLK, BUFF1, BUFF2, IRET)
      MSGSUP = I
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Delete old version
      CALL RMEXT (DISK, CNO, 'IM', VER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IRET)
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, BUFF1, IRET)
C                                       Change status
      IF (STAT.NE.'WRIT') THEN
         CALL STATCH (STAT, 'WRIT', DISK, CNO, UTYPE, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLRD'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       Open old version
C                                       Set up needed variables
      LLEC = 30
      LCOL = 0
      LKEY = NKEYLM
C                                       Open file
      CALL TABINI ('READ', 'IM', DISK, CNO, OUTVER, CATBLK, OLUN, LKEY,
     *   LLEC, LCOL, LDATP, BUFF1, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('READ', 'TABINI', 'IMREF1', IRET)
         GO TO 990
         END IF
C                                       # rows in old table
      NROW = BUFF1(5)
C                                       Read keywords
      IF (LKEY.NE.NKEYLM) THEN
         WRITE (MSGTXT,1020) LKEY, NKEYLM
         CALL MSGWRT (6)
         END IF
      LKEY = NKEYLM
      CALL TABKEY ('READ', LEYW, LKEY, BUFF1, LLOCS, LEYVAL, LEYTYP,
     *   IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
         CALL TABERR ('READ', 'TABKEY', 'IMREF1', IRET)
         GO TO 990
         END IF
      IRET = 0
C                                       Observing code
      IPT = 1
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) CALL H2CHR (8, 1, LEYVAH(IPOINT), OBSCOD)
C                                       # Stokes in data
      IPT = IPT + 1
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) NOSTKD = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       1st Stokes in data
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) STK1 = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       # band's (IF's) in data
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) NOBAND = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       # spectral channels in data
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) NOCHAN = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       Ref frq
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) THEN
         IF (LEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, LEYVAR(IPOINT), REFFRQ)
         ELSE
            REFFRQ = LEYVAR(IPOINT)
            END IF
         END IF
      IPT = IPT + 1
C                                       Chn. bw
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) THEN
         IF (LEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, LEYVAR(IPOINT), KEYVAD)
            CHNBW = KEYVAD
         ELSE
            CHNBW = LEYVAR(IPOINT)
            END IF
         END IF
      IPT = IPT + 1
C                                       Ref. pixel
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) THEN
         IF (LEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, LEYVAR(IPOINT), KEYVAD)
            REFPIX = KEYVAD
         ELSE
            REFPIX = LEYVAR(IPOINT)
            END IF
         END IF
      IPT = IPT + 1
C                                       # polzns in table
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) NOPOLZ = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       Order of polynomial
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) NPOLY = LEYVAL(IPOINT)
      IPT = IPT + 1
C                                       Revision number
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) THEN
         IF (LEYTYP(IPT).EQ.TABDBL) THEN
            CALL RCOPY (NWDPDP, LEYVAR(IPOINT), KEYVAD)
            REVNUM = KEYVAD
         ELSE
            REVNUM = LEYVAR(IPOINT)
            END IF
         END IF
      IPT = IPT + 1
C                                       Table revision number
      IPOINT = LLOCS(IPT)
      IF (IPOINT.GT.0) TABREV = LEYVAL(IPOINT)
C                                       Get array indices and no. values
      DO 40 I = 1,LCOL
         LMKOLS(I) = LDATP(I,1)
         LMNUMV(I) = LDATP(I,2) / 10
   40    CONTINUE
C                                       Set up new table
      TABREV = 2
      CALL IMINI ('WRIT', BUFF2, DISK, CNO, ATTVER, CATBLK, LUN,
     *   IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('WRIT', 'IMINI', 'IMREF1', IRET)
         GO TO 990
         END IF
      ROWF = IIMRNO
C                                       Loop and translate
      DO 100 J = 1, NROW
         ILMRNO = J
         CALL TABIO ('READ', 0, ILMRNO, LECORD, BUFF1, IRET)
         IF (IRET.GT.0) THEN
            CALL TABERR ('READ', 'TABIO', 'IMREF1', IRET)
            GO TO 990
            END IF
         TIME = LECD(LMKOLS(LTLM))
         TINT = LECORD(LMKOLS(LINT))
         ISRC = LECI(LMKOLS(LSOU))
         NOSTA = LECI(LMKOLS(LANO))
         IARRAY = LECI(LMKOLS(LARR))
         IFQID = LECI(LMKOLS(LFRE))
         IFR = LECORD(LMKOLS(LIFR))
         DO 50 I = 1, NPOLY
            GDELA1(I) = LECD(LMKOLS(LGDL)+I-1)
            GRATE1(I) = LECD(LMKOLS(LGRT)+I-1)
            IF (NOPOLZ.GT.1) THEN
               GDELA2(I) = LECD(LMKOLS(LGDL)+I-1)
               GRATE2(I) = LECD(LMKOLS(LGRT)+I-1)
               END IF
   50       CONTINUE
         DO 60 I = 1, NOBAND
            FREQVR(I) = LECORD(LMKOLS(LFVR)+I-1)
   60       CONTINUE
         DO 80 I = 1, NOBAND
            IOFF = (I-1) * NPOLY
            DO 70 K = 1, NPOLY
               PDELA1(I,K) = LECD(LMKOLS(LPDL)+IOFF+K-1)
               PRATE1(I,K) = LECD(LMKOLS(LPRT)+IOFF+K-1)
               IF (NOPOLZ.GT.1) THEN
                  PDELA2(I,K) = LECD(LMKOLS(LPDL)+IOFF+K-1)
                  PRATE2(I,K) = LECD(LMKOLS(LPRT)+IOFF+K-1)
                  END IF
   70          CONTINUE
   80       CONTINUE
C                                       Initialize missing components
            DISP1 = 0.0
            DISP2 = 0.0
            DDISP1 = 0.0
            DDISP2 = 0.0
C                                       Write new row
         CALL IMTAB ('WRIT', BUFF2, NOBAND, IRET)
         IF (IRET.GT.0) THEN
            CALL TABERR ('WRIT', 'IMTAB', 'IMREF1', IRET)
            GO TO 990
            END IF
  100    CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 0, ILMRNO, BUFF1, BUFF1, IRET)
      CALL TABIO ('CLOS', 0, IIMRNO, BUFF2, BUFF2, IRET)
C                                       Delete copy
      CALL RMEXT (DISK, CNO, 'IM', OUTVER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Check if changed status
C                                       Change status
      IF (CHSTAT) THEN
         CALL STATCH ('WRIT', STAT, DISK, CNO, UTYPE, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLWR'
            GO TO 990
            END IF
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFF1, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMREF1: ERROR ',I3,' COPYING OLD TABLE')
 1010 FORMAT ('IMREF1: ERROR ',I3,' DELETING OLD TABLE')
 1020 FORMAT ('IMREF1: NUMBER KEYWORDS',I4,' NOT EXPECTED',I3)
 1080 FORMAT ('IMREF1: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
