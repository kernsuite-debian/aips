LOCAL INCLUDE 'SETJY.INC'
C                                       Local include for SETJY
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSOU
C                                       Maximum number of sources
      PARAMETER (MAXSOU = 5000)
      INTEGER   SEQIN, DISKIN, OLDCNO, IVER, EIF, BIF, NSOUWD, FREQID,
     *   SQUAL, NSOURC, ICOEFF, JCOEFF(MAXSOU), LCOEFF(MAXSOU), RIF
      LOGICAL   DOSWNT, DOFQSL, IS1934
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALCO, XVELTY(2),
     *   XVELDE(2), XOPTY, XCODTY
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, CLCODE*4, VELTP*8,
     *   VELDF*8, OPTYPE*4, SOUNAM(MAXSOU)*16, NEWCOD*4
      REAL      XSEQIN, XDISKI, XQUAL, XBIF, XEIF, ZEROSP(5), SPECI(4),
     *   XNIF, SYSV, RESTF(2), XFQID, APARM(10),
     *   PRIMFL(MAXIF,MAXSOU), PRIMFE(MAXIF,MAXSOU), PRIMSP(4,MAXSOU),
     *   PRIMFO(MAXIF,MAXSOU), PRIMEO(MAXIF,MAXSOU), VELS(MAXIF,MAXSOU)
      DOUBLE PRECISION FRCALC(MAXIF)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XXSOUR, XQUAL,
     *   XCALCO, XBIF, XEIF, ZEROSP, SPECI, XOPTY, XCODTY, XNIF, SYSV,
     *   RESTF, XVELTY, XVELDE, XFQID, APARM
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, CLCODE, VELTP, VELDF,
     *   OPTYPE, SOUNAM, NEWCOD
      COMMON /EDTINF/ FRCALC, PRIMFL, PRIMFO, DOSWNT, DOFQSL, NSOUWD,
     *   SEQIN, DISKIN, OLDCNO, IVER, EIF, BIF, FREQID, SQUAL, NSOURC,
     *   PRIMFE, PRIMEO, ICOEFF, PRIMSP, IS1934, VELS, JCOEFF, LCOEFF,
     *   RIF
LOCAL END
      PROGRAM SETJY
C-----------------------------------------------------------------------
C! Enters source flux densities in a SU (source) table.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2004-2007, 2009-2016
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
C  SETJY enters flux densities in an SU table.
C   ADVERBS:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    SOURCES     The list of sources
C    BIF         Low IF # for flux density.
C    EIF         High IF # for flux density.
C    ZEROSP      I,Q,U,V flux density
C    CLCODE      Calibrator code
C    SYSVEL      Velocity of source (m/s) at pixel defined
C                by APARM(1)
C    RESTFREQ    Line rest frequency (Hz)
C    VELTP       Velocity type 'LSR,'HELIO'
C    VELDF       Velocity defination 'RADIO' or 'OPTICAL'
C    APARM       1: pixel to which SYSVEL refers (0 => 1)
C    APARM       2: epoch of flux scale (LEQ 0 => 1999.2, 1 => Baars,
C                  2 => 1995.2, GEQ 3 => 1990)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, BUFFER(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SETJY '/
C-----------------------------------------------------------------------
C                                       Initialize
      CALL SETJIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Edit tables
      CALL SETJ (IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Add history to output
      CALL STJHIS
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SETJIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   Initialization subroutine for SETJY
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C                      4 => user routine detected error.
C                      5 => catalog troubles
C                      8 => can't start
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER TYPE*2, STAT*4
      INTEGER   BUFF1(256), NPARM, IROUND, IERR, I, LUN
      LOGICAL   MATCH
      REAL      SELBAN
      DOUBLE PRECISION SELFRQ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE., BUFF1)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 157
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      FREQID = IROUND (XFQID)
      SQUAL = IROUND (XQUAL)
      IS1934 = .FALSE.
      RIF = IROUND (XNIF)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (8, 1, XVELTY, VELTP)
      CALL H2CHR (8, 1, XVELDE, VELDF)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
C                                       cal code
      CALL H2CHR (4, 1, XCALCO, CLCODE)
      CALL H2CHR (4, 1, XCODTY, NEWCOD)
      IF (NEWCOD(1:1).EQ.'*') THEN
         MSGTXT = 'CODETYPE = ''' // CLCODE // ' ILLEGAL'
         IRET = 1
         GO TO 990
         END IF
C                                       Coefficient set to
C                                       use for flux density
C                                       calculation
      ICOEFF = IROUND (APARM(2))
      ICOEFF = MAX (-1, MIN (6, ICOEFF))
C                                       Find input
      OLDCNO = 1
      TYPE = '  '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, TYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'WRIT', BUFF1, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Fill UVH.INC
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       APARM(4) default
      IF (APARM(4).EQ.0.0) APARM(4) = (1 + CATBLK(KINAX+JLOCF)) / 2.0
      CALL DFILL (MAXIF, 0.0D0, FRCALC)
C                                       Check to see if fq entries
      DOFQSL = ILOCFQ.GT.0
      IF (DOFQSL) THEN
         IF (FREQID.EQ.0) FREQID = -1
         SELBAN = 0.0
         SELFRQ = 0.D0
         LUN = 28
C                                       Give warning message if FREQID
C                                       -1 since should specify FREQID
C                                       for mode CALC, else frequency
C                                       may be wrong if
C                                       desired frequency not ref. freq
         IF (FREQID.LE.0 .AND. OPTYPE.EQ.'CALC') THEN
            WRITE (MSGTXT,1055) FREQID
            CALL MSGWRT (6)
            MSGTXT = '     FREQID WILL BE RESET TO 1, CHECK YOUR' //
     *         ' RESULTS CAREFULLY'
            CALL MSGWRT (6)
            END IF
C
         CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ,
     *      MATCH, FREQID, IRET)
         IF (.NOT.MATCH) THEN
            MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS' //
     *         ' - CHECK INPUTS'
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',I3,
     *   ' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' FILLING UVH.INC')
 1055 FORMAT ('**WARNING: OPCODE=CALC AND FREQID = ',I4)
      END
      SUBROUTINE SETJ (IRET)
C-----------------------------------------------------------------------
C   Subroutine to add flux densities and calcode to source table.
C   Output:
C      IRET   I  Return code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYWRD(2)*8, COMPAR(2)*4
      INTEGER   BUFFER(768), SUKOLS(MAXSUC), SUNUMV(MAXSUC), NUMIF, J,
     *   LUN1, LUN2, LUN3, IIF, JERR, SOUWAN(30), LSIGN, CVER, LOOP,
     *   BUFF2(768), KEYLOC(2), KEYVAL(8), KEYTYP(2), NFRQ, ISURNO,
     *   NDONE, BUFFX(1024), ISBAND(MAXIF), REFANT
      LOGICAL   NOFLUX, HELO
      REAL      ALTRFP(MAXIF), REFPIX, FINC(MAXIF)
      DOUBLE PRECISION NUX(MAXIF), FOFF(MAXIF), TFREQ, SPX, SPX1,
     *   REFFRQ, DREST
C                                       DSOU.INC vars no in common
      INTEGER   IDSOUR, QUAL, SUFQID, ID(3), IDNUM
      REAL      FLUX(4,MAXIF), DD
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF), RESTFQ(MAXIF), PMRA,
     *   PMDEC, FRQREF, FRQDIF
      CHARACTER CALCOD*4, VELTYP*8, VELDEF*8, BNDCOD(MAXIF)*8, DATE*8
C
      INCLUDE 'SETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA KEYWRD /'VELDEF  ', 'VELTYP  '/
      DATA LUN1, LUN2, LUN3 /77,78,79/
      DATA COMPAR /'RADI','OPTI'/
C-----------------------------------------------------------------------
C                                       If invalid OPTYPE, exit
      IF((OPTYPE.NE.'RESE') .AND. (OPTYPE.NE.'REJY') .AND.
     *   (OPTYPE.NE.'CALC') .AND. (OPTYPE.NE.'REVL') .AND.
     *   (OPTYPE.NE.' ') .AND. (OPTYPE.NE.'SPEC') .AND.
     *   (OPTYPE.NE.'VCAL') .AND. (OPTYPE.NE.'VANT')) THEN
         IRET = 1
         MSGTXT = 'INVALID OPTYPE = ''' // OPTYPE // ''' - TRY AGAIN'
         GO TO 990
         END IF
C                                       See if fluxes to be changed
      IF ((OPTYPE.EQ.'RESE') .OR. (OPTYPE.EQ.'REJY') .OR.
     *   (OPTYPE.EQ.'REVL') .OR. (OPTYPE.EQ.'CALC') .OR.
     *   (OPTYPE.EQ.'VCAL') .OR. (OPTYPE.EQ.'VANT'))
     *   CALL RFILL (4, 0.0, ZEROSP)
      NOFLUX = (ABS (ZEROSP(1)).LE.1.0E-20) .AND.
     *   (ABS (ZEROSP(2)).LE.1.0E-20) .AND. (ABS (ZEROSP(3)).LE.1.0E-20)
     *   .AND. (ABS (ZEROSP(4)).LE.1.0E-20)
      IF (NOFLUX) CALL RFILL (4, 0.0, SPECI)
C                                       Reformat table if necessary
      CALL FMTSOU (DISKIN, OLDCNO, CATBLK, LUN1, LUN2, BUFFER, BUFF2,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch source list
      CALL FNDSOU (DISKIN, OLDCNO, XSOUR, BUFFER, NSOUWD, DOSWNT,
     *   SOUWAN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Fill frequency table
      CVER = 1
      IF ((DOFQSL) .AND. (FREQID.LT.0)) THEN
         WRITE (MSGTXT,2000) FREQID
         IRET = 1
         GO TO 990
         END IF
      CALL CHNDAT ('READ', BUFFER, DISKIN, OLDCNO, CVER, CATBLK,
     *   LUN3, NUMIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      REFFRQ = CATD(KDCRV+JLOCF)
      IF ((ICOEFF.EQ.0) .AND. (REFFRQ.LE.5.0D8)) ICOEFF = -1
      CALL FILL (MAXSOU, ICOEFF+1, JCOEFF)
      CALL FILL (MAXSOU, ICOEFF, LCOEFF)
C                                       Open SU table
C                                       First read it to get current
C                                       values
      CALL SOUINI ('READ', BUFFER, DISKIN, OLDCNO, 1, CATBLK, LUN1,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
C                                       Then write to insert new
C                                       values.
      CALL SOUINI ('WRIT', BUFFER, DISKIN, OLDCNO, 1, CATBLK, LUN1,
     *   NUMIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPNING SOURCE TABLE'
         GO TO 990
         END IF
C                                       Set EIF, BIF defaults
      BIF = MAX (1, MIN (BIF, NUMIF))
      IF (BIF.GT.EIF) EIF = NUMIF
      EIF = MAX (1, MIN (EIF, NUMIF))
      IF ((RIF.LT.BIF) .OR. (RIF.GT.EIF)) RIF = BIF
C                                       Update KEYWORDS if necessary
C                                       Velocity definition
      LSIGN = 1
      IF (VELDF.EQ.' ') VELDF = VELDEF
      IF (VELDF(1:4).EQ.COMPAR(1)) LSIGN = -1
      IF (VELDF(1:4).EQ.COMPAR(2)) LSIGN = 1
      IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC') .OR.
     *   (OPTYPE.EQ.'VCAL') .OR. (OPTYPE.EQ.'VANT')) THEN
         IF (VELDF.EQ.' ') VELDF = VELDEF
         VELDEF = VELDF
         KEYLOC(1) = 1
         KEYTYP(1) = 3
         IF (VELDEF.EQ.' ') VELDEF = 'OPTICAL'
         CALL CHR2H (8, VELDEF, 1, KEYVAL(KEYLOC(1)))
C                                       Velocity type
         IF (VELTP.EQ.' ') VELTP = VELTYP
         VELTYP = VELTP
         KEYLOC(2) = KEYLOC(1) + 2
         KEYTYP(2) = 3
         IF (VELTYP.EQ.' ') VELTYP = 'LSR'
         HELO = (VELTYP(1:5).EQ.'HELIO') .OR. (VELTYP(1:5).EQ.'BARYC')
         CALL CHR2H (8, VELTYP, 1, KEYVAL(KEYLOC(2)))
C                                       Write keywords
         CALL TABKEY ('WRIT', KEYWRD, 2, BUFFER, KEYLOC, KEYVAL, KEYTYP,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         VELDF = ' '
         VELTP = ' '
         END IF
C                                       Get number of channels
      NFRQ = CATBLK(KINAX+JLOCF)
C                                       Get number of sources.
      NSOURC = BUFFER(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 800
C                                       Check if too many
      IF (NSOURC.GT.MAXSOU) THEN
         WRITE (MSGTXT,1005) NSOURC, MAXSOU
         CALL MSGWRT (6)
         NSOURC = MAXSOU
         END IF
C                                       Loop through records
      NDONE = 0
      REFPIX = CATR(KRCRP+JLOCF)
      IIF = MAXIF * NSOURC
      CALL RFILL (IIF, 0.0, PRIMFL)
      CALL RFILL (IIF, -9129.1, VELS)
      DO 500 LOOP = 1,NSOURC
C                                       Read record
         ISURNO = LOOP
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *      SOUNAM(LOOP), QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *      RESTFQ, PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, 'READ'
            GO TO 990
            END IF
C                                       Check qualifier
         IF ((QUAL.NE.SQUAL) .AND. (SQUAL.NE.-1)) GO TO 500
C                                       check CALCODE
         IF (CLCODE.NE.' ') THEN
            IF (CLCODE.EQ.'*') THEN
               IF (CALCOD.EQ.' ') GO TO 500
            ELSE IF (CLCODE.EQ.'-CAL') THEN
               IF (CALCOD.NE.' ') GO TO 500
            ELSE
               IF (CALCOD.NE.CLCODE) GO TO 500
               END IF
            END IF
C                                       Search lists
         IF (NSOUWD.GT.0) THEN
            DO 80 J = 1,NSOUWD
               IF ((IDSOUR.EQ.SOUWAN(J)) .AND. DOSWNT) GO TO 200
               IF ((IDSOUR.EQ.SOUWAN(J)) .AND. (.NOT.DOSWNT)) GO TO 500
 80            CONTINUE
C                                       Not in list - see if OK
            IF (DOSWNT) GO TO 500
            END IF
C                                       Process source.
C                                       Check if fluxes wanted
 200     IF (APARM(1).EQ.0) APARM(1) = 1
         NDONE = NDONE + 1
         IF ((APARM(3).GT.0.0) .AND. (APARM(3).LE.2.0) .AND.
     *      (APARM(3).NE.1.0)) THEN
            WRITE (MSGTXT,1200) APARM(3)
            CALL MSGWRT (5)
         ELSE
            APARM(3) = 1.0
            END IF
         IF (SOUNAM(LOOP)(:7).EQ.'1934-638') IS1934 = .TRUE.
         IF (OPTYPE.EQ.'CALC') THEN
            CALL H2CHR (8, 1, CATH(KHDOB), DATE)
            CALL DATEST (DATE, ID)
            CALL DAYNUM (ID(1), ID(3), ID(2), IDNUM)
            DD = ID(1) + IDNUM/365.25
            END IF
         DO 250 IIF = BIF,EIF
C                                       Calculate flux densities for
C                                       known calibrators.
            IF (OPTYPE.EQ.'CALC') THEN
               TFREQ = REFFRQ + FOFF(IIF) + FREQO(IIF) +
     *            FINC(IIF) * (APARM(4) - REFPIX)
               FRCALC(IIF) = TFREQ
               CALL GETFLX (1, TFREQ, DD, SOUNAM(LOOP), JCOEFF(LOOP),
     *            PRIMSP(1,LOOP), PRIMFO(IIF,LOOP), PRIMEO(IIF,LOOP),
     *            JERR)
               CALL GETFLX (0, TFREQ, DD, SOUNAM(LOOP), LCOEFF(LOOP),
     *            PRIMSP(1,LOOP), PRIMFL(IIF,LOOP), PRIMFE(IIF,LOOP),
     *            JERR)
               PRIMFO(IIF,LOOP) = PRIMFO(IIF,LOOP) * APARM(3)
               PRIMEO(IIF,LOOP) = PRIMEO(IIF,LOOP) * APARM(3)
               PRIMFL(IIF,LOOP) = PRIMFL(IIF,LOOP) * APARM(3)
               PRIMFE(IIF,LOOP) = PRIMFE(IIF,LOOP) * APARM(3)
               IF (JERR.NE.0) GO TO 500
               FLUX(1,IIF) = PRIMFL(IIF,LOOP)
               END IF
C                                       Use user supplied values
            IF (.NOT.NOFLUX) THEN
               SPX = 1.0D0
               IF (SPECI(1).NE.0.0) THEN
                  TFREQ = LOG10 ((REFFRQ + FOFF(IIF) + FREQO(IIF))/1.D9)
                  SPX = TFREQ * (SPECI(1) + TFREQ * (SPECI(2) + TFREQ *
     *               (SPECI(3) + TFREQ * SPECI(4))))
                  SPX = 10.0D0 ** SPX
                  IF (OPTYPE.NE.'SPEC') THEN
                     TFREQ = LOG10 ((REFFRQ + FOFF(BIF) + FREQO(BIF))
     *                  /1.D9)
                     SPX1 = TFREQ * (SPECI(1) + TFREQ * (SPECI(2) +
     *                  TFREQ *  (SPECI(3) + TFREQ * SPECI(4))))
                     SPX1 = 10.0D0 ** SPX1
                     SPX = SPX / SPX1
                     END IF
                  END IF
               IF (ZEROSP(1).GE.0.0) FLUX(1,IIF) = ZEROSP(1) * SPX
               FLUX(2,IIF) = ZEROSP(2) * SPX
               FLUX(3,IIF) = ZEROSP(3) * SPX
               FLUX(4,IIF) = ZEROSP(4) * SPX
               END IF
C                                       Reset to zero
            IF ((OPTYPE.EQ.'RESE') .OR. (OPTYPE.EQ.'REJY')) THEN
               FLUX(1,IIF) = 0.0
               FLUX(2,IIF) = 0.0
               FLUX(3,IIF) = 0.0
               FLUX(4,IIF) = 0.0
               END IF
C                                       Velocity info.
            IF ((OPTYPE.EQ.'RESE') .OR. (OPTYPE.EQ.'REVL')) THEN
               LSRVEL(IIF) = 0.D0
               RESTFQ(IIF) = 0.D0
C                                       calculate it
            ELSE IF ((OPTYPE.EQ.'VCAL') .OR. (OPTYPE.EQ.'VANT')) THEN
               SYSV = 0.0
               CATR(KRARP) = REFPIX
               NUX(IIF) = REFFRQ + FOFF(IIF) + FREQO(IIF)
               CALL POPSRD ('R2D', RESTF, DREST)
               IF (DREST.GT.1.0E-10) RESTFQ(IIF) = DREST
               IF (OPTYPE.EQ.'VANT') THEN
                  REFANT = APARM(5) + 0.1
                  REFANT = MAX (1, REFANT)
               ELSE
                  REFANT = 0
                  END IF
               CALL GETV (DISKIN, OLDCNO, IDSOUR, IIF, LSIGN, HELO,
     *            REFANT, NUX(IIF), RESTFQ(IIF), LSRVEL(IIF), IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'GETTING VELOCITY'
                  GO TO 990
                  END IF
               VELS(IIF,LOOP) = LSRVEL(IIF)
C
            ELSE IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC')) THEN
               CATR(KRARP) = REFPIX
               LSRVEL(IIF) = SYSV * 1.0D3
               CALL POPSRD ('R2D', RESTF, DREST)
               IF (DREST.GT.1.0E-10) RESTFQ(IIF) = DREST
C
               ALTRFP(IIF) = APARM(1)
C                                       Calculate signed freq. inc
               IF (ISBAND(IIF).EQ.0) ISBAND(IIF) = 1
C
               FRQREF = (APARM(1)-REFPIX) * FINC(RIF) +
     *            REFFRQ + FOFF(RIF) + FREQO(RIF)
               NUX(IIF) = REFFRQ + FOFF(IIF) + FREQO(IIF)
C                                       optical - non linear
               IF (LSIGN.GT.0) THEN
C                 LSRVEL(IIF) = LSRVEL(IIF) + VELITE * RESTFQ(IIF) *
C    *               (1.0D0/FRQREF - 1.0D0/NUX(IIF))
                  FRQDIF = RESTFQ(IIF) * (1.0D0/NUX(IIF) - 1.0D0/FRQREF)
                  LSRVEL(IIF) = LSRVEL(IIF) + VELITE * FRQDIF
C                                       radio is linear
               ELSE
C                 LSRVEL(IIF) = VELITE / RESTFQ(IIF) * (NUX(IIF)-FRQREF)
C    *               + LSRVEL(IIF)
                  FRQDIF = NUX(IIF) - FRQREF
                  LSRVEL(IIF) = - FRQDIF / RESTFQ(IIF) * VELITE +
     *               LSRVEL(IIF)
                  END IF
               END IF
 250        CONTINUE
C                                       Calcode
         IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC')) THEN
            IF (NEWCOD.NE.' ') CALCOD = NEWCOD
            IF (NEWCOD.EQ.'----') CALCOD = ' '
            END IF
C                                       rewrite record
         ISURNO = LOOP
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *      SOUNAM(LOOP), QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *      RESTFQ, PMRA, PMDEC, IRET)
C                                       Check error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, 'WRIT'
            GO TO 990
            END IF
 500     CONTINUE
C                                       Done
      IF (NDONE.EQ.0) THEN
         IRET = 1
         MSGTXT = 'NO SOURCES MET SELECTION CRITERA - CHECK INPUTS'
         GO TO 990
         END IF
      CALL CATIO ('UPDT', DISKIN, OLDCNO, CATBLK, 'REST', BUFFX, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      GO TO 900
C                                       No SOURCE records
 800  IRET = 10
      MSGTXT = 'ERROR: SOURCE (SU) TABLE IS EMPTY'
      CALL MSGWRT (8)
C                                       Close file
 900  CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, JERR)
      IF (JERR.EQ.0) GO TO 999
      IRET = JERR
      WRITE (MSGTXT,1900) IRET
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' ON ',A)
 1005 FORMAT ('NUMBER SOURCES',I5,' EXCEEDS MAX',I5,
     *   ' DOING FIRST ONES ONLY')
 1010 FORMAT ('ERROR ',I3,' FILLING FREQ TABLE.')
 1020 FORMAT ('TABSOU ERROR ',I3,1X,A4,'ING SOURCE (SU) TABLE')
 1040 FORMAT ('SETJ: ERROR ',I3,' UPDATING CATALOGUE HEADER')
 1200 FORMAT ('All fluxes are scaled by a factor of',F7.4)
 1900 FORMAT ('TABIO ERROR',I3,' CLOSING SU TABLE - NOT UPDATED?')
 2000 FORMAT ('FREQID ',I3,' FORBIDDEN, USE A USEFUL VALUE - SEE HELP')
      END
      SUBROUTINE FMTSOU (DISK, CNO, CATBLK, LUN1, LUN2, BUFF1, BUFF2,
     *   IRET)
C-----------------------------------------------------------------------
C   Reformats SU table if necessary to include Line rest frequency
C   and other made March 1987. Works by copying to version 2 and then
C   back to version 1.
C   Input:
C      DISK        I    Disk number
C      CNO         I    Catalog number
C      CATBLK(256) I    Catalog header
C      LUN1,2      I    LUNs to use
C   Output:
C      BUFF1,BUFF2 I    I/O buffers
C      IRET        I    Return code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), LUN1, LUN2, BUFF1(768),
     *   BUFF2(768), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VT*8, VD*8, SOUNAM*16, CALCO*4
      INTEGER   SUKOLS(MAXSUC,2), SUNUMV(MAXSUC,2), NUMIF, IVER, OVER,
     *   IQUAL, IDSOUR, JERR, ISURNO, LOOP, NSOURC, SUFQID
      DOUBLE PRECISION   BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      REAL     FLUX(4,MAXIF)
      DOUBLE PRECISION   SYSVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IVER = 1
      OVER = 2
C                                       Open SU table
      CALL SOUINI ('READ', BUFF1, DISK, CNO, IVER, CATBLK, LUN1,
     *   NUMIF, VT, VD, SUFQID, ISURNO, SUKOLS(1,1), SUNUMV(1,1), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ'
         GO TO 990
         END IF
C                                       See if table to be reformatted
C                                       IF SUNUMV(17) .ne. NUMIF, or
C                                       SUFQID = -999.
      IF (SUFQID.EQ.-999) GO TO 10
      IF (SUNUMV(17,1).EQ.NUMIF) THEN
C                                       Close up and return.
         CALL TABIO ('CLOS', 0, ISURNO, BUFF1, BUFF1, JERR)
         IF (JERR.EQ.0) GO TO 999
         IRET = JERR
         WRITE (MSGTXT,1900) IRET
         GO TO 990
         END IF
C                                       Get number of sources.
 10   NSOURC = BUFF1(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 600
C                                       Open output table.
      SUFQID = -1
      CALL SOUINI ('WRIT', BUFF2, DISK, CNO, OVER, CATBLK, LUN2,
     *   NUMIF, VT, VD, SUFQID, ISURNO, SUKOLS(1,2), SUNUMV(1,2), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRIT'
         GO TO 990
         END IF
C                                       Copy file
      DO 500 LOOP = 1,NSOURC
C                                       Read record
         ISURNO = LOOP
         CALL TABSOU ('READ', BUFF1, ISURNO, SUKOLS(1,1), SUNUMV(1,1),
     *      IDSOUR, SOUNAM, IQUAL, CALCO, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, SYSVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, 'READ'
            GO TO 990
            END IF
C                                       rewrite record
         ISURNO = LOOP
         CALL TABSOU ('WRIT', BUFF2, ISURNO, SUKOLS(1,2), SUNUMV(1,2),
     *      IDSOUR, SOUNAM, IQUAL, CALCO, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, SYSVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
C                                       Check error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET, 'WRIT'
            GO TO 990
            END IF
 500     CONTINUE
C                                       Finished copy - close
 600  CALL TABIO ('CLOS', 0, ISURNO, BUFF1, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         IRET = JERR
         WRITE (MSGTXT,1900) IRET
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 0, ISURNO, BUFF2, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         IRET = JERR
         WRITE (MSGTXT,1900) IRET
         GO TO 990
         END IF
C                                       delete version 1
      CALL RMEXT (DISK, CNO, 'SU', IVER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy 2 back to 1
      CALL TABCOP ('SU', OVER, IVER, LUN1, LUN2, DISK, DISK, CNO, CNO,
     *   CATBLK, BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       delete version 2
      CALL RMEXT (DISK, CNO, 'SU', OVER, CATBLK, BUFF1, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' OPENING SU TABLE FOR ',A4)
 1020 FORMAT ('TABSOU ERROR ',I3,1X,A4,'ING SOURCE (SU) TABLE')
 1900 FORMAT ('TABIO ERROR',I3,' CLOSING SU TABLE - NOT UPDATED?')
      END
      SUBROUTINE STJHIS
C-----------------------------------------------------------------------
C   Add reference in output history
C   Input from common:
C      DISKIN     I    Disk number
C      OLDCNO     I    Catalog slot number
C      BIF        I    Low IF number
C      EIF        I    High IF number
C      ZEROSP(5)  R    Flux density array.
C      NEWCOD     C*4  Calcode
C      SYSV       R    Velocity
C      APARM(1)   R    Pixel to which SYSV refers
C      NSOUWD     I    Number of  sources specified
C      DOSWNT     L    If true listed sources wanted
C      XSOUR(*)   C*16 Source list
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, HISTR(12)*32
      INTEGER   BUFFER(1024), LUN1, IRET, TIME(3), DATE(3), I, IS, IIF,
     *   JTRIM, J
      REAL      TEMP, DMAX
      LOGICAL   NOFLUX, T
      DOUBLE PRECISION DT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
      DATA HISTR /'Scaife-Heald low freq',
     *   'Perley-Butler time dependent',
     *   'Perley-Butler (2013)',
     *   'Perley-Butler (2010)',
     *   'Taylor-Perley (1999.2)',
     *   'Perley (1995.2)',
     *   'Perley (1990)',
     *   'Baars',
     *   ' ', ' ', ' ',
     *   'Perley-Butler time independent'/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKIN, OLDCNO, BUFFER, IRET)
      IF (IRET.GT.2) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       OPTYPE
      IF (OPTYPE.NE.' ') THEN
         IF (OPTYPE.EQ.'CALC') THEN
            MSGTXT = '/ Flux calculated using known spectrum'
         ELSE IF (OPTYPE.EQ.'VCAL') THEN
            MSGTXT = '/ Calculating velocity of reference pixel'
         ELSE IF (OPTYPE.EQ.'VANT') THEN
            MSGTXT = '/ Calculating velocity of reference pixel'
         ELSE IF (OPTYPE.EQ.'RESE') THEN
            MSGTXT = '/ Resetting flux and source information to zero'
         ELSE IF (OPTYPE.EQ.'REJY') THEN
            MSGTXT = '/ Resetting flux information to zero'
         ELSE IF (OPTYPE.EQ.'REVL') THEN
            MSGTXT = '/ Resetting velocity information to zero'
         ELSE IF (OPTYPE.EQ.'SPEC') THEN
            MSGTXT = '/ Flux calculated with 1 GHz as reference'
            END IF
         HILINE = TSKNAM // MSGTXT
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       BIF, EIF
      WRITE (HILINE,1020) TSKNAM, BIF, EIF
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
      CALL HIADD (LUN1, HILINE, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 250
C                                       CALCODE
      IF (CLCODE.NE.' ') THEN
         WRITE (HILINE,1023) TSKNAM, CLCODE
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       QUAL
      IF (SQUAL.GE.0) THEN
         WRITE (HILINE,1022) TSKNAM, SQUAL
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       ZEROSP
C                                       See if fluxes to be changed
      NOFLUX = (ABS(ZEROSP(1)).LE.1.0E-20) .AND.
     *   (ABS(ZEROSP(2)).LE.1.0E-20) .AND. (ABS(ZEROSP(3)).LE.1.0E-20)
     *   .AND. (ABS(ZEROSP(4)).LE.1.0E-20)
C                                       CALC fluxes
      IF (OPTYPE.EQ.'CALC') THEN
         IF (APARM(3).NE.1.0) THEN
            WRITE (MSGTXT,1035) APARM(3)
            HILINE = TSKNAM // MSGTXT
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         WRITE (MSGTXT,1036) APARM(4)
         HILINE = TSKNAM // MSGTXT
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         IF (IS1934) THEN
            MSGTXT = 'Also using Reynolds (1994) for 1934-638'
            CALL MSGWRT (2)
            HILINE = TSKNAM // '/ ' // MSGTXT
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         DO 15 IIF = BIF,EIF
            IF (FRCALC(IIF).GT.0.0D0) THEN
               WRITE (MSGTXT,1031) IIF, FRCALC(IIF)/1.0D9
               HILINE = TSKNAM // MSGTXT
               CALL MSGWRT (2)
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               END IF
 15         CONTINUE
         DO 30 IS = 1,NSOURC
            IF ((PRIMFL(BIF,IS).GT.0.) .OR. (PRIMSP(1,IS).NE.0.0)) THEN
               J = JTRIM (HISTR(LCOEFF(IS)+2))
               MSGTXT = 'Using ' // HISTR(LCOEFF(IS)+2)(:J) //
     *            ' coefficients'
               CALL MSGWRT (2)
               HILINE = TSKNAM // '/ ' // MSGTXT
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               END IF
            DMAX = 0.0
            DO 20 IIF = BIF,EIF
C                                       If flux was calculated
               IF (PRIMFL(IIF,IS).GT.0) THEN
                  TEMP = (PRIMFL(IIF,IS) - PRIMFO(IIF,IS)) /
     *               PRIMFL(IIF,IS)
                  DMAX = MAX (DMAX, ABS(TEMP))
C                                       If error was known
                  IF (PRIMFE(IIF,IS).GT.0) THEN
                     WRITE (MSGTXT,1030) SOUNAM(IS), IIF,
     *                  PRIMFL(IIF,IS), PRIMFE(IIF,IS)
C                                       Else no error estimate
                  ELSE
                     WRITE (MSGTXT,1025) SOUNAM(IS), IIF,
     *                  PRIMFL(IIF,IS)
                     END IF
                  HILINE = TSKNAM // MSGTXT
                  CALL MSGWRT (2)
                  CALL HIADD (LUN1, HILINE, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 250
                  END IF
 20            CONTINUE
            IF (PRIMSP(1,IS).NE.0.0) THEN
               WRITE (HILINE,1055) TSKNAM, PRIMSP(1,IS)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               WRITE (HILINE,1056) TSKNAM, PRIMSP(2,IS),
     *            PRIMSP(3,IS), PRIMSP(4,IS)
               MSGTXT = HILINE(7:)
               CALL MSGWRT (2)
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               END IF
            DMAX = DMAX * 100.0
            IF (DMAX.GT.0.1) THEN
               MSGTXT = 'Previous coefficients ' //
     *            HISTR(JCOEFF(IS)+2)
               CALL MSGWRT (2)
               HILINE = TSKNAM // '/ ' // MSGTXT
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               WRITE (HILINE,1057) TSKNAM, DMAX
               MSGTXT = HILINE(9:)
               CALL MSGWRT (2)
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
               IF (DMAX.GT.3.0) THEN
                  DO 25 IIF = BIF,EIF
C                                       If flux was calculated
                     IF (PRIMFL(IIF,IS).GT.0.0) THEN
C                                       If error was known
                        IF (PRIMEO(IIF,IS).GT.0) THEN
                           WRITE (MSGTXT,1030) SOUNAM(IS), IIF,
     *                        PRIMFO(IIF,IS), PRIMEO(IIF,IS)
C                                       Else no error estimate
                        ELSE
                           WRITE (MSGTXT,1025) SOUNAM(IS), IIF,
     *                        PRIMFO(IIF,IS)
                           END IF
                        HILINE = TSKNAM // MSGTXT
                        CALL MSGWRT (2)
                        CALL HIADD (LUN1, HILINE, BUFFER, IRET)
                        IF (IRET.NE.0) GO TO 250
                        END IF
 25                  CONTINUE
                  END IF
               END IF
 30         CONTINUE
         IF (APARM(3).NE.1.0) THEN
            WRITE (MSGTXT,1035) APARM(3)
            CALL MSGWRT (2)
            HILINE = TSKNAM // MSGTXT
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
C                                       VCAL
      ELSE IF ((OPTYPE.EQ.'VCAL') .OR. (OPTYPE.EQ.'VANT')) THEN
         DO 40 IS = 1,NSOURC
            DO 35 IIF = BIF,EIF
C                                       If flux was calculated
               IF (ABS(VELS(IIF,IS)+9129.1).GT.0.1) THEN
                  WRITE (MSGTXT,1040) SOUNAM(IS), IIF,
     *               VELS(IIF,IS)/1000.0
                  HILINE = TSKNAM // MSGTXT
                  CALL MSGWRT (2)
                  CALL HIADD (LUN1, HILINE, BUFFER, IRET)
                  IF (IRET.NE.0) GO TO 250
                  END IF
 35            CONTINUE
 40         CONTINUE
         IF (OPTYPE.EQ.'VANT') THEN
            I = APARM(5) + 0.1
            I = MAX (I, 1)
            WRITE (MSGTXT,1041) I
         ELSE
            MSGTXT = '/ Velocities refer to the center of the Earth'
            END IF
         HILINE = TSKNAM // MSGTXT
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
C                                       Other modes sources/fluxes
      ELSE
C                                       All selected
         IF (NSOUWD.LE.0) THEN
            MSGTXT = 'SOURCES = ''''     /All sources selected'
            HILINE = TSKNAM // MSGTXT
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
C                                       list them
         ELSE
            DO 50 I = 1,NSOUWD,2
               IF (I.EQ.1) THEN
                  MSGTXT = 'SOURCES = ''' // XSOUR(I) // ''''
               ELSE
                  MSGTXT = ' '
                  MSGTXT(11:) = '''' // XSOUR(I) // ''''
                  END IF
               IF (NSOUWD.GT.I) THEN
                  MSGTXT(30:) = ', ''' // XSOUR(I+1) // ''''
                  IF (NSOUWD.GT.I+1) MSGTXT(51:) = ','
                  END IF
               IS = JTRIM (MSGTXT) + 3
               IF (DOSWNT) THEN
                  MSGTXT (IS:) = '/ Included'
               ELSE
                  MSGTXT (IS:) = '/ Excluded'
                  END IF
               HILINE = TSKNAM // MSGTXT
               CALL MSGWRT (2)
               CALL HIADD (LUN1, HILINE, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 250
 50            CONTINUE
            END IF
         IF (.NOT.NOFLUX) THEN
            WRITE (HILINE,1050) TSKNAM, ZEROSP(1), ZEROSP(2), ZEROSP(3),
     *         ZEROSP(4)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         END IF
C                                       spectral index parameters
      IF (SPECI(1).NE.0.0) THEN
         WRITE (HILINE,1055) TSKNAM, SPECI(1)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         WRITE (HILINE,1056) TSKNAM, SPECI(2), SPECI(3), SPECI(4)
         MSGTXT = HILINE(7:)
         CALL MSGWRT (2)
         CALL HIADD (LUN1, HILINE, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 250
         END IF
C                                       new calcode
      IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC')) THEN
         IF (NEWCOD.NE.' ') THEN
            IF (NEWCOD.EQ.'----') THEN
               WRITE (HILINE,1021) TSKNAM, '    '
            ELSE
               WRITE (HILINE,1021) TSKNAM, NEWCOD
               END IF
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         END IF
C                                       SYSVEL & Ref. pixel
      IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC') .OR.
     *   (OPTYPE.EQ.'VCAL') .OR. (OPTYPE.EQ.'VANT')) THEN
         IF ((OPTYPE.EQ.' ') .OR. (OPTYPE.EQ.'SPEC')) THEN
            WRITE (HILINE,1060) TSKNAM, SYSV
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            WRITE (HILINE,1061) TSKNAM, APARM(1)
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            WRITE (HILINE,1062) TSKNAM, RIF
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
C                                       Rest frequency
         CALL POPSRD ('R2D', RESTF, DT)
         IF (ABS(DT).GT.1.0E-20) THEN
            WRITE (HILINE,1065) TSKNAM, DT
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
C                                       Velocity type
         IF (VELTP.NE.' ') THEN
            WRITE (HILINE,1070) TSKNAM, VELTP
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
C                                       Velocity definition
         IF (VELDF.NE.' ') THEN
            WRITE (HILINE,1071) TSKNAM, VELDF
            MSGTXT = HILINE(7:)
            CALL MSGWRT (2)
            CALL HIADD (LUN1, HILINE, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 250
            END IF
         END IF
C                                       Close HI file
 250  CALL HICLOS (LUN1, T, BUFFER, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'release =''',A7,' ''  /********* Start ',A12,2X,A8)
 1020 FORMAT (A6,'BIF =',I3,' EIF =',I3,' / Range of IFs')
 1021 FORMAT (A6,'CODETYPE = ''',A4,'''   / NEW Calibrator code')
 1022 FORMAT (A6,'QUAL = ',I4,'   / Qualifier code in')
 1023 FORMAT (A6,'CALCODE = ''',A4,'''   / Calibrator code in')
 1025 FORMAT ('''',A,''' IF =',I3,' flux =',F7.4,
     *   ' (Jy calcd)')
 1030 FORMAT ('''',A,''' IF =',I3,' flux =',F7.4,' +/- ',F7.4,
     *   ' (Jy calcd)')
 1031 FORMAT ('FREQ(',I2,') =',F11.6,' GHz')
 1035 FORMAT ('APARM(3)=',F7.4,' /Factor that scales all fluxes')
 1036 FORMAT ('APARM(4)=',F9.2,' / channel for frequencies')
 1040 FORMAT ('''',A,''' IF =',I3,' velocity =',F11.2,
     *   ' (km/sec calcd)')
 1041 FORMAT ('APARM(5) =',I3,5X,'/ Velocity reference antenna')
 1050 FORMAT (A6,'ZEROSP =',4F10.3,' /I,Q,U,V fluxes')
 1055 FORMAT (A6,'SPECINDX=',F8.4,5X,' / spectral index')
 1056 FORMAT (A6,'SPECURVE=',3F9.4,' / spectral curvature')
 1057 FORMAT (A6,'/ FLUXES HAVE CHANGED since last version by <=',F6.1,
     *   ' percent')
 1060 FORMAT (A6,'SYSVEL =',F10.3,' / Velocity (km/s)')
 1061 FORMAT (A6,'APARM(1) =',F7.1,' / Sysvel ref pixel')
 1062 FORMAT (A6,'NIF =',I5,' / Sysvel ref IF')
 1065 FORMAT (A6,'RESTFREQ =',1PD15.8,'   /Rest frequency (Hz)')
 1070 FORMAT (A6,'VELTYP = ''',A8,''' /velocity type')
 1071 FORMAT (A6,'VELDEF = ''',A8,''' /velocity definition')
      END
      SUBROUTINE GETFLX (IT, TFREQ, DD, SOURCE, ICTYPE, SPECI, FLUX,
     *   FERR, IERR)
C-----------------------------------------------------------------------
C   Routine that uses the Baars et al formulae to calculate the
C   fluxes for 3C295, 3C286, 3C48 and 3C147.    1934-638 from R Duncan
C   Inputs:
C      IT       I     Is this current (0) or previous (1)
C      TFREQ    D     Frequency (Hz)
C      DD       R     Year (incl fraction)
C      SOURCE   C*8   Source for which to calculate flux
C      ICTYPE   I     = -1  Use Scaife and Heald low freq coeff
C                        0: Use Perley 2013 coeff.
C                        1: Use Perley 2010 coeff.
C                        2: Use Taylor 1999.2 coeff.
C                        3: Use Perley 1995.2 coeff.
C                        4: Use Perley 1990 coeff.
C                     >= 5: Use Baars coeff.
C                     1 becomes 2 on IT=1 for some sources
C                     >= 0 -> 10 for time independent sources
C   Outputs:
C      SPECI    R(4)  spectral index parameters
C      FLUX     R     Calculated fluxes.
C      FERR     R     Flux error, 0=> Not known
C      IERR     I     Error code, 0=OK, 1=>unknown source.
C
C Flux density, S, at frequency, nu, is calculated assuming
C
C   Log  (S) = F  and Log  (nu) = x, and
C      10                10
C
C   F  = [ a + b * x + c * x**2 + d * x**3]
C
C where  a, b, c and d are observed parameters.
C The error in F as a function of errors in a,b,c and d are
C
C    2      dF 2  2     dF 2  2     dF 2  2     dF 2  2
C   E   =  (--)  E   + (--)  E  +  (--)  E  +  (--)  E
C    F      da    a     db    b     dc    c     dd    d
C
C where the dF/da are partial derivatives.  So
C
C    2       2     2  2     4  2     6  2
C   E   =   E   + x  E  +  x  E  +  x  E
C    F       a        b        c        d
C
C The error in S is
C                                    F
C           dS             dS      10                F dF
C   E   =  (--)  E   and  (--) = d --  =  Log (10) 10  -- = S Log (10)
C    S      dF    F        dF      dF        e         dF        e
C so,
C   E   = S Log (10) * E
C    S         e        F
C-----------------------------------------------------------------------
      DOUBLE PRECISION TFREQ
      CHARACTER SOURCE*16
      REAL      DD, SPECI(4), FLUX, FERR
      INTEGER   IT, ICTYPE, IERR
C                                       XNSOU = number of recognized
C                                       source names. XSOUR actual numb.
C                                       of sources
      INTEGER   XNSOU, XSOUR, NDATES
      PARAMETER (XNSOU = 24, XSOUR=6, NDATES=17)
      INTEGER   SNSOU, SSOUR
      PARAMETER (SNSOU=9, SSOUR = 3)
C
      INTEGER   ISRC, KSOUNO(XNSOU), LENCHK(XNSOU), LOOP, IBAND, LCTYPE,
     *   I, LSRC, SENCHK(SNSOU)
      REAL      COEFF(4,XSOUR), NCOEFF(4,XSOUR), PCOEFF(4,XSOUR),
     *   RCOEFF(4,XSOUR), COERR(4,XSOUR), FBAND(7), TCOEFF(4,XSOUR),
     *   TCOEF3(4,XSOUR), W1, W2, DATES(NDATES), DCOEFF(4,NDATES,3),
     *   LCOEFF(5,XSOUR), LCOERR(5,XSOUR), SCOEF3(4,SSOUR)
      CHARACTER KNOSOU(XNSOU)*16, LNOSOU(XNSOU)*16, SNOSOU(SNSOU)*16
      DOUBLE PRECISION FREQ, TEMP1, TEMP2, DT, FERROR
      LOGICAL   DONMSG(4)
      INCLUDE 'INCS:DMSG.INC'
      SAVE DONMSG
C                                       Source lists, Baars et al.
C                                       3C286
      DATA COEFF /1.480, 0.292, -0.124, 0.000,
C                                       3C48
     *            2.345, 0.071, -0.138, 0.000,
C                                       3C147
     *            1.766, 0.447, -0.184, 0.000,
C                                       3C138
     *            2.009, -0.07176, -0.0862, 0.000,
C                                       1934-638
     *            -23.839, 19.569, -4.8168, 0.35836,
C                                       3C295
     *            1.485, 0.759, -0.255, 0.000/
C
C                                       3C286, Baars et al errors
      DATA COERR /0.018, 0.006, 0.001, 0.000,
C                                       3C48
     *            0.030, 0.001, 0.001, 0.000,
C                                       3C147
     *            0.017, 0.006, 0.001, 0.000,
C                                       3C138
     *            0.000, 0.000, 0.000, 0.000,
C                                       1934-638
     *            0.000, 0.000, 0.000, 0.000,
C                                       3C295
     *            0.013, 0.009, 0.001, 0.000/
C
C                                       Source lists, Taylor 1999.2
C                                       Note that the VLA coefficients
C                                       are for freq. in GHz, not MHz,
C                                       not true of 1934-638.  This
C                                       requires some modifications in
C                                       the calculation loop for RCOEFF.
C                                       3C286
      DATA RCOEFF /1.23734, -0.43276, -0.14223,  0.00345,
C                                       3C48
     *             1.31752, -0.74090, -0.16708,  0.01525,
C                                       3C147
     *             1.44856, -0.67252, -0.21124,  0.04077,
C                                       3C138
     *             1.00761, -0.55629, -0.11134, -0.01460,
C                                       1934-638 (Reynolds, 02/Jul/94)
     *            -30.7667,  26.4908,  -7.0977, 0.605334,
C                                       3C295
     *             1.46744, -0.77350, -0.25912,  0.00752/
C
C                                       Perley 2010 coefficients
C                                       same units as RCOEFF
C                                       3C286
      DATA TCOEFF /1.2361,  -0.4127,  -0.1864,   0.0294,
C                                       3C48
     *             1.3197,  -0.7253,  -0.2023,   0.0540,
C                                       3C147
     *             1.4428,  -0.6300,  -0.3142,   0.1032,
C                                       3C138
     *             1.0053,  -0.4384,  -0.1855,   0.0511,
C                                       1934-638 (Reynolds, 02/Jul/94)
     *           -30.7667,  26.4908,  -7.0977,   0.605334,
C                                       3C295
     *             1.4605,  -0.7043,  -0.3951,   0.0815 /
C                                       3C196
C    *             1.2753,  -0.7971,  -0.2255,   0.0380/
C
C                                       Perley 2013 coefficients
C                                       same units as RCOEFF
C                                       3C286
      DATA TCOEF3 /1.2515,  -0.4605,  -0.1715,   0.0336,
C                                       3C48 (2010)
     *             1.3197,  -0.7253,  -0.2023,   0.0540,
C                                       3C147 (2010)
     *             1.4428,  -0.6300,  -0.3142,   0.1032,
C                                       3C138 (2010)
     *             1.0053,  -0.4384,  -0.1855,   0.0511,
C                                       1934-638 (Reynolds, 02/Jul/94)
     *           -30.7667,  26.4908,  -7.0977,   0.605334,
C                                       3C295
     *             1.4866,  -0.7871,  -0.3440,   0.0749 /
C                                       3C196
C    *             1.2969,  -0.8690,  -0.1788,   0.0305/
C
C                                       steady sources Perley 2013
C                                       3C123
      DATA SCOEF3 / 1.8077, -0.8018, -0.1157, 0.0,
C                                       3C196
     *              1.2969, -0.8690, -0.1788, 0.0305,
C                                       3C295
     *              1.4866, -0.7871, -0.3440, 0.0749/
C                                       Source lists, Perley 1990
C                                       3C286, Perley 1990
      DATA NCOEFF /1.35899, 0.35990, -0.13338, 0.000,
C                                       3C48
     *            2.0868, 0.20889, -0.15498, 0.000,
C                                       3C147
     *            1.92641, 0.36072, -0.17389, 0.000,
C                                       3C138      (Baars again)
     *            2.009, -0.07176, -0.0862, 0.000,
C                                       1934-638 (Reynolds, 02/Jul/94)
     *            -30.7667, 26.4908, -7.0977, 0.605334,
C                                       3C295      (Baars again)
     *            1.485, 0.759, -0.255, 0.000/
C
C                                       Source lists, Perley 1995.2
C                                       3C286
      DATA PCOEFF /0.50344,  1.05026, -0.31666,  0.01602,
C                                       3C48
     *             1.16801,  1.07526, -0.42254,  0.02699,
C                                       3C147
     *             0.05702,  2.09340, -0.70760,  0.05477,
C                                       3C138
     *             1.97498, -0.23918,  0.01333, -0.01389,
C                                       1934-638 (Reynolds, 02/Jul/94)
     *            -30.7667,  26.4908,  -7.0977, 0.605334,
C                                       3C295
     *             1.28872,  0.94172, -0.31113,  0.00569/
C                                       Source lists: low freq
C                                       3C286
      DATA LCOEFF /27.477, -0.158,  0.032, -0.180,  0.000,
C                                       3C48
     *             64.768, -0.387, -0.420,  0.181,  0.000,
C                                       3C147
     *             66.738, -0.022, -1.012,  0.549,  0.000,
C                                       3C196
     *             83.084, -0.699,  0.110,  0.000,  0.000,
C                                       3c380
     *             77.352, -0.767,  0.000,  0.000,  0.000,
C                                       3C295
     *             97.763, -0.582, -0.298,  0.583, -0.363/
C                                       Error bars: low freq
C                                       3C286
      DATA LCOERR /0.746, 0.033, 0.043, 0.052, 0.000,
C                                       3C48
     *             1.761, 0.039, 0.031, 0.060, 0.000,
C                                       3C147
     *             2.490, 0.030, 0.167, 0.170, 0.000,
C                                       3C196
     *             1.862, 0.014, 0.024, 0.000, 0.000,
C                                       3C380
     *             1.164, 0.013, 0.000, 0.000, 0.000,
C                                       3C295
     *             2.787, 0.045, 0.085, 0.116, 0.137/
C                                       Source lists
      DATA KNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C138',   '0518+165', '0521+166', 'J0521+1638',
     *   '1934-638','1934-638', '1934-638', 'J1939-6342',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
      DATA LNOSOU /'3C286','1328+307','1331+305', 'J1331+3030',
     *   '3C48',    '0134+329', '0137+331', 'J0137+3309',
     *   '3C147',   '0538+498', '0542+498', 'J0542+4951',
     *   '3C196',   '0809+483', '0813+482', 'J0813+4813',
     *   '3C380',   '1828+487', '1829+487', 'J1829+4844',
     *   '3C295',   '1409+524', '1411+522', 'J1411+5212'/
      DATA SNOSOU /'3C123', '0433+295', 'J0437+2946',
     *   '3C196', '0809+483', 'J0813+4813',
     *   '3C295', '1409+524', 'J1411+5212'/
C                                       date list
      DATA DATES /1983.4, 1985.9, 1987.3, 1989.9, 1995.2, 1998.1,
     *   1999.3, 2000.8, 2001.9, 2003.1, 2004.7, 2006.0, 2007.4, 2008.7,
     *   2010.0, 2010.9, 2012.0/
C                                       3C48
      DATA DCOEFF /
     *   1.3339,-.7643,-.1946,.055,   1.3350,-.7598,-.1869,.057,
     *   1.3361,-.7577,-.1905,.048,   1.3363,-.7605,-.1965,.057,
     *   1.3359,-.7673,-.2041,.059,   1.3342,-.7732,-.2078,.065,
     *   1.3342,-.7682,-.2097,.056,   1.3323,-.7654,-.2091,.060,
     *   1.3342,-.7708,-.2014,.059,   1.3341,-.7691,-.2006,.057,
     *   1.3341,-.7641,-.2102,.059,   1.3335,-.7705,-.2008,.058,
     *   1.3335,-.7660,-.1982,.051,   1.3361,-.7700,-.2119,.076,
     *   1.3334,-.7662,-.1988,.062,   1.3332,-.7665,-.1980,.064,
     *   1.3324,-.7690,-.1950,.059,
C                                       3C147
     *   1.4620,-.7085,-.2347,.051,   1.4648,-.7177,-.2501,.089,
     *   1.4624,-.7115,-.2336,.071,   1.4646,-.7194,-.2532,.092,
     *   1.4632,-.7121,-.2346,.086,   1.4641,-.7090,-.2313,.088,
     *   1.4642,-.7132,-.2424,.082,   1.4585,-.7086,-.2296,.068,
     *   1.4636,-.7124,-.2426,.084,   1.4639,-.7144,-.2453,.082,
     *   1.4635,-.7112,-.2453,.091,   1.4631,-.7136,-.2338,.094,
     *   1.4645,-.7115,-.2378,.084,   1.4625,-.7112,-.2396,.081,
     *   1.4623,-.7139,-.2405,.081,   1.4607,-.7150,-.2372,.077,
     *   1.4616,-.7187,-.2424,.079,
C                                       3C138
     *   1.0328,-.5523,-.1161,.008,   1.0337,-.5591,-.1605,.032,
     *   1.0354,-.5914,-.1032,-.005,  1.0292,-.5636,-.1857,.052,
     *   1.0145,-.5466,-.1758,.038,   1.0259,-.5679,-.1735,.039,
     *   1.0204,-.5702,-.1636,.030,   1.0081,-.5077,-.2492,.064,
     *   1.0196,-.5627,-.1823,.039,   1.0177,-.5686,-.1591,.029,
     *   1.0094,-.5003,-.2642,.085,   1.0181,-.5543,-.1486,.038,
     *   1.0149,-.5408,-.1174,.012,   1.0132,-.4941,-.1556,.045,
     *   1.0230,-.4983,-.1529,.048,   1.0207,-.5140,-.1626,.058,
     *   1.0332,-.5608,-.1197,.041/
C                                       Number of characters to check
      DATA LENCHK /5,8,8,10,  4,8,8,10, 5,8,8,10,
     *   5,8,8,10, 8,8,8,10, 5,8,8,10/
      DATA SENCHK /5,8,9, 5,8,9, 5,8,9/
C                                       Source number in coef table
      DATA KSOUNO /1,1,1,1, 2,2,2,2, 3,3,3,3,
     *   4,4,4,4, 5,5,5,5, 6,6,6,6/
C                                       Frequency break points for bands
      DATA FBAND /0.15E3, 0.7E3, 2.0E3, 6.0E3, 11.5E3, 18.E3, 28.E3/
      DATA DONMSG /4*.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      CALL RFILL (4, 0.0, SPECI)
C                                       Frequency In MHz
      FREQ = TFREQ * 1.0D-6
      IBAND = 8
      DO 10 LOOP = 1,7
         IF (FREQ.LT.FBAND(8-LOOP)) IBAND = 8-LOOP
 10      CONTINUE
C                                       Find source
      ISRC = -1
      IF (ICTYPE.LT.0) THEN
         DO 20 LOOP = 1,XNSOU
            IF (SOURCE(1:LENCHK(LOOP)).EQ.LNOSOU(LOOP)(1:LENCHK(LOOP)))
     *         ISRC = KSOUNO(LOOP)
 20       CONTINUE
      ELSE
         DO 25 LOOP = 1,SNSOU
            IF (SOURCE(1:SENCHK(LOOP)).EQ.SNOSOU(LOOP)(1:SENCHK(LOOP)))
     *         ISRC = (LOOP - 1) / 3 + XSOUR + 1
 25         CONTINUE
         DO 30 LOOP = 1,XNSOU
            IF (SOURCE(1:LENCHK(LOOP)).EQ.KNOSOU(LOOP)(1:LENCHK(LOOP)))
     *         ISRC = KSOUNO(LOOP)
 30         CONTINUE
         END IF
C                                       steady sources
C                                       Perley 2013
      IF (ISRC.GT.XSOUR) THEN
         ICTYPE = 10
         ISRC = ISRC - XSOUR
         DT = LOG10 (FREQ)
         DT = DT - 3.0D0
         CALL RCOPY (3, SCOEF3(2,ISRC), SPECI)
         TEMP2 = SCOEF3(1,ISRC) + DT * (SCOEF3(2,ISRC) + DT *
     *      (SCOEF3(3,ISRC) + DT * SCOEF3(4,ISRC)))
         GO TO 190
         END IF
C                                       fix ref for some sources
C                                       HI writing depends on this
      IF ((IT.EQ.1) .AND. (ICTYPE.EQ.1) .AND. (ISRC.GE.2) .AND.
     *   (ISRC.LE.4)) ICTYPE = 2
      LCTYPE = ICTYPE
      IF ((IT.EQ.0) .AND. (ICTYPE.EQ.0) .AND. ((ISRC.LT.2) .OR.
     *   (ISRC.GT.4))) LCTYPE = 1
C                                       Complain if unknown
      IF (ISRC.LT.0) THEN
         MSGTXT = 'UNKNOWN FLUX CALIBRATOR: ' // SOURCE
         CALL MSGWRT (6)
         FLUX = 0.0
         IERR = 1
         GO TO 999
         END IF
C                                       message about model
      IF ((ISRC.LE.4) .AND. (.NOT.DONMSG(ISRC))) THEN
         MSGTXT = 'A source model for this calibrator is available'
         CALL MSGWRT (5)
         MSGTXT = 'Consult the help file for CALRD for assistance'
         CALL MSGWRT (5)
         DONMSG(ISRC) = .TRUE.
         END IF
C                                       Compute flux
      DT = LOG10 (FREQ)
C                                       Assume no error
      FERROR = 0.0
C                                       Low frequency
      IF (LCTYPE.LT.0) THEN
C                                       return wrt 1 GHz, not 150 MHz
         DT = LOG10 (1.0D3 / 150.0D0)
         SPECI(1) = LCOEFF(2,ISRC) + 2.D0*DT*LCOEFF(3,ISRC) +
     *      3.D0*DT*DT*LCOEFF(4,ISRC) + 4.D0*DT*DT*DT*LCOEFF(5,ISRC)
         SPECI(2) = LCOEFF(3,ISRC) + 3.D0*DT*LCOEFF(4,ISRC) +
     *      6.D0*DT*DT*LCOEFF(5,ISRC)
         SPECI(3) = LCOEFF(4,ISRC) + 4.D0*DT*LCOEFF(5,ISRC)
         SPECI(4) = LCOEFF(5,ISRC)
C                                       compute flux, error
         DT = LOG10 (FREQ/150.0D0)
         TEMP2 = DT * (LCOEFF(2,ISRC) + DT * (LCOEFF(3,ISRC) + DT *
     *      (LCOEFF(4,ISRC) + DT * LCOEFF(5,ISRC))))
         FLUX = LCOEFF(1,ISRC) * (10.D0 ** TEMP2)
         DT = DT * DT
         FERROR = LCOERR(1,ISRC)**2 + (LOG(10.0D0)**2) * DT *
     *      (LCOERR(2,ISRC)**2 + DT * (LCOERR(3,ISRC) + DT *
     *      (LCOERR(4,ISRC) + DT * LCOERR(5,ISRC))))
         FERROR = SQRT (FERROR) * (10.0D0 ** TEMP2)
         GO TO 210
C                                       Perley-Butler 2013 time
      ELSE IF (LCTYPE.EQ.0) THEN
         DT = DT - 3.0D0
         LSRC = ISRC - 1
         IF ((DD.LE.DATES(1)) .OR. (DD.GE.DATES(NDATES))) THEN
            I = NDATES
            IF (DD.LE.DATES(1)) I = 1
            CALL RCOPY (3, DCOEFF(2,I,LSRC), SPECI)
            TEMP2 = DCOEFF(1,I,LSRC) + DT * (DCOEFF(2,I,LSRC) + DT *
     *         (DCOEFF(3,I,LSRC) + DT * DCOEFF(4,I,LSRC)))
C                                       interpolate
         ELSE
            DO 110 I = 2,NDATES
               IF (DD.LT.DATES(I)) THEN
                  W1 = (DATES(I) - DD) / (DATES(I) - DATES(I-1))
                  W2 = 1.0 - W1
                  TEMP2 = DCOEFF(1,I,LSRC) + DT * (DCOEFF(2,I,LSRC) +
     *                DT * (DCOEFF(3,I,LSRC) + DT * DCOEFF(4,I,LSRC)))
                  TEMP1 = DCOEFF(1,I-1,LSRC) + DT * (DCOEFF(2,I-1,LSRC)
     *               + DT * (DCOEFF(3,I-1,LSRC) + DT *
     *               DCOEFF(4,I-1,LSRC)))
                  FLUX = W2 * (10.D0**TEMP2) + W1 * (10.D0**TEMP1)
                  SPECI(1) = W2*DCOEFF(2,I,LSRC) + W1*DCOEFF(2,I-1,LSRC)
                  SPECI(2) = W2*DCOEFF(3,I,LSRC) + W1*DCOEFF(3,I-1,LSRC)
                  SPECI(3) = W2*DCOEFF(4,I,LSRC) + W1*DCOEFF(4,I-1,LSRC)
                  GO TO 200
                  END IF
 110           CONTINUE
            END IF
C                                       Perley 2013
      ELSE IF (LCTYPE.EQ.1) THEN
         IF (ISRC.NE.5) THEN
            DT = DT - 3.0D0
            CALL RCOPY (3, TCOEF3(2,ISRC), SPECI)
            END IF
         TEMP2 = TCOEF3(1,ISRC) + DT * (TCOEF3(2,ISRC) + DT *
     *      (TCOEF3(3,ISRC) + DT * TCOEF3(4,ISRC)))
C                                       Perley 2010
      ELSE IF (LCTYPE.EQ.2) THEN
         IF (ISRC.NE.5) THEN
            DT = DT - 3.0D0
            CALL RCOPY (3, TCOEFF(2,ISRC), SPECI)
            END IF
         TEMP2 = TCOEFF(1,ISRC) + DT * (TCOEFF(2,ISRC) + DT *
     *      (TCOEFF(3,ISRC) + DT * TCOEFF(4,ISRC)))
C                                       Taylor 1999.2 &
C                                       Reynolds (1934-638)
      ELSE IF (LCTYPE.EQ.3) THEN
         IF (ISRC.NE.5) THEN
            DT = DT - 3.0D0
            CALL RCOPY (3, RCOEFF(2,ISRC), SPECI)
            END IF
         TEMP2 = RCOEFF(1,ISRC) + DT * (RCOEFF(2,ISRC) + DT *
     *      (RCOEFF(3,ISRC) + DT * RCOEFF(4,ISRC)))
C                                       Baars scale
      ELSE IF (LCTYPE.GE.6) THEN
         TEMP2 = COEFF(1,ISRC) + DT * (COEFF(2,ISRC) + DT *
     *      (COEFF(3,ISRC) + DT * COEFF(4,ISRC)))
C                                       Baars error, sum of squares
         FERROR =  (COERR(1,ISRC)**2) +
     *            ((COERR(2,ISRC)*DT)**2) +
     *            ((COERR(3,ISRC)*DT*DT)**2) +
     *            ((COERR(4,ISRC)*DT*DT*DT)**2)
C                                       Perley 1995.2 &
C                                       Reynolds (1934-638)
      ELSE IF (LCTYPE.EQ.4) THEN
         TEMP2 = PCOEFF(1,ISRC) + DT * (PCOEFF(2,ISRC) + DT *
     *      (PCOEFF(3,ISRC) + DT * PCOEFF(4,ISRC)))
C                                       Perley 1990 &
C                                       Reynolds (1934-638)
      ELSE IF (LCTYPE.EQ.5) THEN
         TEMP2 = NCOEFF(1,ISRC) + DT * (NCOEFF(2,ISRC) + DT *
     *      (NCOEFF(3,ISRC) + DT * NCOEFF(4,ISRC)))
         END IF
C
 190  FLUX = 10.0D0 ** TEMP2
C                                       If non-zero error exponent
 200  IF (FERROR.GT.0) THEN
C                                       sqrt sum of squares * factor
         FERROR = LOG(10.) * FLUX * SQRT(FERROR)
         END IF
C                                       Convert back to real
 210  FERR = FERROR
C
 999  RETURN
      END
      SUBROUTINE GETV (DISK, CNO, SID, IIF, LSIGN, HELO, REFANT, NUX,
     *   LRESTF, SYSVEL, IRET)
C-----------------------------------------------------------------------
C   Returns the source velocity at the first observation time in the
C   data set.
C   Inputs:
C      IIF      I   IF of this velocity, rest freq, etc
C      LSIGN    I   Velocity type sign for computation
C      NUX      D   Frequency of reference pixel this IF not counting
C                   any in the CL/FO table
C      LRESTF   D   Rest frequency
C      REFANT   I   Velcities are wrt this antenna: 0 -> Earth center
C   Output:
C      SYSVEL   D   Velocity
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SID, IIF, LSIGN, REFANT, IRET
      LOGICAL   HELO
      DOUBLE PRECISION NUX, LRESTF, SYSVEL
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, LUNTMP, IVER, TABUFF(512), INXRNO, NXKOLS(MAXNXC),
     *   NXNUMV(MAXNXC), IREC, NREC, ID, SUBA, VSTART, VEND, FQID,
     *   LSTSOU, GUSE, CLSUB, CLFQ, IANT, OLDSOU, I, NANT, IA1, IA2
      REAL      UT, TIME, DTIME
      LOGICAL   FIRST, SORT
      DOUBLE PRECISION CLFOFF(MAXIF), OBS, DOPVEL, ARRLON
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE FIRST, SORT, LSTSOU, CLFOFF, GUSE, IANT, DOPVEL, TIME, UT
      DATA FIRST, SORT, LSTSOU, GUSE, IANT /.TRUE., .FALSE., 0, 0, 0/
C-----------------------------------------------------------------------
C                                       first time: get ant, NX, ...
      IF (FIRST) THEN
         FIRST = .FALSE.
         LUN = LUNTMP (1)
C                                       time system, array center
         IVER = 1
         CALL ANTINI ('READ', TABUFF, DISK, CNO, IVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANFQID, ANTNIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE ANTENNA TABLE'
            GO TO 990
            END IF
         NANT = TABUFF(5)
         CALL TABIO ('CLOS', 1, IANRNO, TABUFF, TABUFF, IRET)
C                                       get antenna info (a la CVEL)
         IF (REFANT.GT.0) THEN
C                                       Fill AN information
C                                       into common in D/CANS.INC
            CALL GETANT (DISK, CNO, IVER, CATBLK, TABUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'GETTING ANTENNA INFO'
               GO TO 990
               END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
C                                       go to left-hand system
            ARRLON = 0.0D0
            IF ((ABS(CNTRX).GT.1.D2) .AND. (ABS(CNTRY).GT.1.D2) .AND.
     *         (ABS(CNTRZ).GT.1.D2)) ARRLON = ATAN2 (CNTRY, CNTRX)
            DO 10 I = 1, MAXANT
               ANTX(I) = CNTRX + STNX(I)*COS(ARRLON) -
     *            STNY(I)*SIN(ARRLON)
               ANTY(I) = CNTRY + STNY(I)*COS(ARRLON) +
     *            STNX(I)*SIN(ARRLON)
               ANTY(I) = -ANTY(I)
               ANTZ(I) = CNTRZ + STNZ(I)
 10            CONTINUE
            END IF
         END IF
C                                       find time first record
      IF (SID.NE.LSTSOU) THEN
         UT = 0.0
         TIME = 0.0
         CALL FNDEXT ('NX', CATBLK, IVER)
         IF (IVER.GT.0) THEN
            LUN = LUNTMP (1)
            IVER = 1
            CALL NDXINI ('READ', TABUFF, DISK, CNO, IVER, CATBLK, LUN,
     *         INXRNO, NXKOLS, NXNUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING THE INDEX TABLE'
               GO TO 990
               END IF
            NREC = TABUFF(5)
            DO 20 IREC = 1,NREC
               INXRNO = IREC
               CALL TABNDX ('READ', TABUFF, INXRNO, NXKOLS, NXNUMV,
     *            TIME, DTIME, ID, SUBA, VSTART, VEND, FQID, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING THE INDEX TABLE'
                  GO TO 990
                  END IF
               IF (ID.EQ.SID) THEN
                  TIME = TIME - DTIME/2.0
                  UT = TIME - (DATUTC/86400.0D0)
                  GO TO 25
                  END IF
 20            CONTINUE
 25         CALL TABIO ('CLOS', 1, IANRNO, TABUFF, TABUFF, IRET)
            END IF
C                                       CL/FO addition all IFs
         HELIO = HELO
         CALL DFILL (MAXIF, 0.0D0, CLFOFF)
         LSTSOU = SID
         CLFQ = 0
         CLSUB = 0
         IF (REFANT.GT.0) THEN
            IA1 = REFANT
            IA2 = REFANT
         ELSE
            IA1 = 1
            IA2 = NANT
            END IF
         DO 30 IANT = IA1,IA2
            CALL FRQUPD (DISK, CNO, GUSE, IANT, TIME, LSTSOU, CLSUB,
     *         CLFQ, CATBLK, SORT, CLFOFF, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING CL/FO TABLE OFFSETS'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               GO TO 40
               END IF
 30         CONTINUE
         IANT = MAX (1, REFANT)
 40      OLDSOU = -1
         CALL CVLDOP (DISK, CNO, UT, REFANT, LSTSOU, OLDSOU, LSTSOU,
     *      DOPVEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING DOPPLER VELOCITY'
            GO TO 990
            END IF
         END IF
C                                       velocity
      OBS = NUX + CLFOFF(IIF)
      IF (LSIGN.GT.0) THEN
         SYSVEL = VELITE * (LRESTF/OBS - 1.0D0) - DOPVEL
      ELSE
         SYSVEL = VELITE * (1.0D0 - OBS/LRESTF) - DOPVEL
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETV ERROR',I4,' ON ',A)
      END
