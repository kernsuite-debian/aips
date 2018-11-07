      SUBROUTINE DGHEAD
C-----------------------------------------------------------------------
C! Fills output CATBLK for UVGET
C# UV Calibration Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2006-2007, 2010, 2012, 2017-2018
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
C   Corrects CATBLK in common /MAPHDR/ to correspond to UVGET output
C   data.  If only one output source is specified then the information
C   about that source is filled in from the source file (if any).
C   The order of the regular axes is changed to reflect the order
C   used by DATGET.
C      For compressed input data the WEIGHT and SCALE ramdom parameters
C   are removed if they are the last two.
C   Inputs from common /SELCAL/:
C      BCHAN   I       First channel desired.
C      ECHAN   I       Last channel desired.
C      BIF     I       First IF desired.
C      EIF     I       Last IF desired.
C      PMODE   I       Polarizarion mode (see DGINIT for codes)
C                      0 => same Stokes' as in input.
C      NSOUWD  I       Number of sources specified.
C      DOSWNT  L       If true sources specified are included
C                      else excluded.
C      SOUWAN  I(30)   List or source numbers from source file.
C   Input/Output in common /MAPHDR/ (DSEL.INC):
C      CATBLK  I(256)  Uvdata catalog header record.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, OLDCTP(7)*8, CHWT*8, CHSCL*8,
     *   CHFQ*8, CTEMP*8, CHVEL(4)*4, BNDCOD(MAXIF)*8
      HOLLERITH CATH(256)
      INTEGER   IERR, I1, I2, ITEMP, LUNSF, TABUFF(512), KOLS(MAXSUC),
     *   NUMV(MAXSUC), JERR, INOGRP, NSSAV, INDEX, WTOFF, SCLOFF,
     *   FRQOFF, SRCOFF, I, NSOURC, NNIF, IVER, ISBAND(MAXIF), NRPS
      REAL      CATR(256), FINC(MAXIF)
      DOUBLE PRECISION CATD(128), FOFF(MAXIF)
      INTEGER   OLDNAX(7), AXPNT, NUMAX, NEXTAX
      LOGICAL   AXDONE(7), CMPRES, GOODRP, WASLIN
      REAL      OLDCRP(7), OLDROT(7), OLDCIC(7)
      DOUBLE PRECISION OLDCRV(7)
      INTEGER   ISURNO, SIDSOU, SQUAL, SUFQID
      CHARACTER SSNAME*16, SCALCO*4
      DOUBLE PRECISION SBANDW, SRAEPO, SDECEP, SEPOCH, SRAAPP, SDECAP,
     *   SPMRA, SPMDEC, SLSRVE(MAXIF), SLREST(MAXIF), SRAOBS, SDECOB
      REAL     SFLUX(4,MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA LUNSF /24/
      DATA CHWT, CHSCL, CHFQ /'WEIGHT','SCALE','FREQSEL'/
      DATA CHVEL /'LSR ','BARY','HELI','RADI'/
C-----------------------------------------------------------------------
C                                       Compressed data
      CMPRES = CATBLK(KINAX).EQ.1
      IF (CMPRES) THEN
C                                       Look for Weight and scale
         CALL AXEFND (8, CHWT, CATBLK(KIPCN), CATH(KHPTP), WTOFF, IERR)
         CALL AXEFND (8, CHSCL, CATBLK(KIPCN), CATH(KHPTP), SCLOFF,
     *      IERR)
C                                       Set Complex axis dim=3
         CATBLK(KINAX) = 3
C                                       Remove weight and scale rand
C                                       parms.
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+WTOFF*2))
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+SCLOFF*2))
         IF (CATBLK(KIPCN).EQ.(SCLOFF+1))
     *      CATBLK(KIPCN)= CATBLK(KIPCN) - 1
         IF (CATBLK(KIPCN).EQ.(WTOFF+1))
     *      CATBLK(KIPCN)= CATBLK(KIPCN) - 1
         END IF
C                                       Also check for FREQSEL random
C                                       parameter
      CALL AXEFND (8, CHFQ, CATBLK(KIPCN), CATH(KHPTP), FRQOFF, IERR)
      IF (IERR.EQ.0) THEN
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+FRQOFF*2))
         IF (CATBLK(KIPCN).EQ.(FRQOFF+1))
     *      CATBLK(KIPCN) = CATBLK(KIPCN) - 1
         END IF
C                                       Check if IF axis present.
      IF (JLOCIF.LT.0) GO TO 20
C                                       Update IF axis
         I1 = BIF
         I2 = EIF
         CATD(KDCRV+JLOCIF) = CATD(KDCRV+JLOCIF) + (I1-1) *
     *      CATR(KRCIC+JLOCIF)
         CATR(KRCIC+JLOCIF) = 1.0
         CATBLK(KINAX+JLOCIF) = I2 - I1 + 1
C                                       Update freq axis
 20   IF (JLOCF.LT.0) GO TO 40
C                                       Frequency
         IF (JLOCIF.GT.0) THEN
            IVER = 1
            CALL CHNDAT ('READ', TABUFF, IUDISK, IUCNO, IVER, CATUV,
     *         LUNSF, NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1000) JERR
               FOFF(1) = 0.D0
               CALL MSGWRT (6)
               JERR = 0
               END IF
            CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + FOFF(BIF)
            CATR(KRCIC+JLOCF) = FINC(BIF)
            END IF
         I1 = BCHAN
         I2 = ECHAN
         CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - I1 + 1
         ITEMP = I2 - I1 + 1
         CATBLK(KINAX+JLOCF) = MIN (ITEMP, CATBLK(KINAX+JLOCF))
C                                       Update polarization axis.
C                                       Number of pixels.
 40   NSSAV = CATBLK(KINAX+JLOCS)
      IF (PMODE.GT.0) CATBLK(KINAX+JLOCS) = 1
C                                       Exceptions
      IF ((PMODE.EQ.7) .OR. (PMODE.EQ.13) .OR. (PMODE.EQ.14) .OR.
     *   (PMODE.EQ.20) .OR. (PMODE.EQ.21))
     *   CATBLK(KINAX+JLOCS) = MIN (2, NSSAV)
      IF (PMODE.EQ.5) CATBLK(KINAX+JLOCS) = 3
      IF (PMODE.EQ.8) CATBLK(KINAX+JLOCS) = 2
      IF ((PMODE.EQ.6) .OR. (PMODE.EQ.15) .OR. (PMODE.EQ.22))
     *   CATBLK(KINAX+JLOCS) = 4
C                                       Coordinate increment.
      IF (PMODE.GT.0) CATR(KRCIC+JLOCS) = 1
      IF (PMODE.GE.9) CATR(KRCIC+JLOCS) = -1
      IF (PMODE.EQ.7) CATR(KRCIC+JLOCS) = 3
C                                       Coordinate ref. value.
      WASLIN = CATD(KDCRV+JLOCS).LE.-4.5D0
      IF ((DOPOL.GT.0) .AND. (WASLIN)) DOPOL = MOD (DOPOL,10) + 10
      IF (PMODE.GT.0) CATD(KDCRV+JLOCS) = 1.0D0
      IF (PMODE.EQ.2) CATD(KDCRV+JLOCS) = 4.0D0
      IF (PMODE.EQ.3) CATD(KDCRV+JLOCS) = 2.0D0
      IF (PMODE.EQ.8) CATD(KDCRV+JLOCS) = 2.0D0
      IF (PMODE.EQ.4) CATD(KDCRV+JLOCS) = 3.0D0
      IF (PMODE.GE.9) CATD(KDCRV+JLOCS) = -1.0D0
      IF (PMODE.EQ.10) CATD(KDCRV+JLOCS) = -2.0D0
      IF (PMODE.EQ.11) CATD(KDCRV+JLOCS) = -3.0D0
      IF (PMODE.EQ.12) CATD(KDCRV+JLOCS) = -4.0D0
      IF (PMODE.EQ.14) CATD(KDCRV+JLOCS) = -3.0D0
      IF (PMODE.GE.16) CATD(KDCRV+JLOCS) = -5.0D0
      IF (PMODE.EQ.17) CATD(KDCRV+JLOCS) = -6.0D0
      IF (PMODE.EQ.18) CATD(KDCRV+JLOCS) = -7.0D0
      IF (PMODE.EQ.19) CATD(KDCRV+JLOCS) = -8.0D0
      IF (PMODE.EQ.21) CATD(KDCRV+JLOCS) = -7.0D0
C                                       Polarization calibrating
C                                       XX,YY,XY,YX data? It will be
C                                       changed to RR,LL,RL and LR.
C      IF ((DOPOL.GT.0) .AND. (CATD(KDCRV+JLOCS).LT.-4.5D0))
C     *   CATD(KDCRV+JLOCS) = CATD(KDCRV+JLOCS) + 4.0D0
C                                       Warn if processing X-Y data
C                                       without poln. calibrating.
C                                       to Stokes Q U or V
      IF ((DOPOL.LE.0) .AND. (WASLIN)) THEN
         IF (CATD(KDCRV+JLOCS)+CATR(KRCIC+JLOCS)*(CATBLK(KINAX+JLOCS)-1)
     *      .GT.1.5D0) THEN
            MSGTXT = 'WARNING: uncalibrated linear polarization data' //
     *         ' may be a problem'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Clear extension info
      CALL CATCLR (CATBLK)
C                                       See if one source specified.
      CALL DFILL (MAXIF, 0.0D0, SFREQS)
      IF (.NOT.((NSOUWD.EQ.1).AND.DOSWNT)) GO TO 910
C                                       Get source info. from source
C                                       file.
C                                       Open file
         CALL SOUINI ('READ', TABUFF, IUDISK, IUCNO, 1, CATUV, LUNSF,
     *      INOGRP, VELTYP, VELDEF, SUFQID, I, KOLS, NUMV, JERR)
C                                       See if file exists.
         IF (JERR.NE.0) GO TO 910
C                                       Get number of sources.
         NSOURC = TABUFF(5)
         IF (NSOURC.LE.0) GO TO 900
C                                       Loop looking for source.
         DO 200 I = 1,NSOURC
            ISURNO = I
            CALL TABSOU ('READ', TABUFF, ISURNO, KOLS, NUMV, SIDSOU,
     *         SSNAME, SQUAL, SCALCO, SFLUX, SFREQS, SBANDW, SRAEPO,
     *         SDECEP, SEPOCH, SRAAPP, SDECAP, SRAOBS, SDECOB, SLSRVE,
     *         SLREST, SPMRA, SPMDEC, JERR)
            IF (JERR.GT.0) GO TO 900
            IF (SIDSOU.EQ.SOUWAN(1)) GO TO 210
 200     CONTINUE
C                                       Didn't find
         GO TO 900
C                                       Found - Name
 210     CALL CHR2H (8, SSNAME, 1, CATH(KHOBJ))
C                                       RA
         CATD(KDCRV+JLOCR) = SRAEPO
         CATD(KDORA) = SRAOBS
         CATR(KRCIC+JLOCR) = 1.0
C                                       Dec
         CATD(KDCRV+JLOCD) = SDECEP
         CATD(KDODE) = SDECOB
         CATR(KRCIC+JLOCD) = 1.0
C                                       Epoch
         CATR(KREPO) = SEPOCH
         CURSOU = SOUWAN(1)
C                                       Frequency offset
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + SFREQS(BIF)
C                                       velocity info
         CATD(KDRST) = SLREST(BIF)
         CATD(KDARV) = SLSRVE(BIF)
         CATR(KRARP) = CATR(KRCRP+JLOCF)
C                                       Velocity reference frame
         ITEMP = 3
         IF (VELTYP(1:4).EQ.CHVEL(1)) ITEMP = 1
         IF (VELTYP(1:4).EQ.CHVEL(2)) ITEMP = 2
         IF (VELTYP(1:4).EQ.CHVEL(3)) ITEMP = 2
         IF (VELDEF(1:4).EQ.CHVEL(4)) ITEMP = ITEMP + 256
         CATBLK(KIALT) = ITEMP
C                                       Close file
 900     CALL TABIO ('CLOS', 0, I, TABUFF, TABUFF, JERR)
C                                       Drop "SOURCE" random parm
 910  CALL AXEFND (8, 'SOURCE  ', CATBLK(KIPCN), CATH(KHPTP),
     *   SRCOFF, IERR)
C                                       single source
      IF (IERR.NE.0) THEN
         CATR(KRARP) = CATR(KRARP) - BCHAN + 1
C                                       multi -> single source
      ELSE IF ((NSOUWD.EQ.1).AND.DOSWNT) THEN
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+SRCOFF*2))
         IF (CATBLK(KIPCN).EQ.(SRCOFF+1))
     *      CATBLK(KIPCN) = CATBLK(KIPCN) - 1
         END IF
C                                       Tidy up random parameters
      NRPS = CATBLK(KIPCN)
      GOODRP = .FALSE.
      DO 915 I = NRPS,1,-1
         INDEX = KHPTP + 2 * (I-1)
         CALL H2CHR (8, 1, CATH(INDEX), CTEMP)
         IF ((CTEMP.NE.' ') .AND. (CTEMP.NE.'REMOVED '))
     *      GOODRP = .TRUE.
         IF (.NOT.GOODRP) THEN
            IF ((CTEMP.EQ.' ') .OR. (CTEMP.EQ.'REMOVED '))
     *         CATBLK(KIPCN) = CATBLK(KIPCN) - 1
            END IF
 915     CONTINUE
C                                       Reorder regular axes:
C                                       COMPLEX, STOKES, FREQ,
C                                       IF (if present), others
      NUMAX = CATBLK(KIDIM)
      DO 920 I = 1,NUMAX
         AXDONE(I) = .FALSE.
 920     CONTINUE
C                                       Save first axes
      NEXTAX = 0
      DO 930 I = 1,4
         AXPNT = 0
         IF (I.EQ.2) AXPNT = JLOCS
         IF (I.EQ.3) AXPNT = JLOCF
         IF (I.EQ.4) AXPNT = JLOCIF
         IF (AXPNT.GE.0) THEN
            NEXTAX = NEXTAX + 1
            AXDONE(1+AXPNT) = .TRUE.
            OLDNAX(NEXTAX) = CATBLK(KINAX+AXPNT)
            OLDCRP(NEXTAX) = CATR(KRCRP+AXPNT)
            OLDROT(NEXTAX) = CATR(KRCRT+AXPNT)
            OLDCIC(NEXTAX) = CATR(KRCIC+AXPNT)
            INDEX = KHCTP + AXPNT * 2
            CALL H2CHR (8, 1, CATH(INDEX), OLDCTP(NEXTAX))
            OLDCRV(NEXTAX) = CATD(KDCRV+AXPNT)
            END IF
 930     CONTINUE
C                                       Other axes
      DO 940 I = 1,NUMAX
         IF (.NOT.AXDONE(I)) THEN
            AXPNT = I - 1
            NEXTAX = NEXTAX + 1
            OLDNAX(NEXTAX) = CATBLK(KINAX+AXPNT)
            OLDCRP(NEXTAX) = CATR(KRCRP+AXPNT)
            OLDROT(NEXTAX) = CATR(KRCRT+AXPNT)
            OLDCIC(NEXTAX) = CATR(KRCIC+AXPNT)
            INDEX = KHCTP + AXPNT * 2
            CALL H2CHR (8, 1, CATH(INDEX), OLDCTP(NEXTAX))
            OLDCRV(NEXTAX) = CATD(KDCRV+AXPNT)
            END IF
 940     CONTINUE
C                                       Put 'em back in the right order
      DO 950 I = 1,NUMAX
         AXPNT = I - 1
         CATBLK(KINAX+AXPNT) = OLDNAX(I)
         CATR(KRCRP+AXPNT) = OLDCRP(I)
         CATR(KRCRT+AXPNT) = OLDROT(I)
         CATR(KRCIC+AXPNT) = OLDCIC(I)
         INDEX = KHCTP + AXPNT * 2
         CALL CHR2H (8, OLDCTP(I), 1, CATH(INDEX))
         CATD(KDCRV+AXPNT) = OLDCRV(I)
 950     CONTINUE
C                                       Count vis records
      CALL VISCNT (JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DGHEAD: ERROR ',I3,' RETURNED FROM CHNDAT')
      END
