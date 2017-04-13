      SUBROUTINE COPTAB (DISKIN, CNOIN, DISKOU, CNOOUT, IRET)
C-----------------------------------------------------------------------
C! copies tables when calibration has been applied
C# subroutine editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2009-2013
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
C   Updates tables for selection by IF etc
C   Inputs:
C      DISKIN   I   Input disk number
C      CONIN    I   Input catalog number
C      DISKOU   I   Output disk number
C      CNOOUT   I   Output catalog number
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C      SELIF L  Select IFs or not
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKOU, CNOOUT, IRET
C
      INCLUDE 'INCS:DSEL.INC'
      DOUBLE PRECISION CATD(128)
      REAL      CATR(256), FINC(MAXIF)
      LOGICAL   TABLE, EXIST, FITASC, SELIF, ISBAND(MAXIF), MULTI
      CHARACTER NOTTYP(21)*2, BNDCOD(MAXIF)*8
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, AN(50),
     *   NA, NNIF, I, FREQID, BUFF1(512), BUFF2(512), ISURNO, SIDSOU,
     *   SQUAL, SUFQID, NSOURC, INOGRP, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NONOT, BPOL, EPOL, IROUND, BVER
      CHARACTER VELTYP*8, VELDEF*8, SSNAME*16, SCALCO*4
      DOUBLE PRECISION SBANDW, SRAEPO, SDECEP, SEPOCH, SRAAPP, SDECAP,
     *   SPMRA, SPMDEC, SLSRVE(MAXIF), SFREQO(MAXIF), SLREST(MAXIF),
     *   TIME1, TIME2, FOFF(MAXIF), FQOFF, SRAOBS, SDECOB
      REAL     SFLUX(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATD, CATBLK)
      DATA LUN1, LUN2 /28, 29/
      DATA NONOT, NOTTYP /21, 'NX','FQ','CH','CL','SN','SU','FG','BP',
     *   'IM','CQ','PC','TY','GC','MC','WX','BL','AN','CP','PD','SY',
     *   'CD'/
C-----------------------------------------------------------------------
C                                       Single source now?
      MULTI = ILOCSU.GT.0
C                                       polarization
      IF (CATUV(KINAX+JLOCS).EQ.CATBLK(KINAX+JLOCS)) THEN
         BPOL = 1
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
      ELSE
         FINC(1) = CATD(KDCRV+JLOCS) + (1 - CATR(KRCRP+JLOCS)) *
     *      CATR(KRCIC+JLOCS)
         BPOL = ABS (IROUND (FINC(1)))
         EPOL = MIN (2, CATBLK(KINAX+JLOCS))
         EPOL = MAX (EPOL, BPOL)
         END IF
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKOU, CNOIN,
     *   CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FLGIHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       more complex tables
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
      NA = 0
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      TIME1 = TSTART - 0.0104D0
      TIME2 = TEND + 0.0104D0
      SELIF = (BIF.GT.1) .OR. (EIF.LT.CATUV(KINAX+KLOCIF)) .OR.
     *   (FRQSEL.GT.0)
      CALL FILL (50, 0, AN)
C                                       FQ table
      IF (JLOCIF.GT.0) THEN
         CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Multi to single source
         IF ((KLOCSU.GE.0) .AND. (ILOCSU.LT.0)) THEN
C                                       Open file
            CALL SOUINI ('READ', BUFF2, IUDISK, IUCNO, 1, CATUV, LUN1,
     *         INOGRP, VELTYP, VELDEF, SUFQID, I, KOLS, NUMV, IRET)
            IF (IRET.NE.0) GO TO 20
C                                       Get number of sources.
            NSOURC = BUFF2(5)
C                                       Loop looking for source.
            DO 10 I = 1,NSOURC
               ISURNO = I
               CALL TABSOU ('READ', BUFF2, ISURNO, KOLS, NUMV, SIDSOU,
     *            SSNAME, SQUAL, SCALCO, SFLUX, SFREQO, SBANDW, SRAEPO,
     *            SDECEP, SEPOCH, SRAAPP, SDECAP, SRAOBS, SDECOB,
     *            SLSRVE, SLREST, SPMRA, SPMDEC, IRET)
               IF (IRET.GT.0) CALL DFILL (MAXIF, 0.0D0, SFREQO)
               IF ((SIDSOU.EQ.SOUWAN(1)) .OR. (IRET.GT.0)) GO TO 15
 10            CONTINUE
C                                       Didn't find
            CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Close file
 15         CALL TABIO ('CLOS', 0, I, BUFF2, BUFF2, IRET)
            END IF
C                                       Read old
 20      VER = 1
         CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, VER, CATUV, LUN1,
     *      NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Fixup
         NNIF = EIF - BIF + 1
         FQOFF = FOFF(BIF)
C                                       force the first IF to zero
         DO 30 I = BIF,EIF
            FOFF(I) = FOFF(I) - FQOFF + (SFREQO(I) - SFREQO(BIF))
 30         CONTINUE
C                                       Output ref IF = 1
         CATD(KDCRV+JLOCIF) = 1.0D0
         CATR(KRCRP+JLOCIF) = 1.0
C                                       Rewrite new
         VER = 1
         FREQID = 1
         CALL CHNDAT ('WRIT', BUFF1, DISKOU, CNOOUT, VER, CATBLK, LUN1,
     *      NNIF, FOFF(BIF), ISBAND(BIF), FINC(BIF), BNDCOD(BIF),
     *      FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Reference frequency in AN table
C                                       IF selection
      CALL FNDEXT ('AN', CATUV, NVER)
      DO 100 VER = 1,NVER
         CALL ISTAB ('AN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, VER, CATUV, CATBLK, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Revise tables:
      OFQID = FRQSEL
C                                       BP tables
      IF (DOBAND.LE.0) THEN
         CALL FNDEXT ('BP', CATUV, NVER)
         DO 120 VER = 1,NVER
            CALL ISTAB ('BP', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL BPSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL,
     *         EPOL, BIF, EIF, BCHAN, ECHAN, TIME1, TIME2, OFQID, ISUB,
     *         JSUB, BUFF1, BUFF2, IRET)
            IF (IRET.GT.0) GO TO 999
 120        CONTINUE
         END IF
C                                       CL tables
      CALL FNDEXT ('CL', CATUV, NVER)
C                                       write FO table only
      IF (.NOT.MULTI) THEN
         VER = 1
         CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL CL2FO (DISKIN, CNOIN,
     *      VER, LUN1, CATUV, DISKOU, CNOOUT, LUN2, CATBLK, SOUWAN,
     *      BUFF1, BUFF2, IRET)
C                                       write null table
      ELSE IF (DOCAL) THEN
         VER = 1
         CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do NOT select on sources
         IF (EXIST .AND. (IERR.EQ.0)) CALL CLNULL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, 0, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         DO 140 VER = 1,NVER
            CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
C                                       do NOT select on sources
            IF (EXIST .AND. (IERR.EQ.0)) CALL CLSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL,
     *         EPOL, BIF, EIF, OFQID, TIME1, TIME2, 0, SOUWAN, AN, NA,
     *         ISUB, JSUB, BUFF1, BUFF2, IRET)
            IF (IRET.NE.0) GO TO 999
 140        CONTINUE
         END IF
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, AN, 0, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATUV, NVER)
      DO 150 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BIF, EIF, BCHAN,
     *      ECHAN, OFQID, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 150     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATUV, NVER)
      DO 160 VER = 1,NVER
         CALL ISTAB ('CQ', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       FG tables
      CALL FNDEXT ('FG', CATUV, NVER)
      IF ((FGVER.GT.0) .AND. (NVER.GT.0)) THEN
         BVER = FGVER + 1
         IF (NVER.LE.FGVER) THEN
            MSGTXT = 'WARNING: NO FG TABLES ARE COPIED SINCE HIGHEST'
     *         // ' WAS APPLIED'
            NVER = 0
         ELSE
            WRITE (MSGTXT,1160) BVER, NVER
            END IF
         CALL MSGWRT (6)
         END IF
      DO 180 VER = BVER,NVER
         CALL ISTAB ('FG', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0) .AND. (VER.GT.FGVER))
     *      CALL FGSEL (DISKIN, CNOIN, DISKOU, CNOOUT, VER, CATUV,
     *      CATBLK, LUN1, LUN2, BIF, EIF, BCHAN, ECHAN,  TIME1,
     *      TIME2, OFQID, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATUV, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATUV, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATUV, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATUV, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       PD tables
      CALL FNDEXT ('PD', CATUV, NVER)
      IF (DOPOL.LE.0) THEN
         DO 270 VER = 1,NVER
            CALL ISTAB ('PD', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL PDSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL,
     *         EPOL, BIF, EIF, BCHAN, ECHAN, OFQID, ISUB, JSUB, BUFF1,
     *         BUFF2, IRET)
            IF (IRET.NE.0) GO TO 999
 270        CONTINUE
         END IF
C                                       SN tables
      CALL FNDEXT ('SN', CATUV, NVER)
      IF (DOCAL) NVER = 0
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SNSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, ISUB, JSUB, BUFF1, BUFF2,
     *      IRET)
         IF (IRET.GT.0) GO TO 999
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SYSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 290     CONTINUE
C                                       SU tables
      IF (ILOCSU.GE.0) THEN
         CALL FNDEXT ('SU', CATUV, NVER)
         DO 300 VER = 1,NVER
            CALL ISTAB ('SU', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BIF, EIF,
     *         OFQID, BUFF1, BUFF2, IRET)
            IF (IRET.GT.0) GO TO 999
 300        CONTINUE
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATUV, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL TYSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATUV, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, TIME1,
     *      TIME2, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATUV, NVER)
      IF (DOBL) NVER = 0
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, EIF, OFQID, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('COPTAB: ONLY FG TABLE VERSIONS',I4,' TO',I4,' COPIED')
      END
