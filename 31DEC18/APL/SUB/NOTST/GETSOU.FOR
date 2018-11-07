      SUBROUTINE GETSOU (SUID, DISK, CNO, CATBLK, LUN, IERR)
C-----------------------------------------------------------------------
C! Find info on a given source id.
C# EXT-appl UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2003-2004, 2012-2013
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
C   Routine to look up source info for a given source number.
C   If SUID = 0 or can't find SU table the info will be obtained from
C   the common maintained by UVPGET; UVPGET should be called before
C   calling GETSOU.
C   Inputs:
C      SUID     I       Source number.
C      DISK     I       Disk number for NX and SN tables.
C      CNO      I       Catalog slot number
C      CATBLK   I(256)  Catalog header
C      LUN      I       LUN to use. (e.g. 25)
C   Output in common (INCLUDE INCS:DSOU.INC)
C      IDSOUR   I       Source ID as defined in the SOURCE table.
C      SOUNAM   C*16    Source name
C      QUAL     I       Source qualifier.
C      CALCOD   C*4     Calibrator code
C      FLUX     R(4,*)  Total flux density I, Q, U, V pol, (Jy)
C                          1 set per IF.
C      VELTYP   C*8     Velocity type ('LSR', 'HELIO')
C      VELDEF   C*8     Velocity def.'RADIO','OPTICAL
C      SUFQID   I       FQ ID for which flux, vel etc. were modified.
C                       -1 => have virgin values
C                       -999 => is not in the table
C      FREQO    D(*)    Frequency offset (Hz)
C      BANDW    D       Bandwidth (Hz)
C      RAEPO    D       Right ascension at mean EPOCH (radians)
C      DECEPO   D       Declination at mean EPOCH (radians)
C      EPOCH    D       Mean Epoch for position in yr. since year 0.0
C      RAAPP    D       Apparent Right ascension (radians)
C      DECAPP   D       Apparent Declination(radians)
C      RAOBS    D       Pointing Right ascension (radians)
C      DECOBS   D       Pointing Declination(radians)
C      LSRVEL   D(*)    LSR velocity (m/sec) of each IF
C      RESTFQ   D(*)    Line rest frequency (Hz) of IF.
C      PMRA     D       Proper motion (deg/day) in RA
C      PMDEC    D       Proper motion (deg/day) in declination
C   Output:
C      IERR     I       Return code. 0=OK, else failed.
C                          11 => Couldn't find source in SU table.
C-----------------------------------------------------------------------
      INTEGER   SUID, DISK, CNO, CATBLK(256), LUN, IERR
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), VER, KOLS(MAXSUC), NUMV(MAXSUC), NUMIF,
     *   JERR, I, MSGSAV, IRNO, NUMREC, LOOP, IEQ, DIR, CATUV(256)
      REAL      REQ, POLAR(2)
      DOUBLE PRECISION JD, DELDAT, OBSPOS(3)
      LOGICAL GR
      HOLLERITH CATH(256)
      CHARACTER OBSDAT*8
      INTEGER   XDSOUR, XQUAL, XUFQID
      REAL      XFLUX(4,MAXIF)
      DOUBLE PRECISION XFREQO(MAXIF), XBANDW, XRAEPO, XECEPO, XEPOCH,
     *   XRAAPP, XECAPP, XSRVEL(MAXIF), XESTFQ(MAXIF), XPMRA, XPMDEC,
     *   XRAOBS, XDECOB, CATD(128)
      CHARACTER XSNAME*16, XALCOD*4, XELTYP*8, XELDEF*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATUV, CATH, CATD)
      EQUIVALENCE (IEQ, REQ)
      DATA DELDAT, DIR, OBSPOS, POLAR /1.D-6, 1, 3*0.D0, 2*0./
C-----------------------------------------------------------------------
      CALL COPY (256, CATBLK, CATUV)
      IERR = 0
C                                       See if need to look up source
      IF ((SUID.EQ.IDSOUR) .AND. (IDSOUR.NE.0)) GO TO 999
C                                       Initialize
      JERR = 0
      IERR = 0
C                                       See if no source
      IF (SUID.EQ.0) GO TO 500
C                                       Find Source Info
C                                       Open SU table
      MSGSAV = MSGSUP
      MSGSUP = 32000
      VER = 1
C                                       do not change variables in the
C                                       common unless we find the source
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, XELTYP, XELDEF, XUFQID, IRNO, KOLS, NUMV, JERR)
      MSGSUP = MSGSAV
      IF (JERR.NE.0) GO TO 500
C                                       Get number of records
      NUMREC = BUFFER(5)
C                                       Find source
      DO 50 LOOP = 1,NUMREC
         IRNO = LOOP
         CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, XDSOUR, XSNAME,
     *      XQUAL, XALCOD, XFLUX, XFREQO, XBANDW, XRAEPO, XECEPO,
     *      XEPOCH, XRAAPP, XECAPP, XRAOBS, XDECOB, XSRVEL, XESTFQ,
     *      XPMRA, XPMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
C                                       Desired source: move to common
         IF (SUID.EQ.XDSOUR) THEN
            IDSOUR = XDSOUR
            QUAL = XQUAL
            SUFQID = XUFQID
            BANDW = XBANDW
            RAEPO = XRAEPO * DG2RAD
            DECEPO = XECEPO * DG2RAD
            EPOCH = XEPOCH
            PMRA = XPMRA
            RAAPP = XRAAPP * DG2RAD
            DECAPP = XECAPP * DG2RAD
            RAOBS = XRAOBS * DG2RAD
            DECOBS = XDECOB * DG2RAD
            PMDEC = XPMDEC
            SNAME = XSNAME
            CALCOD = XALCOD
            VELTYP = XELTYP
            VELDEF = XELDEF
            DO 40 I = 1,NUMIF
               FREQO(I) = XFREQO(I)
               LSRVEL(I) = XSRVEL(I)
               RESTFQ(I) = XESTFQ(I)
               FLUX(1,I) = XFLUX(1,I)
               FLUX(2,I) = XFLUX(2,I)
               FLUX(3,I) = XFLUX(3,I)
               FLUX(4,I) = XFLUX(4,I)
 40            CONTINUE
            GO TO 60
            END IF
 50      CONTINUE
C                                       Didn't find source
      JERR = 11
      WRITE (MSGTXT,1050) SUID
 60   CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1060) IERR
         GO TO 990
 70   IERR = JERR
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Single source, get info from
C                                       header.
  500 CONTINUE
C                                       Find time of observation
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
      CALL JULDAY (OBSDAT, JD)
C                                       Find RAAPP, DECAP for
C                                       observation date.
C                                       Note use of EQUIVALENCE:
      IEQ = CATBLK(KREPO)
      EPOCH = REQ
      RAEPO = RA * DG2RAD
      DECEPO = DEC * DG2RAD
      RAOBS = CATD(KDORA) * DG2RAD
      DECOBS = CATD(KDODE) * DG2RAD
      GR = .TRUE.
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *   RAEPO, DECEPO, RAAPP, DECAPP)
      IERR = 0
      IDSOUR = 0
      QUAL = 0
      CALCOD = '    '
      VELTYP = '        '
      VELDEF = '        '
      SUFQID = -999
      SNAME = SOURCE
      BANDW = 0.0
      DO 550 LOOP = 1,NUMIF
         FREQO(LOOP) = 0.0D0
         FLUX(1,LOOP) = 0.0
         FLUX(2,LOOP) = 0.0
         FLUX(3,LOOP) = 0.0
         FLUX(4,LOOP) = 0.0
         LSRVEL(LOOP) = 0.0D0
         RESTFQ(LOOP) = 0.0D0
 550     CONTINUE
      PMRA = 0.0
      PMDEC = 0.0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETSOU: TABSOU ERROR ',I3)
 1050 FORMAT ('GETSOU: COULD NOT FIND DATA FOR SOURCE NO. ',I5)
 1060 FORMAT ('GETSOU: TABIO ERROR ',I3,' CLOSING SU TABLE')
      END
