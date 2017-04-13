C                                                          Include DTPM
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2012, 2015
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
C                                       Include file for simulation
C                                       parameters (DTSIM/DTCHK).
      INCLUDE 'INCS:PUVD.INC'
C                                       AN table information
      INTEGER   MAXSUB
      PARAMETER (MAXSUB = 5)
      LOGICAL   WDTPOL(MAXSUB)
      CHARACTER*8 LDTANT(MAXANT,MAXSUB), LDTARR(MAXSUB),
     *   LDTPOL(MAXSUB)
      DOUBLE PRECISION DTCPOS(3,MAXANT,MAXSUB), DTORB(6,MAXANT,MAXSUB)
      INTEGER   IDTMNT(MAXANT,MAXSUB), IDTANO(MAXANT,MAXSUB),
     *   NDTANT(MAXSUB), IDTSUB(MAXSUB), NDTSUB, NDTMSB
      COMMON /DTAN/ DTCPOS, DTORB, IDTMNT, IDTANO,
     *   NDTANT, IDTSUB, NDTSUB, NDTMSB, WDTPOL
      COMMON /DTANC/ LDTANT, LDTARR, LDTPOL
C                                       FQ table information
      INTEGER   MAXFQD
      PARAMETER (MAXFQD = 8)
      DOUBLE PRECISION DTFREQ(MAXIF,MAXFQD)
      REAL      RDTCHW(MAXIF,MAXFQD), RDTTBW(MAXIF,MAXFQD)
      INTEGER   IDTSDB(MAXIF,MAXFQD), IDTFQD(MAXFQD), NDTFQD, NDTFID
      COMMON /DTFQ/ DTFREQ, RDTCHW, RDTTBW, IDTSDB, IDTFQD, NDTFQD,
     *   NDTFID
C                                       SU table information
      INTEGER   MAXSID
      PARAMETER (MAXSID = 5)
      CHARACTER LDTSOU(MAXSID)*16
      DOUBLE PRECISION DTSRA(MAXSID), DTSDEC(MAXSID), DTEPOC(MAXSID),
     *   DTSUFQ(MAXIF,MAXSID), DTSLSR(MAXIF,MAXSID)
      INTEGER   IDTSID(MAXSID), NDTSID
      COMMON /DTSU/ DTSRA, DTSDEC, DTEPOC, DTSUFQ, DTSLSR, IDTSID,
     *   NDTSID
      COMMON /DTSUC/ LDTSOU
C                                       Catalog header
      CHARACTER LDTDAT*8
      LOGICAL   WDTCAT
      DOUBLE PRECISION DTRFRQ, DTJDAT, DTLRFQ
      INTEGER   NDTIF, NDTCHN, NDTSTK, IDTSTK(4), IDTCMP
      COMMON /DTCA/ DTRFRQ, DTJDAT, DTLRFQ, NDTIF, NDTCHN, NDTSTK,
     *   IDTSTK, IDTCMP, WDTCAT
      COMMON /DTCAC/ LDTDAT
C                                       Calibration errors
      INTEGER   MAXAFQ, MAXERP, MAXPRD
C                                       Max. no. FQ_ID * Max. ant.
      PARAMETER (MAXAFQ = MAXFQD * MAXANT)
C                                       Max. parameters per entry
      PARAMETER (MAXERP = 10)
C                                       Product for all IF's
      PARAMETER (MAXPRD = MAXIF * MAXERP)
C
      CHARACTER LDTAER(MAXAFQ)*8
      LOGICAL   WDTLIN
      REAL RDTBP(MAXERP,MAXIF,2,MAXAFQ), RDTRAT(MAXERP,MAXIF,2,MAXAFQ),
     *   RDTDEL(MAXERP,MAXIF,2,MAXAFQ), RDTPHS(MAXERP,MAXIF,2,MAXAFQ),
     *   RDTFED(MAXERP,MAXIF,2,MAXAFQ), RDTGAN(MAXERP,MAXIF,2,MAXAFQ),
     *   RDTTSY(MAXERP,MAXIF,2,MAXAFQ), RDTGER(MAXERP,MAXIF,2,MAXAFQ)
      INTEGER   IDTEFQ(MAXAFQ), NDTAER
      COMMON /DTCE/ RDTBP, RDTRAT, RDTDEL, RDTPHS, RDTFED, RDTGAN,
     *   RDTTSY, RDTGER, IDTEFQ, NDTAER, WDTLIN
      COMMON /DTCEC/ LDTAER
C                                       Source model data
      INTEGER   MAXSMD, MAXCMP
C                                       Allow up to 128 components
C                                       per source
      PARAMETER (MAXCMP = 128, MAXSMD = MAXSID * MAXCMP)
      CHARACTER LDTSMD(MAXSMD)*16
      REAL      RDTSPM(12,MAXSMD), RDTSPC(5,MAXSMD), RDTVEL(2,MAXSMD)
      INTEGER   NDTSMD
      COMMON /DTSM/ RDTSPM, RDTSPC, RDTVEL, NDTSMD
      COMMON /DTSMC/ LDTSMD
C                                       Schedule entries
      INTEGER   MAXSCH
      PARAMETER (MAXSCH = 128)
      CHARACTER LDTSCH(MAXSCH)*16, LDTSEL(MAXSCH)*8
      REAL      RDTSTT(MAXSCH), RDTEND(MAXSCH), RDTINT(MAXSCH),
     *   RDTSTK(MAXSCH)
      INTEGER   IDTDAY(MAXSCH), IDTMTH(MAXSCH), IDTYR(MAXSCH),
     *   IDTSSB(MAXSCH), IDTSFQ(MAXSCH), NDTSCH
      COMMON /DTSH/ RDTSTT, RDTEND, RDTINT, RDTSTK, IDTDAY, IDTMTH,
     *   IDTYR, IDTSSB, IDTSFQ, NDTSCH
      COMMON /DTSHC/ LDTSCH, LDTSEL
C                                       Flag information
      INTEGER   MAXFG
      PARAMETER (MAXFG = 128)
      CHARACTER LDTFGA(MAXFG)*8, LDTFGB(MAXFG)*8, LDTFGS(MAXFG)*16
      REAL      RDTSFG(MAXFG), RDTEFG(MAXFG)
      INTEGER   IDTFDY(MAXFG), IDTFMH(MAXFG), IDTFYR(MAXFG),
     *   IDTBIF(MAXFG), IDTEIF(MAXFG), IDTBCH(MAXFG),
     *   IDTECH(MAXFG), IDTFFQ(MAXFG), IDTSBF(MAXFG),
     *   IDTSKF(4,MAXFG), NDTFG
      COMMON /DTFG/ RDTSFG, RDTEFG, IDTFDY, IDTFMH, IDTFYR, IDTBIF,
     *   IDTEIF, IDTBCH, IDTECH, IDTFFQ, IDTSBF, IDTSKF, NDTFG
      COMMON /DTFGC/ LDTFGA, LDTFGB, LDTFGS
C                                       Bandpass table information
C                                       (only one BP table allowed)
C                                       Max. # BP entries
      INTEGER   MAXBPD
      PARAMETER (MAXBPD = 2*MAXANT)
      CHARACTER LDTBPT*8, LDTBAN(MAXBPD)*8
      REAL      RDTBPT(MAXBPD), RDTBPE(MAXBPD), RDTBPI(MAXBPD),
     *   RDTBPS(MAXBPD)
      INTEGER   IDTBPD(MAXBPD), IDTBPM(MAXBPD), IDTBPY(MAXBPD),
     *   IDTBPX(MAXBPD), NDTBPA, IDTBPV, IDTBFI(MAXIF,MAXBPD),
     *   IDTBFP(MAXIF,MAXBPD), IDTBFC(2*MAXIF,MAXBPD),IDTBFQ(MAXBPD),
     *   IDTBSB(MAXBPD), IDTBPC(MAXBPD), IDTBPN, NDTBPD
      COMMON /DTBP/ RDTBPT, RDTBPE, RDTBPI, RDTBPS, IDTBPD, IDTBPM,
     *   IDTBPY, IDTBPX, NDTBPA, IDTBPV, IDTBFI, IDTBFP, IDTBFC,
     *   IDTBFQ, IDTBSB, IDTBPC, IDTBPN, NDTBPD
      COMMON /DTBPC/ LDTBPT, LDTBAN
C                                       Defaults
      INTEGER   DYEAR, DMONTH, DDAY
      REAL      DEFSYS(MAXIF), DEFGAN(MAXIF)
      COMMON /DTDEFS/ DEFGAN, DEFSYS, DYEAR, DMONTH, DDAY
C                                                        End DTPM
