C                                                          Include DSEL.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2004, 2010, 2018
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
C                                       Parameter includes.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MXDSVB, MAXSB
C                                       Parameter for VLBA delay
C                                       decorrelation corrections
C                                       MXDSVB = Delay decorrelation
C                                       buffer size (assumes max. 99
C                                       subarrays)
      PARAMETER (MXDSVB=MAXCID * MAXIF)
C                                       Max. no. subarrays
      PARAMETER (MAXSB=99)
C                                       Pointers for CL/SN table access;
C                                       note these must be coordinated
C                                       with COLHED in GAININ.
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer
      INTEGER   CTDTIM, CTRTMI, CTISID, CTIANT, CTISUB, CTIFQI, CTRIFR,
     *   CTRDD1, CTRRE1, CTRIM1, CTRRA1, CTRDE1, CTRWE1, CTIRF1,
     *   CTRDD2, CTRRE2, CTRIM2, CTRRA2, CTRDE2, CTRWE2, CTIRF2
C                                       Common to all polarizations
C                                       CTDTIM = Time
      PARAMETER (CTDTIM = 1)
C                                       CTRTMI = Time interval
      PARAMETER (CTRTMI = CTDTIM + 1)
C                                       CTISID = Source ID.
      PARAMETER (CTISID = CTRTMI + 1)
C                                       CTIANT = Antenna number
      PARAMETER (CTIANT = CTISID + 1)
C                                       CTISUB = Subarray number
      PARAMETER (CTISUB = CTIANT + 1)
C                                       CTIFQI = Freq. ID.
      PARAMETER (CTIFQI = CTISUB + 1)
C                                       CTRIFR = Ions. Fara. rot.
      PARAMETER (CTRIFR = CTIFQI + 1)
C                                       Polarization 1
C                                       CTRDD1 = Dispersive delay
      PARAMETER (CTRDD1 = CTRIFR + 1)
C                                       CTRRE1 = Real part of gain
      PARAMETER (CTRRE1 = CTRDD1 + 1)
C                                       CTRIM1 = Imaginary part
      PARAMETER (CTRIM1 = CTRRE1 + 1)
C                                       CTRDE1 = Delay residual
      PARAMETER (CTRDE1 = CTRIM1 + 1)
C                                       CTRRA1 = Rate residual
      PARAMETER (CTRRA1 = CTRDE1 + 1)
C                                       CTRWE1 = Weight
      PARAMETER (CTRWE1 = CTRRA1 + 1)
C                                       CTIRF1 = Ref. antenna
      PARAMETER (CTIRF1 = CTRWE1 + 1)
C                                       Polarization 2
C                                       CTRDD2 = Dispersive delay
      PARAMETER (CTRDD2 = CTIRF1 + 1)
C                                       CTRRE2 = Real part of gain
      PARAMETER (CTRRE2 = CTRDD2 + 1)
C                                       CTRIM2 = Imaginary part
      PARAMETER (CTRIM2 = CTRRE2 + 1)
C                                       CTRDE2 = Delay residual
      PARAMETER (CTRDE2 = CTRIM2 + 1)
C                                       CTRRA2 = Rate residual
      PARAMETER (CTRRA2 = CTRDE2 + 1)
C                                       CTRWE2 = Weight
      PARAMETER (CTRWE2 = CTRRA2 + 1)
C                                       CTIRF2 = Ref. antenna
      PARAMETER (CTIRF2 = CTRWE2 + 1)
C                                       Data selection and control
      INTEGER   SOUWAN(XSTBSZ), CALWAN(XSTBSZ), JADR(2,4), NANTSL,
     *   NSOUWD, DECMP(2,MAXIF*4), ANTENS(MAXANT), SOUWTN(30), NCALWD,
     *   CALWTN(30), SUBARR, CURSOU, NXKOLS(MAXNXC), MVIS(8),
     *   NXNUMV(MAXNXC), FQKOLS(MAXFQC), FQNUMV(MAXFQC), PMODE, LRECIN,
     *   UBUFSZ, BCHAN, ECHAN, BIF, EIF, NPRMIN, KLOCSU, KLOCFQ, SELQUA,
     *   KLOCIF, KLOCFY, KLOCWT, KLOCSC, NDECMP, BCHANS, ECHANS, FRQSEL,
     *   FSTRED, INXRNO, NINDEX, FSTVIS, LSTVIS, IFQRNO, INITVS, DOBAND,
     *   BPVER, FGVER, DOSMTH
      LOGICAL   DOSWNT, DOCWNT, DOAWNT, ALLWT, TRANSL, ISCMP, DOXCOR,
     *   DOACOR, DOWTCL, DOFQSL, DOFLAG, DOUVRA
      REAL      SMOOTH(3), SMTAB(MAXSMO), SELFAC(2,4), TIMRNG(8),
     *   UVRNG(2), UVRA(2), TSTART, TEND, SELBAN
      CHARACTER SOURCS(30)*16, CALSOU(30)*16, STOKES*4, SELCOD*4
      DOUBLE PRECISION UVFREQ, SELFRQ, SFREQS(MAXIF)
C                                       CAL table info
      CHARACTER LTPVBA(MXDSVB)*8
      DOUBLE PRECISION DBTVBA(MXDSVB)
      REAL      CALTAB(XCTBSZ,2), CURCAL(XCTBSZ), LAMBDA(MAXCIF),
     *   LCALTM, GMMOD, CALTIM(3), RATFAC(MAXIF), DELFAC(MAXIF), DXTIME,
     *   DXFREQ, IFRTAB(MAXANT,2), IFR(MAXANT), DDTAB(2,MAXANT,2),
     *   DDELAY(2,MAXANT), TVGVBA(MXDSVB), TIMECL(2,MAXANT),
     *   NXDSM(MXDSVB)
      LOGICAL   DOCAL, DOAPPL, DORSM, DODSM(MXDSVB), WRNVBA(MAXSB)
      INTEGER   ICLRNO, NCLINR, MAXCLR, CNTREC(2,3), CLVER, CLUSE,
     *   NUMANT, NUMPOL, NUMIF, CIDSOU(2), CLKOLS(MAXCLC), NLAMDA,
     *   CLNUMV(MAXCLC), LCLTAB, LCUCAL, ICALP1, ICALP2, POLOFF(4,2),
     *   NFTVBA(MXDSVB), ICQVBA(MAXSB), ITFVBA(MXDSVB),
     *   NEXTCL, DOPOL, PDVER
C                                       Baseline table info
      REAL      LBLTM, BLTAB(XBTBSZ,2), BLFAC(XBTBSZ), BLTIM(3)
      LOGICAL   DOBL
      INTEGER   IBLRNO, NBLINR, BLVER, BLKOLS(MAXBLC), BLNUMV(MAXBLC),
     *   IBLP1, IBLP2
C                                       Channel 0 stuff
      INTEGER   FSTVS3, LREC3, LSTVS3, NREAD3, FSTRD3, KLOCW3,
     *   KLOCS3, NDECM3, DECM3(2,MAXIF*4), BIND3, RECNO3, LENBU3
      LOGICAL   ISCMP3, DOUVIN
C                                       File specification.
      INTEGER   IUDISK, IUSEQ, IUCNO, IULUN, IUFIND, ICLUN, IFLUN,
     *   IXLUN, IBLUN, IPLUN, IQLUN, IANLUN, CATUV(256), CATBLK(256)
      REAL      USEQ, UDISK
      CHARACTER UNAME*12, UCLAS*6, UFILE*48
C                                       I/O buffers
      INTEGER   CLBUFF(512), NXBUFF(512), BLBUFF(512), FQBUFF(512),
     *   ANBUFF(512)
      REAL      UBUFF(UVBFSL)
C                                       Character common
      COMMON /SELCHR/ SOURCS, CALSOU, STOKES, SELCOD, UNAME, UCLAS,
     *   UFILE, LTPVBA
C                                       Commons for UVGET use
C                                       Buffers
      COMMON /SELBUF/ UBUFF, CLBUFF, NXBUFF, BLBUFF, FQBUFF, ANBUFF
C                                       Data selection and control
      COMMON /SELCAL/ CATUV, UVFREQ, SELFRQ, SFREQS,
     *   SMTAB, SELFAC, SOUWAN, CALWAN, JADR, DECMP, USEQ, UDISK,
     *   TIMRNG, UVRNG, UVRA, TSTART, TEND, SELBAN, DOSWNT, DOCWNT,
     *   DOAWNT, ALLWT, TRANSL, DOSMTH, ISCMP, DOXCOR, DOACOR, DOWTCL,
     *   DOFQSL, INXRNO, NINDEX, FSTVIS, LSTVIS, IFQRNO, IUDISK, IUSEQ,
     *   IUCNO, IULUN, IUFIND, ICLUN, IFLUN, IXLUN, IBLUN, IPLUN, IQLUN,
     *   ANTENS, NANTSL, NSOUWD, SOUWTN, NCALWD, CALWTN,
     *   SUBARR, CURSOU, NXKOLS, NXNUMV, FQKOLS, FQNUMV, MVIS, PMODE,
     *   LRECIN, UBUFSZ, BCHAN, ECHAN, BIF, EIF, NPRMIN, KLOCSU, KLOCFQ,
     *   SELQUA, SMOOTH, KLOCIF, KLOCFY, KLOCWT, KLOCSC, NDECMP, BCHANS,
     *   ECHANS, FRQSEL, FSTRED, INITVS, IANLUN, DOBAND, BPVER, DOFLAG,
     *   FGVER, DOUVRA
C                                       CAL table info
      COMMON /CGNINF/ DBTVBA, CALTAB, CURCAL, LAMBDA, LCALTM,  GMMOD,
     *   CALTIM, RATFAC, DELFAC, DXTIME, DXFREQ, IFRTAB, IFR, DDTAB,
     *   DDELAY, NLAMDA, TVGVBA, DOCAL, DOAPPL, ICLRNO, NCLINR, MAXCLR,
     *   CNTREC, CLVER, CLUSE, NUMANT, NUMPOL, NUMIF, CIDSOU, CLKOLS,
     *   CLNUMV, LCLTAB, LCUCAL, ICALP1, ICALP2, POLOFF, NXDSM, NFTVBA,
     *   ICQVBA, ITFVBA, DODSM, WRNVBA, DORSM, DOPOL, PDVER
      COMMON /EXTEST/ TIMECL, NEXTCL
C                                       BL table info
      COMMON /CBLINF/ BLTAB, BLFAC, LBLTM, BLTIM,
     *   DOBL, IBLRNO, NBLINR, BLVER, BLKOLS, BLNUMV, IBLP1, IBLP2
C                                       Channel 0 common
      COMMON /CHNZ/ DECM3, FSTVS3, LREC3, LSTVS3, NREAD3, FSTRD3,
     *   KLOCW3, KLOCS3, NDECM3, BIND3, RECNO3, LENBU3, ISCMP3, DOUVIN
C
      COMMON /MAPHDR/ CATBLK
C                                                          End DSEL.
