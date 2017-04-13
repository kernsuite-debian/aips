C     Developed from MK3IN.FOR   M. Wunderlich MPIfR Bonn  Feb 97
C     Last change:  04-Jun-97 09:10
C
LOCAL INCLUDE 'MK3IN.INC'
C                                       Local include MK3IN
C                                       AIPSish stuff here
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  ITAPE, NFILES, SEQOUT, DISKO, CNOOUT, NUMHIS, JBUFSZ,
     *   FILSIZ, FSTVIS, LSTVIS, CNTVIS, NSOUR, IDSOUR(100),
     *   KLOCWT, NCHAN, NPOLN, NIF, ISTOK, CURSOU, DAYOFF,
     *   SUBUFF(1024), SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO,
     *   CLBUFF(1024), CLKOLS(MAXCLC), CLNUMV(MAXCLC), ICLRNO,
     *   IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC)
      HOLLERITH XINFIL(12), XIN2FL(12), XREFDA(2), XNAMOU(3),
     *   XCLAOU(2)
      CHARACTER INFILE*48, IN2FIL*48, REFDAY*8, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64, OFILE*48, OBSDAT*8, SULIST(100)*16
      REAL      XTAPE, XNF, XTR(8), XSRC(4, 30), XDOUVC, XDOCON, XSOUT,
     *   XDISO, APARM(10), BUFFER(UVBFSS), BANDW, EPOCH, CLINC, IATOFF,
     *   UVROT(2), SCNGST, CURINT
      LOGICAL   DOUVC, DOCON
      DOUBLE PRECISION REFREQ, REFJD
C                                       Output file info
      COMMON /INFCOM/ REFREQ, REFJD, BANDW, EPOCH, CLINC, IATOFF, UVROT,
     *   SCNGST, CURINT,
     *   FILSIZ, FSTVIS, LSTVIS, CNTVIS, KLOCWT, NCHAN, NPOLN, NIF,
     *   ISTOK, CURSOU, DAYOFF
C                                       AIPS I/O buffer etc
      COMMON /BUFRS/ JBUFSZ, BUFFER
C                                       AIPS adverbs passed
      COMMON /INPARM/ XINFIL, XIN2FL, XTAPE, XNF, XREFDA, XTR, XSRC,
     *   XDOUVC, XDOCON, XNAMOU, XCLAOU, XSOUT, XDISO, APARM,
     *   ITAPE, NFILES, SEQOUT, DISKO, CNOOUT, NUMHIS, DOUVC, DOCON
C                                       Various CHARACTER variables
      COMMON /CHRCOM/ INFILE, IN2FIL, REFDAY, NAMOUT, CLAOUT, HISCRD,
     *   OFILE, OBSDAT, SULIST
C                                       Tables and source stuff
      COMMON /TABCOM/ NSOUR, IDSOUR,
     *   SUBUFF, SUKOLS, SUNUMV, ISURNO,
     *   CLBUFF, CLKOLS, CLNUMV, ICLRNO,
     *   FQKOLS, FQNUMV, IFQRNO
LOCAL END
LOCAL INCLUDE 'PTAPE.INC'
C                                       Parameters for A-tape data.
      INTEGER   MXCOR, MXBAS, MXM3FQ, MXHUGE, MXREF,
     *   MXANT, MXCTYP, MXTIME, MXRDAY, MXFQID
C                                       MXCOR = Max no. correlator mod.
      PARAMETER (MXCOR=100)
C                                       MXBAS = Max. no. baselines
      PARAMETER (MXBAS=120)
C                                       MXMKFQ = Max. no. MKIII freq.
C                                                plus one.
      PARAMETER (MXM3FQ=29)
C                                       MXTIME = Max. no. time entries
C                                       per scan.
      PARAMETER (MXTIME=784)
C                                       MXHUGE = size of lag data array
C                                       All data on baseline for scan
      PARAMETER (MXHUGE=MXCOR*8*MXTIME)
C                                       MXREF = size of cross ref.
C                                       tables.
      PARAMETER (MXREF=5000)
C                                       MXANT = Max. no antennas
      PARAMETER (MXANT=30)
C                                       MXRDAY = Max no of days in obs
      PARAMETER (MXRDAY=20)
C                                       Highest correlation mode.
      PARAMETER (MXCTYP=2)
C                                       No of observing bands
      PARAMETER (MXFQID=24)
LOCAL END
LOCAL INCLUDE 'MKDAT.INC'
C                                       Arrays for current scan
C                                       baseline visibility data.
      LOGICAL MKTOK(MXTIME), MKFRPS, MKDOPL
      REAL MKDATA(2,MXHUGE), MKWT(MXHUGE/8), MKFBS(4,MXTIME)
      INTEGER MKNTIM, MKCTIM, MKTYPE
      COMMON /MKADAT/ MKDATA, MKWT, MKFBS, MKNTIM,
     *   MKCTIM, MKTYPE, MKTOK, MKFRPS, MKDOPL
LOCAL END
LOCAL INCLUDE 'MKINP.INC'
C                                       Some global A-tape parameters.
      LOGICAL MKDFBS, MKDPHA, DOSIDB, DOLOFF
      REAL ATSEL(2), MKCLIN, MKMPER, MKDROP
      INTEGER MKMXFL, MKNTYP, MKPRNT, MKDBUG, MKPOL1, MKPOL2,
     *   MKAIF, MKSIDB, MKNLAG, MKNPOL, MKAFRQ, MINPOL, MAXPOL,
     *   MKMASK
      COMMON /MKAINP/ ATSEL, MKCLIN, MKMPER, MKDROP, MKMXFL,
     *   MKNTYP, MKPRNT, MKDBUG, MKPOL1, MKPOL2, MKAIF,
     *   MKSIDB, MKNLAG, MKNPOL, MKAFRQ, MINPOL, MAXPOL, MKMASK,
     *   MKDFBS, MKDPHA, DOSIDB, DOLOFF
LOCAL END
LOCAL INCLUDE 'MKSCN.INC'
C                                       Scan based indices and
C                                       other parameters.
      LOGICAL MKWANT
      INTEGER MKBL(2,MXBAS), MKBLNX(2,MXBAS), MKBFQ(MXBAS),
     *   MKBAFQ(MXBAS), MKBPFQ(MXBAS), MKXREF(12,MXREF),
     *   MKINDU, MKNBL, MKBASE, MKBASL, MKNFFQ, LXTNT(256)
      DOUBLE PRECISION MKFFQ(MXM3FQ), LSBOFF
      COMMON /MKASCN/ MKFFQ, LSBOFF,
     *   MKBL, MKBLNX, MKBFQ, MKBAFQ, MKBPFQ,
     *   MKXREF, MKINDU, MKNBL, MKBASE, MKBASL, MKNFFQ, MKWANT, LXTNT
LOCAL END
LOCAL INCLUDE 'MKTIM.INC'
C                                       Current scan start time etc.
      DOUBLE PRECISION ATJDRF, MKSJD, MKSUTC, MKTMS, MKTOFF
      REAL ATIME1, ATIME2
      INTEGER MKLSCN, MKNTOF
      COMMON /MKATIM/ ATJDRF, MKSJD, MKSUTC, MKTMS, MKTOFF,
     *   ATIME1, ATIME2, MKLSCN, MKNTOF
LOCAL END
LOCAL INCLUDE 'MKVAR.INC'
C                                       Quantities related to the
C                                       correlator model.
      DOUBLE PRECISION MKTIMC(2,MXBAS), MKTIM0(MXBAS), MKPRT(MXBAS),
     *   MKINT(MXBAS), MKSIDT(MXBAS), MKSIDD(MXBAS), MKSMRT(MXBAS),
     *   MKUT1(MXBAS), MKWOB(2,MXBAS), MKGHA(MXBAS), MKSRAT(MXBAS),
     *   MKDHAD(MXBAS), MKVLIG(MXBAS), MKDLY(MXBAS), MKRAT(MXBAS),
     *   MKACC(MXBAS), MKPMRA, MKPMDC(MXBAS)
      INTEGER MKMODE(MXBAS), MKNFP(MXBAS), MKKSEL(MXBAS),
     *   MKNTRK(MXBAS)
      COMMON /MKAVAR/ MKTIMC, MKTIM0, MKPRT, MKINT, MKSIDT, MKSIDD,
     *   MKSMRT, MKUT1, MKWOB, MKGHA, MKSRAT, MKDHAD, MKVLIG,
     *   MKDLY, MKRAT, MKACC, MKPMRA, MKPMDC, MKMODE, MKNFP,
     *   MKKSEL, MKNTRK
LOCAL END
LOCAL INCLUDE 'MKSRC.INC'
C                                       Source based quantities.
      DOUBLE PRECISION MKRAIN(MXBAS), MKDECI(MXBAS), MKEPOC(MXBAS),
     *   MKRA(MXBAS), MKDEC(MXBAS), MKPPHA(3), MKPEPO, MKPDIS
      COMMON /MKASRC/ MKRAIN, MKDECI, MKEPOC, MKRA, MKDEC, MKPPHA,
     *   MKPEPO, MKPDIS
LOCAL END
LOCAL INCLUDE 'MKCOR.INC'
C                                       Correlator table
      INTEGER MKCFRQ(MXCOR), MKCSTK(MXCOR), MKCTYP(MXCOR),
     *   MKCMOD(MXCOR), MKGOOD(MXCOR), MKCTOT(MXCOR),
     *   MKMXLG(MXCOR), MKMNLG(MXCOR), MKER51(MXCOR,20),
     *   MKNR51(MXCOR), MKSERL(MXCOR), MKCMIN(MXCOR),
     *   MKCMAX(MXCOR), MKNSER, MKNCOR
      COMMON /MKACOR/ MKCFRQ, MKCSTK, MKCTYP, MKCMOD, MKGOOD,
     *   MKCTOT, MKMXLG, MKMNLG, MKER51, MKNR51, MKSERL,
     *   MKCMIN, MKCMAX, MKNSER, MKNCOR
LOCAL END
LOCAL INCLUDE 'MKCLT.INC'
C                                       CL table quantities.
      DOUBLE PRECISION MKTMCL(500), MKGEO(3,MXANT,500)
      REAL MKATM(3,MXANT,500)
      INTEGER MKNTMC, MKIREF
      COMMON /MKACLT/ MKTMCL, MKGEO, MKATM, MKNTMC, MKIREF
LOCAL END
LOCAL INCLUDE 'MKRFQ.INC'
C                                       RF frequency and phase
C                                       calibration tables.
      LOGICAL MKLOST
      DOUBLE PRECISION MKFRQ(MXM3FQ), MKPFRQ(MXM3FQ),
     *   MKAFQT(MXM3FQ), MKLOFF(2,MXM3FQ), MKFQT(MXFQID,MXM3FQ),
     *   MKPCAL(5,2,MXCOR), MKPCSC(4,MXANT,MXM3FQ),
     *   MKPFQT(MXFQID,MXM3FQ)
      INTEGER MKNFRQ(MXBAS), MKAFNO(MXFQID,MXM3FQ), MKNFQT(MXFQID),
C    *   MKNPFQ(MXFQID), MKNPID, MKNFID
     *   MKNPFQ(MXFQID), MKNPID, MKNFID, STKPFQ(MXFQID,MXM3FQ)
      COMMON /MKARFQ/ MKFRQ, MKPFRQ, MKAFQT, MKLOFF, MKFQT,
     *   MKPCAL, MKPCSC, MKPFQT, MKNPFQ,
C    *   MKNFRQ, MKAFNO, MKNFQT, MKNFID, MKNPID, MKLOST
     *   MKNFRQ, MKAFNO, MKNFQT, MKNFID, MKNPID, MKLOST, STKPFQ
LOCAL END
LOCAL INCLUDE 'MKSTA.INC'
C                                       Antenna based parameters.
      DOUBLE PRECISION ANTLOC(2,3,MXBAS), ANTSYN(2,MXBAS),
     *   ANIDLY(2,MXBAS), ANTCLK(2,MXBAS), ANTZEN(2,MXBAS),
     *   ANTEPO(2,MXBAS), MKAZ(MXANT), MKEL(MXANT)
      INTEGER NANT
      COMMON /MKASTA/ ANTLOC, ANTSYN, ANIDLY, ANTCLK, ANTZEN,
     *   ANTEPO, MKAZ, MKEL, NANT
LOCAL END
LOCAL INCLUDE 'MKCHR.INC'
C                                       Character variables
      CHARACTER MKSNAM(MXBAS)*8, ANTNAM(MXANT)*8,
     *   MKFCOD(4)*1, MKCVER*8
      COMMON /MKACHR/ MKSNAM, ANTNAM, MKFCOD, MKCVER
LOCAL END
LOCAL INCLUDE 'MKOTH.INC'
C                                       Tape buffers and other
C                                       variables.
      LOGICAL KFNDID(9)
C                       TBUFF changed from 2500 to 10240 for TAR
      INTEGER TBUFF(10240)
C
      INTEGER FDVEC(50), TMPBUF(2048), TBIND, MKFILE, IDXLST, IBLST
      DOUBLE PRECISION DTMPBF(1024)
      EQUIVALENCE (DTMPBF, TMPBUF)
      COMMON /MKAOTH/ TMPBUF, FDVEC, TBUFF, TBIND,
     *   MKFILE, IDXLST, IBLST, KFNDID
LOCAL END
LOCAL INCLUDE 'MK3TAB.INC'
C                                       Parameter tables.
      CHARACTER LTCVER*8, LTSRCE*8
      DOUBLE PRECISION DTJULD(MXRDAY), DTUT1(MXRDAY), DTGAST(MXRDAY),
     *   DTSIDD(MXRDAY), DTWOB(2,MXRDAY), DTVLIG, DTSMRT, DTINT,
     *   DTRAIN, DTDECI, DTEPOC, DTRA, DTDEC, DTTIM1, DTTIM2,
     *   DTALOC(3,MXANT), DTAEPO(MXANT), DTASYN(MXANT), DTACLK(MXANT),
     *   DTAFQT(MXFQID,MXM3FQ)
      INTEGER NTJULD, ITSOUR, ITNAFQ(MXFQID), NTAFQT
C                                       Run-based parameters.
      COMMON /RUNTB/ DTJULD, DTUT1, DTGAST, DTSIDD, DTWOB, DTVLIG,
     *   DTSMRT, DTINT, NTJULD
      COMMON /RUNTBC/ LTCVER
C                                       Source-based parameters.
      COMMON /SRCTB/ DTRAIN, DTDECI, DTEPOC, DTRA, DTDEC, DTTIM1,
     *   DTTIM2, ITSOUR
      COMMON /SRCTBC/ LTSRCE
C                                       Antenna-based parameters.
      COMMON /ANTTB/ DTALOC, DTAEPO, DTASYN, DTACLK
C                                       AIPS FQ table.
      COMMON /FRQATB/ DTAFQT, ITNAFQ, NTAFQT
LOCAL END
LOCAL INCLUDE 'AFILE.INC'
C                                       A-file text information
      INTEGER NUMLIN
C
      PARAMETER (NUMLIN=10000)
C
      CHARACTER ROOT(NUMLIN)*100, TAPEN(NUMLIN)*5, ATAPEN*5
      CHARACTER ROOTID(NUMLIN)*6
      INTEGER   EXTENT(NUMLIN), ROOTNM
      LOGICAL   AFILEX
C
      COMMON /AFILCH/ ROOT, TAPEN, ATAPEN, ROOTID
      COMMON /AFILDT/ EXTENT, ROOTNM, AFILEX
LOCAL END
LOCAL INCLUDE 'UNIX.INC'
C                                       Additional information
C                                       for UNIX style A-files
C
C                                       Let's assume that 100 characters
C                                       are enough for a TAR filename,
C                                       otherwise scanning the TAR
C                                       header would be much more
C                                       difficult
      CHARACTER UFLNAM(NUMLIN)*100
C                                       One line of a UNIX-style A-file
C                                       to be interpreted
      CHARACTER AULINE*512
C
LOCAL END
LOCAL INCLUDE 'MKFRNG.INC'
C                                       Include for MkIII FRNGE
C                                       solutions.
      INTEGER  MAXM3X
C                                       MAXM3X = Max. number of FRNGE
C                                       solutions (baselines).
      PARAMETER (MAXM3X=66)
      LOGICAL   M3GOT(MAXM3X)
      INTEGER   IC2(6,MAXM3X), IC3(6,MAXM3X), IC4(4,MAXM3X),
     *   IC5(MAXM3X), IC6(MAXM3X), IC7(MAXM3X), IC8(MAXM3X),
     *   IC9(28,MAXM3X), IC10(56,MAXM3X), IC11(MAXM3X), IC12(6,MAXM3X),
     *   IC13(MAXM3X), IC14(MAXM3X), IC15(MAXM3X), IC16(MAXM3X),
     *   IC17(MAXM3X), IC18(28,MAXM3X), IC19(84,MAXM3X),
     *   IC20(28,MAXM3X), IC21(56,MAXM3X), IC22(28,MAXM3X),
     *   IC23(MAXM3X), IC24(MAXM3X)
      REAL      RC51(28,MAXM3X), RC52(2,MAXM3X), RC53(MAXM3X),
     *   RC54(MAXM3X), RC55(MAXM3X), RC56(MAXM3X), RC57(MAXM3X),
     *   RC58(MAXM3X), RC59(2,MAXM3X), RC60(2,MAXM3X), RC61(MAXM3X),
     *   RC62(2,MAXM3X), RC63(6,MAXM3X), RC64(MAXM3X), RC65(MAXM3X),
     *   RC66(MAXM3X), RC67(MAXM3X), RC68(MAXM3X), RC69(MAXM3X),
     *   RC70(MAXM3X), RC71(MAXM3X), RC72(MAXM3X), RC73(MAXM3X),
     *   RC74(MAXM3X), RC75(MAXM3X), RC76(MAXM3X), RC77(MAXM3X)
      DOUBLE PRECISION DC39(14,MAXM3X), DC40(MAXM3X), DC41(MAXM3X),
     *   DC42(MAXM3X), DC43(MAXM3X), DC44(MAXM3X), DC45(MAXM3X),
     *   DC46(MAXM3X), DC47(MAXM3X), DC48(MAXM3X), DC49(MAXM3X),
     *   DC50(MAXM3X)
      CHARACTER  CC1(MAXM3X)*2, CC25(MAXM3X)*8, CC26(MAXM3X)*8,
     *   CC27(MAXM3X)*8, CC28(MAXM3X)*6, CC29(MAXM3X)*8, CC30(MAXM3X)*8,
     *   CC31(MAXM3X)*6, CC32(MAXM3X)*8, CC33(MAXM3X)*1, CC34(MAXM3X)*2,
     *   CC35(MAXM3X)*6, CC36(MAXM3X)*6, CC37(MAXM3X)*8, CC38(MAXM3X)*8
      COMMON /FRGTAB/ DC39, DC40, DC41, DC42, DC43, DC44, DC45, DC46,
     *   DC47, DC48, DC49, DC50, RC51, RC52, RC53, RC54, RC55, RC56,
     *   RC57, RC58, RC59, RC60, RC61, RC62, RC63, RC64, RC65, RC66,
     *   RC67, RC68, RC69, RC70, RC71, RC72, RC73, RC74, RC75, RC76,
     *   RC77, IC2, IC3, IC4, IC5, IC6, IC7, IC8, IC9, IC10, IC11, IC12,
     *   IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22,
     *   IC23, IC24, M3GOT
      COMMON /FRGTAC/ CC1, CC25, CC26, CC27, CC28, CC29, CC30, CC31,
     *   CC32, CC33, CC34, CC35, CC36, CC37, CC38
LOCAL END
LOCAL INCLUDE 'SRCSEL.INC'
C                                       Include for source selection
C                                       common.
C
C                                       SRCLST = source names from
C                                        SOURCES adverb
C                                       NUMSRC = number of names in
C                                        SRCLST
C                                       DESEL = true if sources in
C                                        SRCLST are to be excluded from
C                                        selection
      CHARACTER SRCLST(30)*16
      INTEGER   NUMSRC
      LOGICAL   DESEL
      COMMON /CSRCSL/ SRCLST
      COMMON /SRCSEL/ NUMSRC, DESEL
      SAVE /CSRCSL/, /SRCSEL/
LOCAL END
LOCAL INCLUDE 'ALIAS.INC'
C                                       Local include for antenna
C                                       aliases.
C
C                                       MAXALI = maximum number of
C                                        aliases
      INTEGER   MAXALI
      PARAMETER (MAXALI = 20)
C                                       ALIAS = list of aliases and
C                                        translations
C                                       NALIAS = number of aliases
      CHARACTER ALIAS(2, MAXALI)*8
      INTEGER   NALIAS
      COMMON /CALIAS/ ALIAS
      COMMON /XALIAS/ NALIAS
      SAVE /CALIAS/, /XALIAS/
LOCAL END
LOCAL INCLUDE 'HAYSTACK'
      INTEGER   SRATE, DAP, DPP, TIME0, UTOST1, BQA, BQB, BQC, DHADT,
     *   BTR0, EPOCH0, BTE0, BTR, BTI, TWOPI, TSTART, TIMEND, ELQA1,
     *   ELQA2, ELQB1, ELQB2, ZENAT1, ZENAT2, TMER1, TMER2, TIME,
     *   BQADOT, BQBDOT, BQCDOT, BQD, BQE, PHSITE, PHSDOT, TIME1, PHSDD
      PARAMETER (SRATE = 1,         DAP = 2)
      PARAMETER (DPP = 3,           TIME0 = 4)
      PARAMETER (UTOST1 = 5,        BQA = 6)
      PARAMETER (BQB = 7,           BQC = 8)
      PARAMETER (DHADT = 9,         BTR0 = 10)
      PARAMETER (EPOCH0 = 11,       BTE0 = 12)
      PARAMETER (BTR = 13,          BTI = 14)
      PARAMETER (TWOPI = 15,        TSTART = 16)
      PARAMETER (TIMEND = 17,       ELQA1 = 18)
      PARAMETER (ELQA2 = 19,        ELQB1 = 20)
      PARAMETER (ELQB2 = 21,        ZENAT1 = 22)
      PARAMETER (ZENAT2 = 23,       TMER1 = 24)
      PARAMETER (TMER2 = 25,        TIME = 26)
      PARAMETER (BQADOT = 27,       BQBDOT = 28)
      PARAMETER (BQCDOT = 29,       BQD = 30)
      PARAMETER (BQE = 31,          PHSITE = 32)
      PARAMETER (PHSDOT = 33,       TIME1 = 34)
      PARAMETER (PHSDD = 35)
LOCAL END
      PROGRAM M3TAR
C-----------------------------------------------------------------------
C! Reads UV data from MKIII VLBI Haystack "A tapes" in tar archives
C# UV-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2000, 2004, 2007, 2009, 2012, 2015
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
C   M3TAR  Reads uv data from a Haystack MKIII VLBI "A tape" when
C   they come as standard UNIX TAR's.
C   Also expects a text file on disk with experiment data.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name.
C      IN2FILE        IN2FIL        A-file file name
C      INTAPE         ITAPE         Input tape drive number
C      NFILES         NFILES        Number of files to skip on tape
C      REFDATE        REFDAY        Desired reference day dd/mm/yy
C      TIMERANG       XTR           Desired timerange.
C      DOUVCOMP       DOUVC         If true compress data.
C      DOCONCAT       DOCON         If true concatenate to an old file
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM          APARM         Control information.
C-----------------------------------------------------------------------
      INTEGER  IRET
      CHARACTER PRGM*6
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKCLT.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'SRCSEL.INC'
      INCLUDE 'ALIAS.INC'

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'M3TAR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL MK3INN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read data
      CALL MK3UV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write AN, FQ, HI tables.
      CALL MK3TB
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE MK3INN (PRGN, IRET)
C-----------------------------------------------------------------------
C   MK3INN gets input parameters for M3TAR.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => error creating output file.
C                               8 => can't start
C   Input from common
C      XDOCON   R          >0 Concatenate to output file.
C      XDOUVC   R          >0 Compress output file.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      JBUFSZ   I          Buffer size in words.
C      NCFILE   I          No of catalog files marked.
C      NSCR     I          No of scratch files created.
C      RQUICK   L          Restart AIPS ?
C      CLAOUT   C*6        Output file type.
C      INFILE   C*48       Input file name.
C      IN2FIL   C*48       A-file file name.
C      NAMOUT   C*12       Output file name.
C      REFDAY   C*8        Reference day ('dd/mm/yy').
C      CLINC    R          Time interval between CL entries (days).
C      REFJD    D          Reference Julian day no for output file.
C      APARM    R(10)      AIPS input parameter values.
C      XCLAOU   H(2)       Output file type.
C      XINFIL   H(12)      Experiment file name.
C      XIN2FL   H(12)      A-file file name.
C      XNAMOU   H(3)       Output file name.
C      XREFDA   H(2)       Reference date (dd/mm/yy).
C      XTR      R(8)       Time range (start: day, hh, mm, ss,
C                          stop: day, hh, mm, ss).
C      XSRC     H(4, 30)   Source selection list.
C      DISKO    I          Output disk number.
C      DOCON    L          Concatenate output file ?
C      DOUVC    L          Compress output file ?
C      ITAPE    I          Input tape number.
C      NFILES   I          No of files to advance on the input tape.
C      NUMHIS   I          Number of history records.
C      SEQOUT   I          Output file sewquence number.
C      XDISO    R          Output disk number.
C      XNF      R          No of files to advance on input tape.
C      XSOUT    R          Output file sequence number.
C      XTAPE    R          Tape number.
C      ATSEL    R(2)       Input time range wrt ref Jul day no.
C                          (1=start time; 2=stop time).
C      SRCLST   C(30)*16   Source selection list
C      NUMSRC   I          Number of entries in SRCLST
C      DESEL    L          Source selection sense flag
C      MKCLIN   R          Input CL table increment (days).
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKDFBS   L          True if full FBS correction is required.
C      MKDPHA   L          True if FBS phase correction is required.
C      MKDROP   R          Max dropout rate allowed [0,1]
C                          (default 0.5).
C      MKMPER   R          Max parity error rate allowed [0,1]
C                          (default 0.01).
C      MKMXFL   I          Max file no to read from tape.
C      MKNTYP   I          Highest correlation type accepted (now 2).
C      MKPRNT   I          Print level (0..4).
C      DOSIDB   L          If true separate sidebands by IF
C      MSGTXT   C*80       AIPS message string.
C      ICLRNO   I          Next scan number in CL table.
C      ISURNO   I          Next scan number in SU table.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'AFILE.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      ICLRNO = -1
      ISURNO = -1
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Initialise the parameter tables.
      CALL TBINIT
C                                       Get input parameters.
      NPARM = 175
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters.
      ITAPE = IROUND (XTAPE)
      NFILES = IROUND (XNF)
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
      DOUVC = XDOUVC.GT.0.0
      DOCON = XDOCON.GT.0.0
C                                       Convert AIPS character adverbs
C                                       from HOLLERITH to CHARACTER.
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XIN2FL, IN2FIL)
      CALL H2CHR (8, 1, XREFDA, REFDAY)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Set reference JD
      REFJD = -1.0D0
      IF (REFDAY.NE.' ') THEN
         CALL JULDAY (REFDAY, REFJD)
C                                       JULDAY sets its second argument
C                                       to 0.0 if the first argument
C                                       is not in the correct format.
         IF (REFJD.EQ.0.0D0) THEN
            WRITE (MSGTXT, 1001)
            IRET = 1
            GO TO 990
            END IF
         END IF
C                                       Set timerange
      IF ((XTR(5)+XTR(6)+XTR(7)+XTR(8)) .LE. 1.0E-10) XTR(5) = 999.0
      ATSEL(1) = XTR(1) + (XTR(2) / 24.0) + (XTR(3) / (24.0 * 60.0))
     *   + (XTR(4) / (24.0 * 3600.0))
      ATSEL(2) = XTR(5) + (XTR(6) / 24.0) + (XTR(7) / (24.0 * 60.0))
     *   + (XTR(8) / (24.0 * 3600.0))
      IF ((ATSEL(2).LT.1.E-6) .OR. (ATSEL(2).LT.ATSEL(1)))
     *   ATSEL(2) = 1.E5
      IF (ATSEL(1).LT.1.E-6) ATSEL(1) = -1.E5
C                                       Set source selection criteria
      CALL SETSRC (XSRC)
C                                       CL table increment (days)
      CLINC = APARM(1) / (24.0 * 60.0)
      IF (ABS (CLINC) .LE.1.0E-10) CLINC = 2 / (24.0 * 60.0)
      MKCLIN = CLINC
C                                       Max. parity error rate.
      IF (APARM(2).LE.0) APARM(2) = 0.01
      MKMPER = APARM(2)
C                                       Max. dropout rate.
      IF (APARM(3).LE.0) APARM(3) = 0.5
      MKDROP = APARM(3)
C                                       Full FBS correction?
      MKDFBS = APARM(4). LE. 0.5
C                                       Phase-only FBS correction?
      MKDPHA = APARM(4). LE. 1.5
C                                       Max. physical file no to read.
      MKMXFL = APARM(5) + 0.5
C                                       Error suppress (1=skip CRCC,
C                                       2=skip YS/XS,3=skip both)
      MKMASK = 0
      IF (APARM(6).GT.0) MKMASK = APARM(6) + 0.5
C                                       Separate Sidebands?
      DOSIDB = APARM(7) .GT. 0.0
C                                       LO offset
      DOLOFF = APARM(8) .GT. 0
C                                       Print level
      MKPRNT = APARM(9) + 0.5
      IF (MKPRNT .LT. 0) MKPRNT = 0
C                                       Debug dump ?
      MKDBUG = APARM(10) + 0.5
C                                       Control parameters.
C                                       Highest corr type accepted.
      MKNTYP = MXCTYP
C                                       Read experiment file
      CALL MK3EXP (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Read A-file
      AFILEX = .FALSE.
      IF (IN2FIL.NE.' ') CALL MK3AFL (IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MK3INN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1001 FORMAT ('BAD REFDATE FORMAT: USE DD/MM/YY')
      END
      SUBROUTINE MK3EXP (IRET)
C-----------------------------------------------------------------------
C   Reads experiment information, assigns antenna numbers etc. from
C   reading the text experiment file.
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C   Input/output via common
C      INFILE   C*48       Input file name.
C      IATOFF   R          IAT-UTC (seconds).
C      ANTNAM   C*8(ant#)  Antenna name.
C      MKFCOD   C*1(4)     Conversion table for polzn codes in
C                          order (RR,LL,RL,LR).
C      MKAIF    I          Number of IF's in output file.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKNPOL   I          No of polarizations in output file.
C      MKPOL1   I          Min polzn code for output file
C                          (1=RR,2=LL,3=RL,4=LR).
C      MKPOL2   I          Max polzn code for output file.
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      MKDOPL   L          True if multiple polarizations to be read.
C      NANT     I          No of antennas in antenna list.
C      ALIAS    C(2,?)*8   Antenna aliases and translations
C      NALIAS   I          Number of aliases
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER MAXKEY
      PARAMETER (MAXKEY = 43)
      INTEGER   LUN, FIND, KMODE, I, NSTRT, NSTOP, NPARS, J, K, NDATA
      LOGICAL   APPEND
      CHARACTER PARS(MAXKEY)*8, OPTPAR(MAXKEY)*8, VALCH(MAXKEY)*8,
     *   ENDMRK*8, NULL*1, CDUMMY*80
      CHARACTER LSPC8*8, LSCODE(4)*2
      DOUBLE PRECISION VALS(MAXKEY)
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'ALIAS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA APPEND /.FALSE./
      DATA LUN /10/
      DATA    PARS /30*'STAT????','TIMEOFF','NO_LAGS', 'NO_IF',
     *   4*'FREQ????', 4*'STOK??', 'SIDE????', 'DOPOL'/
      DATA    OPTPAR /'ALIASES ', 42*'        '/
      DATA LSPC8 /'        '/
      DATA    VALS, VALCH /MAXKEY*-1.0D0, MAXKEY*'        '/
      DATA    ENDMRK /'/'/
      DATA NPARS /MAXKEY/
      DATA LSCODE /'RR','LL','RL','LR'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Open text file
      CALL ZTXOPN ('READ', LUN, FIND, INFILE, APPEND, IRET)
      IF (IRET.NE.0) GO TO 999
      NULL = CHAR(0)
C                                       Defaults: use UTC
      VALS(31) = 0.0D0
C                                       8 lags
      VALS(32) = 8.0D0
C                                       One IF
      VALS(33) = 0.0D0
C                                       Default is not multi Stokes
      VALS(43) = -1.0D0
C                                       Default Stokes ='LL'
      VALCH(38) = 'LL'
C                                       Read file
      KMODE = 0
      CALL KEYIN (PARS, VALS, VALCH, NPARS, ENDMRK, KMODE, LUN, FIND,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
C                                       Correction to IAT
      IATOFF = VALS(31)/ 86400.0D0
C
      NULL = CHAR (0)
      NANT = 0
      DO 100 I = 1,30
         IF ((VALCH(I).NE.LSPC8).AND.(VALCH(I)(1:1).NE.NULL)) THEN
            NANT = NANT + 1
            ANTNAM(NANT) = VALCH(I)
            END IF
 100     CONTINUE
C
      IF (NANT.LE.0) THEN
         MSGTXT = 'NO ANTENNA NAMES FOUND IN INPUT LIST'
         IRET = 1
         GO TO 990
         END IF
C                                       List antennas found
      MSGTXT = 'Using the following antennas:'
      CALL MSGWRT (6)
      NSTRT = -4
 300  NSTRT = NSTRT + 5
      NSTOP = NSTRT + 4
      NSTOP = MIN (NSTOP, NANT)
      WRITE (MSGTXT,1300) (I,ANTNAM(I),I=NSTRT,NSTOP)
      CALL MSGWRT (6)
      IF (NSTOP.LT.NANT) GO TO 300
C                                       Number of lags
      MKNLAG = VALS(32) + 0.5
C                                       Number of IF's
      MKAIF = VALS(33) + 0.5
C                                       Multi Stokes?
      MKDOPL = VALS(43) .GT. 0.0D0
C                                       Polarization Frequency codes
      IF (MKDOPL .AND. (VALCH(34)(1:1).EQ.' ') .AND.
     *   (VALCH(35)(1:1).EQ.' ') .AND. (VALCH(36)(1:1).EQ.' ') .AND.
     *   (VALCH(37)(1:1).EQ.' ')) THEN
         VALCH(34) = 'R'
         VALCH(35) = 'L'
         VALCH(36) = 'r'
         VALCH(37) = 'l'
         END IF
      MKFCOD(1) = VALCH(34)(1:1)
      MKFCOD(2) = VALCH(35)(1:1)
      MKFCOD(3) = VALCH(36)(1:1)
      MKFCOD(4) = VALCH(37)(1:1)
C                                       Stokes parameters.
      MKPOL1 = 999
      MKPOL2 = -999
      DO 450 I = 38,41
         IF ((VALCH(I).NE.LSPC8) .AND. (VALCH(I)(1:1).NE.NULL)) THEN
            DO 420 J = 1,4
               IF (VALCH(I)(1:2) .EQ. LSCODE(J)) GO TO 430
420            CONTINUE
C                                       Unidentified Stokes parameter.
            WRITE (MSGTXT,1400) VALCH(I)
            IRET = 2
            GO TO 990
C                                       Update Stokes range.
430         MKPOL1 = MIN (MKPOL1, J)
            MKPOL2 = MAX (MKPOL2, J)
            END IF
450      CONTINUE
C                                       List Stokes parameters found.
      MKNPOL = 0
      IF ((MKPOL1 .NE. 999) .AND. (MKPOL2 .NE. -999)) THEN
         MKNPOL = MKPOL2 - MKPOL1 + 1
         WRITE (MSGTXT,1450) (LSCODE(K), K = MKPOL1,MKPOL2)
         CALL MSGWRT (6)
         END IF
C                                       Double or single sideband.
      MKSIDB = -999
      IF (VALCH(42) .EQ. 'DOUBLE') MKSIDB = 0
C                                       Loop over optional records.
      NALIAS = 0
  500 CONTINUE
         CALL DFILL (MAXKEY, -1.0D0, VALS)
         NDATA = MAXKEY
         CALL KEYIN (OPTPAR, VALS, VALCH, NDATA, ENDMRK, 1, LUN, FIND,
     *      IRET)
         IF (IRET.EQ.1) GO TO 600
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1000)
            GO TO 990
            END IF
         IF (VALS(1).NE.-1.0D0) THEN
C                                       Read aliases
            DO 510 I = 1, MAXKEY
               VALCH(I) = ' '
  510          CONTINUE
            NDATA = 40
            CALL KEYIN (CDUMMY, VALS, VALCH, NDATA, ENDMRK, 3, LUN,
     *         FIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1000)
               GO TO 990
               END IF
            IF (NDATA.GT.(2*MAXALI)) THEN
               WRITE (MSGTXT, 1500) MAXALI
               IRET = 5
               GO TO 990
               END IF
            IF (MOD(NDATA, 2).NE.0) THEN
               WRITE (MSGTXT, 1501)
               IRET = 5
               GO TO 990
               END IF
            IF (NALIAS.NE.0) THEN
               WRITE (MSGTXT, 1502)
               CALL MSGWRT (5)
               END IF
            J = 1
            NALIAS = NDATA/2
            DO 520 I = 1, NALIAS, 1
               IF ((VALCH(J).EQ.' ').OR.(VALCH(J+1).EQ.' ')) THEN
                  WRITE (MSGTXT, 1503)
                  IRET = 5
                  GO TO 990
                  END IF
               ALIAS(1, I) = VALCH(J)
               ALIAS(2, I) = VALCH(J + 1)
               J = J + 2
  520          CONTINUE
C                                       Extra optional cards go in
C                                       as ELSE IF statements here
         ELSE
            WRITE (MSGTXT, 1504)
            CALL MSGWRT (5)
            GO TO 600
            END IF
         GO TO 500
C
  600 CONTINUE
C                                       Close text file
      CALL ZTXCLS (LUN, FIND, IRET)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('KEYIN: ERROR READING INFILE TEXT FILE')
 1300 FORMAT (5(I2,':',A8))
 1400 FORMAT ('MK3EXP: Unidentified Stokes parameter: ',A8)
 1450 FORMAT ('Accept Stokes: ',4(A2,3X))
 1500 FORMAT ('MK3EXP: TOO MANY ALIASES (<', I2, ')')
 1501 FORMAT ('MK3EXP: BAD ALIAS TABLE (ODD NUMBER OF STRINGS)')
 1502 FORMAT ('WARNING: extra alias table ignored')
 1503 FORMAT ('MK3EXP: BAD ALIAS TABLE (MISSING QUOTES?)')
 1504 FORMAT ('MK3EXP: Ignoring junk at end of input file')
      END
      SUBROUTINE MK3AFL (IRET)
C-----------------------------------------------------------------------
C   Reads A-file (text) and loads the UNIX filename into an array
C   used for deciding which scans to accept from the A-tapes.
C   The UNIX filename is unique, but we need the extent no
C   as well for ATHDR.
C   The tape number is not longer supported by the new format.
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C   Input/output via common
C      IN2FIL   C*48       Input file name.
C      ROOT     C*100(*)   File name to accept
C      ROOTID   C*6(*)     Root Id code to accept
C      EXTENT   I(*)       Extent number of type 51 to accept.
C      ROOTNM   I          # scans in A-file
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER  UEXT
      INTEGER   LUN, FIND, ILINE
      CHARACTER LINE*512
      CHARACTER CTEMP2*6, UNAME*100
      INCLUDE 'MK3IN.INC'
      INCLUDE 'AFILE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /10/
C-----------------------------------------------------------------------
      IRET = 0
      ATAPEN = ' '
C                                       Open text file
      CALL ZTXOPN ('READ', LUN, FIND, IN2FIL, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Read and decode UNIX filename
      ILINE = 0
C
 100  CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         ROOTNM = ILINE
         GO TO 900
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Ignore comment lines
      IF ((LINE(1:1).EQ.' ') .OR. (LINE(1:1).EQ.'*')) GO TO 100
C                                       Reject other versions than 4
C                                       We deal with newer versions
C                                       in the future
      IF (LINE(1:1).NE.'4') THEN
         IRET = 9
         WRITE (MSGTXT,1010)
         GO TO 990
         END IF
C
C     We should do the convertion from V=>4 lines here in case
C     the equivalent to the AFILE task is not used.
C     Up to now the line is just plain text of type 2.
C     The UNIX filename for the according file is much more kryptic
C     and should only used from here.
C
      ILINE=ILINE + 1
C                                       Convert line to UNIX filename
      CALL CAF2UX(LINE,UNAME,UEXT,CTEMP2,IRET)
C                                Store UNIX filename (which we don't
C                                really need), parent extent no ,
C                                and root id code
      ROOT(ILINE) = UNAME
      EXTENT(ILINE) = UEXT
      ROOTID(ILINE) = CTEMP2
C
      GO TO 100
C                                       Close text file
 900  CALL ZTXCLS (LUN, FIND, IRET)
      IF (ROOTNM.GT.0) AFILEX = .TRUE.
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MK3AFL: ERROR ',I3,' READING A-FILE')
 1010 FORMAT ('MK3AFL: MUST BE VERSION 4 OR LATER!')
      END
      SUBROUTINE MK3UV (IRET)
C-----------------------------------------------------------------------
C   MK3UV reads uv data one point at a time from MK3DAT and then writes
C   the data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C   Input from common
C      CURSOU   I          Current source number in SU table.
C      CURINT   R          Data integration time (seconds)
C      FILSIZ   I          Output file size.
C      FSTVIS   I          First visibility no in output file.
C      ILOCB    I          Offset from start of vis rec for baseline.
C      ILOCFQ   I          Offset from start of vis rec for freq id.
C      ILOCSU   I          Offset from start of vis rec for source id
C      LREC     I          Length in values of a visibility record.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      MSGTXT   C*80       AIPS message string.
C      CNTVIS   I          Number of visibilities in output file
C      ILOCT    I          Offset from start of vis record for Time.
C      ILOCU    I          Offset from start of vis record for U.
C      ILOCV    I          Offset from start of vis record for V.
C      ILOCW    I          Offset from start of vis record for W.
C      NRPARM   I          Number of random parameters.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IPTRO, LUNO, IA1, IA2, NIOUT, INDO, KBIND, NUMVIS,
     *   NVS, VISBUF, IFQAID, IAREF, IDUM
      REAL      DUM
      INCLUDE 'MK3IN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO /16/
C-----------------------------------------------------------------------
      NIOUT = 0
      NUMVIS = 0
      CNTVIS = 0
      IPTRO = 1
      ILOCV = 0
      ILOCU = 0
      ILOCW = 0
      ILOCT = 0
      NRPARM = 0
C                                       Loop
 100  CONTINUE
         NUMVIS = NUMVIS + 1
C                                      Next vis
         CALL MK3DAT (NUMVIS, BUFFER(IPTRO+ILOCU),
     *      BUFFER(IPTRO+ILOCV), BUFFER(IPTRO+ILOCW),
     *      BUFFER(IPTRO+ILOCT), IA1, IA2, IAREF, IFQAID,
     *      BUFFER(IPTRO+NRPARM), BUFFER(IPTRO), IRET)
C
C                                       Branch on his return
         IF (IRET) 200, 130, 120
C                                       Error (fatal)
 120        WRITE (MSGTXT,1120) IRET
            CALL MSGWRT (8)
            GO TO 200
C                                       Got datum
 130     CNTVIS = CNTVIS + 1
C                                       Startup
         IF (NUMVIS.EQ.1) THEN
C                                       Create/init output file
            CALL MK3OPN (LUNO, INDO, VISBUF, IPTRO, IRET)
            IF (IRET.NE.0) GO TO 999
            CNTVIS = FSTVIS
C                                       Next visibility
            GO TO 100
            END IF
C                                       Fill in baseline, source, FQ id
         IF (ILOCB.GE.0) THEN
            BUFFER(IPTRO+ILOCB) = IA1 * 256 + IA2 + 0.001 * (IAREF - 1)
         ELSE
            BUFFER(IPTRO+ILOCA1) = IA1
            BUFFER(IPTRO+ILOCA2) = IA2
            BUFFER(IPTRO+ILOCSA) = 1
            END IF
         BUFFER(IPTRO+ILOCSU) = CURSOU
         BUFFER(IPTRO+ILOCFQ) = IFQAID
         BUFFER(IPTRO+ILOCIT) = CURINT
C                                       Update pointer, counter
         IPTRO = IPTRO + LREC
         NIOUT = NIOUT + 1
C                                       Time to write
         IF (NIOUT.GE.VISBUF) THEN
C                                       Expand file?
            IF (CNTVIS.GT.FILSIZ) THEN
C                                       Add 1000 vis.
               NVS = CNTVIS + 1000
               CALL MK3XPN (LUNO, NVS, IRET)
C                                        Say how much data
               WRITE (MSGTXT,1200) NUMVIS
               CALL MSGWRT (6)
               IF (IRET.NE.0) THEN
C                                       Expansion failed - save what
C                                       you've got.
                  MSGTXT = 'FILE EXPANSION FAILED - DISK FULL'
                  CALL MSGWRT (8)
                  CNTVIS = CNTVIS - NIOUT
                  NIOUT = 0
                  IRET = 0
                  GO TO 200
                  END IF
               END IF
C                                       Write vis record.
            CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
C                                       Check for end.
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            VISBUF = NIOUT
            NIOUT = 0
            END IF
C                                       Next vis.
         GO TO 100
C                                       Final call to MK3DAT.
 200     IRET = 0
C
         CALL MK3DAT (-1, DUM, DUM, DUM, DUM, IA1, IA2, IDUM, IDUM,
     *      BUFFER, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            CALL MSGWRT (8)
            IRET = 0
            END IF
C                                        Say how much data
      WRITE (MSGTXT,1200) NUMVIS
      CALL MSGWRT (6)
C                                       Close down
      CALL MK3CLS (LUNO, INDO, KBIND, NIOUT, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('MK3UV: MK3DAT ERROR',I3)
 1150 FORMAT ('MK3UV: ERROR',I3,' WRITING VIS FILE')
 1200 FORMAT ('Read ', I7, ' visibilities')
      END
      SUBROUTINE MK3OPN (LUNO, INDO, VISBUF, IPTRO, IRET)
C-----------------------------------------------------------------------
C   Creates/ opens/ initializes the output file and writes FQ table
C   Input:
C      LUNO     I    Logical unit number to use for I/O
C   Output:
C      INDO     I    FTAB pointer for I/O
C      VISBUF   I    The number of visibilities that fit in a buffer.
C      IPTRO    I    Pointer in buffer for next record.
C      IRET     I    Return error code, 0=>OK, else failed.
C   Input from common
C      KDCRV    D(7)       Coordinate value at ref pixel.
C      KIIMS    I          Image sequence number.
C      KINAX    I(7)       Number of pixels on each axis.
C      CATBLK   I(256)     Catalog header.
C      CATD     D(128)     Catalog header.
C      JLOCF    I          Order in data of frequency.
C      JLOCIF   I          Order in data of IF.
C      JLOCS    I          Order in data of Stokes parameters.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      JBUFSZ   I          Buffer size in words.
C      OFILE    C*48       Physical file name for output file.
C      FILSIZ   I          Output file size.
C      FSTVIS   I          First visibility no in output file.
C      NCHAN    I          Number of frequency channels.
C      NIF      I          Number of IF's in output file.
C      NPOLN    I          Number of polarizations in output file.
C      REFREQ   D          Reference frequency (Hz) in catalog hdr.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      SEQOUT   I          Output file sewquence number.
C      MSGTXT   C*80       AIPS message string.
C      LREC     I          Length in values of a visibility record.
C-----------------------------------------------------------------------
      INTEGER   LUNO, INDO, VISBUF, IPTRO, IRET
C
      INTEGER   LENBU, NVS, BO
      LOGICAL   T, F
      INCLUDE 'MK3IN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
C                                       Create new header.
      CALL MK3HED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Put new values in CATBLK.
C                                       Create output file.
      CALL MK3CRE (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Reference Frequency
      REFREQ = CATD(KDCRV+JLOCF)
C                                       Number  of frequency channels
      NCHAN = CATBLK(KINAX+JLOCF)
C                                       Number of IFs.
      NIF = CATBLK(KINAX+JLOCIF)
C                                       Number of polarizations
      NPOLN = CATBLK(KINAX+JLOCS)
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CNOOUT, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1002) IRET
         GO TO 990
         END IF
C                                       Get new file size
      CALL ZEXIST (DISKO, OFILE, FILSIZ, IRET)
      IF (IRET.EQ.0) FILSIZ = (FILSIZ * 256.0D0) / LREC
C                                       Expand old files
      IF (FSTVIS.GT.0) THEN
         NVS = FSTVIS + 1000
         CALL MK3XPN (LUNO, NVS, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FILE EXPANSION FAILED - DISK FULL'
            GO TO 990
            END IF
         END IF
C                                       Init vis file for write
      LENBU = 0
      NVS = 10000000
      CALL UVINIT ('WRIT', LUNO, INDO, NVS, FSTVIS, LREC, LENBU,
     *   JBUFSZ, BUFFER, BO, IPTRO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1003) IRET
         GO TO 990
         END IF
      VISBUF = LENBU
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MK3OPN: ERROR',I3,' CREATING OUTPUT FILE')
 1002 FORMAT ('MK3OPN: ERROR',I3,' OPENING OUTPUT FILE')
 1003 FORMAT ('MK3OPN: ERROR',I3,' INITIALIZING I/O TO OUTPUT FILE')
       END
      SUBROUTINE MK3CRE (IRET)
C-----------------------------------------------------------------------
C   Creates the output file, gets info if already exists.
C   Output:
C      IRET     I     Return error code, 0=>OK, else failed.
C   Input from common
C      KHDOB    H(2)       Observation date in format ('DD/MM/YY').
C      KHIMC    H(2)       Image class (6 characters).
C      KHIMN    H(3)       Image name (12 characters).
C      KHPTY    H          Map physical type ('MA','UV').
C      KIGCN    I          No of random parm groups (uv vis records)
C      KIIMS    I          Image sequence number.
C      DOCON    L          Concatenate output file ?
C      MKAIF    I          Number of IF's in output file.
C      MKSJD    D          Scan start time (Jul day no).
C      FREQ     D          Frequency (Hz).
C      LREC     I          Length in values of a visibility record.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      FCNO     I(128)     Catalog slot nos of the marked files.
C      FRW      I(128)     R/W flags for marked catalog files.
C      FVOL     I(128)     Disk nos of the marked cat files.
C      NCFILE   I          No of catalog files marked.
C      SULIST   C(100)*16  SU table index: source name
C      CLAOUT   C*6        Output file type.
C      NAMOUT   C*12       Output file name.
C      OBSDAT   C*8        Reference day ('dd/mm/yy').
C      KHIMCO   =13        Character offset in Hollerith KHIMC.
C      KHIMNO   =1         Character offset in Hollerith KHIMN.
C      KHPTYO   =14        Character offset in Hollerith KHPTY.
C      DAYOFF   I          No days from ref day to current scan.
C      FSTVIS   I          First visibility no in output file.
C      REFJD    D          Reference Julian day no for output file.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      SEQOUT   I          Output file sewquence number.
C      CATBLK   I(256)     Catalog header.
C      CATH     R(256)     Catalog header.
C      ATJDRF   D          Reference Jul day no for output file.
C      MSGTXT   C*80       AIPS message string.
C      IDSOUR   I(100)     SU table index: source number.
C      SUBUFF   I(1024)    SU table buffer.
C      SUKOLS   I(50)      SU table column pointer array.
C      SUNUMV   I(50)      SU table element count in each column.
C      ISURNO   I          Next scan number in SU table.
C      NSOUR    I          No of sources in source table index.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER NAME*12, CLASS*6, VELDEF*8, VELTYP*8, CALCOD*4
      INTEGER   OLDLRL, INOGRP, VER, LUN, LOOP, I, IQUAL, SUFQID
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKTIM.INC'
      REAL      FLUX(4,MXM3FQ)
      DOUBLE PRECISION  OLDFRQ, BANDWW, RAEPO, DECEPO, RAAPP, DECAPP,
     *   EEPOCH,  LSRVEL(MXM3FQ), PMRA, PMDEC, RESTFQ(MXM3FQ),
     *   XFREQ(MXM3FQ), RAOBS, DECOBS
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get naming defaults
      NAME = 'MKIII DATA  '
      CLASS = '      '
      CALL MAKOUT (NAME, CLASS, 0, CLASS, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       File type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
      FSTVIS = 0
      NSOUR = 0
C                                       Create output file.
      CNOOUT = 1
      CALL UVCREA (DISKO, CNOOUT, BUFFER, IRET)
C                                       Check if exists
      IF (DOCON.AND.(IRET.EQ.2)) THEN
C                                       Message about appending data
         WRITE (MSGTXT,1000)
         CALL MSGWRT (6)
C                                       Get old catalog header
         CALL CATIO ('READ', DISKO, CNOOUT, CATBLK, 'WRIT', BUFFER,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
C                                       Number of old vis.
         FSTVIS = CATBLK(KIGCN)
C                                       Consistency check, ref. freq,
C                                       Record size
         OLDFRQ = FREQ
         OLDLRL = LREC
C                                       Reset reference day
         CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
         GO TO 40
      ELSE
         DOCON = .FALSE.
         END IF
C                                       UVCREA error check.
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1003) IRET
         GO TO 990
         END IF
C                                       Mark in /CFILES/
 40   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOOUT
      FRW(NCFILE) = 1
C                                       Set reference JD, DAYOFF
      CALL JULDAY (OBSDAT, REFJD)
      ATJDRF = REFJD
      DAYOFF = MKSJD - REFJD
C                                       tell user reference day.
      MSGTXT = 'Using reference day = ' // OBSDAT
      CALL MSGWRT (6)
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
C                                       SOURCE table
      LUN = 28
      INOGRP = MKAIF
C                                       Velocity reference frame,
C                                       definition
      VELTYP = 'LSR'
      VELDEF = 'OPTICAL'
      SUFQID = -1
      VER = 1
      CALL SOUINI ('WRIT', SUBUFF, DISKO, CNOOUT, VER, CATBLK, LUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Read existing source list
      IF (FSTVIS.GT.0) THEN
         NSOUR = SUBUFF(5)
         DO 100 LOOP = 1,NSOUR
            I = LOOP
            CALL TABSOU ('READ', SUBUFF, I, SUKOLS, SUNUMV,
     *         IDSOUR(LOOP), SULIST(LOOP), IQUAL, CALCOD, FLUX,
     *         XFREQ, BANDWW, RAEPO, DECEPO, EEPOCH, RAAPP, DECAPP,
     *         RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
 100        CONTINUE
         END IF
C                                       Close SU table
      CALL TABIO ('CLOS', 0, ISURNO, SUBUFF, SUBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Process source, antenna and
C                                       run parameters for first scan.
      CALL PROCSB (IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Appending new data to an existing file')
 1001 FORMAT ('ERROR',I3,' READING OLD CATALOG HEADER')
 1003 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE MK3XPN (LUN, NVS, IRET)
C-----------------------------------------------------------------------
C   Expands AIPS output file.
C   Input:
C      LUN      I  LUN of file
C      NVS      I  Total number of vis wanted in file
C   Output:
C      IRET     I  Return code, 0=>OK, else failed
C   Input from common
C      LREC     I          Length in values of a visibility record.
C   Input/output via common
C      OFILE    C*48       Physical file name for output file.
C      FILSIZ   I          Output file size.
C      DISKO    I          Output disk number.
C-----------------------------------------------------------------------
      INTEGER   LUN, NVS, IRET
C
      INTEGER  NREC
      INCLUDE 'MK3IN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Expand file
      NREC = (((NVS - FILSIZ) * LREC * 2) / 512) + 1
      IF (NREC.GT.0) THEN
         CALL ZEXPND  (LUN, DISKO, OFILE, NREC, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Get new file size
      CALL ZEXIST (DISKO, OFILE, FILSIZ, IRET)
      FILSIZ = (FILSIZ * 256.0D0) / LREC
C
 999  RETURN
      END
      SUBROUTINE MK3CLS (LUNO, INDO, KBIND, NIOUT, IRET)
C-----------------------------------------------------------------------
C   Flushes I/O buffers, closes files and tables and updates the number
C   of visibilities in the catalog header.
C   Input:
C      LUNO    I     LUN for I/O
C      INDO    I     FTAB pointer for I/O
C      KBIND   I     buffer pointer
C      NIOUT   I     Number of vis in buffer.
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      KIGCN    I          No of random parm groups (uv vis records)
C      CNTVIS  I     Highest vis number + 1
C      FILSIZ   I          Output file size.
C      LREC     I          Length in values of a visibility record.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      OFILE    C*48       Physical file name for output file.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      CATBLK   I(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   LUNO, INDO, KBIND, NIOUT, IRET
C
      INCLUDE 'MK3IN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Finish write
C                                       Expand file?
      IF (CNTVIS.GT.FILSIZ) THEN
         CALL MK3XPN (LUNO, CNTVIS, IRET)
         IF (IRET.NE.0) THEN
C                                       Expansion failed - save what
C                                       you've got.
            MSGTXT = 'FILE EXPANSION FAILED - SOME DATA LOST'
            CALL MSGWRT (8)
            IRET = 0
C                                       Some data lost in buffer
            CNTVIS = CNTVIS - ABS (NIOUT) - 1
            GO TO 20
            END IF
         END IF
      NIOUT = - ABS (NIOUT)
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER(1), NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSH'
         GO TO 990
         END IF
C                                       Compress output file.
      CALL UCMPRS (CNTVIS, DISKO, CNOOUT, LUNO, CATBLK, IRET)
C                                       Close file
 20      CALL ZCLOSE (LUNO, INDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOS'
            GO TO 990
            END IF
C                                      Put vis. count in CATBLK
         CATBLK(KIGCN) = CNTVIS
         CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', BUFFER,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MK3CLS: ERROR ',I3,1X,A,'ING OUTPUT')
 1001 FORMAT ('MK3CLS: ERROR ',I3,' UPDATING OUTPUT CATALOG HEADER')
      END
      SUBROUTINE MK3CAL (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill CL table.
C   Output:
C      IRET     I     Return code, 0=>OK, else TABCAL or CALINI error.
C   Input from common
C      DBLANK   D          Magic value (= indeterminate).
C      FBLANK   R          REAL value indicating blanking.
C      DAYOFF   I          No days from ref day to current scan.
C      IATOFF   R          IAT-UTC (seconds).
C      MKATM    R(parm#,    Neutral atmosphere model. parm#: 1=delay
C               ant#,cl#)   (us); 2=rate (us/s); 3=acc (us/s/s).
C      MKGEO    D(parm#,   Antenna based geometric model by ant#
C               ant#,cl#)  and CL table index. Parm# 1,2,3 =
C                          (delay (us), rate (us/s), acc (us/s/s))
C                          respectively.
C      MKTMCL   D(cl#)     UTC times for CL table entries for current
C                          scan (days).
C      MKNTMC   I          Number of CL table entry times per
C                          scan/antenna/IF.
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      MKAFNO   I(fqid#,   AIPS IF no for freq# of frequency
C               freq#)     id fqid#.
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKPCSC   R(parm#,    Scan-averaged phase cal values. parm#:
C               ant#,freq#) 1=Re(RCP);2=Im(RCP);3=Re(LCP);4=Im(LCP).
C      MKNPOL   I          No of polarizations in output file.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKNBL    I          No of baselines in antenna table MKBL.
C      ANTCLK   D(2,base#) Offset clock rate (ref,rem stn) (s/s)
C                          > 0 = fast.
C      ANTEPO   D(2,base#) Antenna clock ref epoch (s since BOY).
C      ANTSYN   D(2,base#) Clock sync (ref,rem stn) (us) >0 = fast.
C      NANT     I          No of antennas in antenna list.
C      MKPRT    D(base#)   Processing reference time (PRT) as used by
C                          MKIII correlator (ms since BOY). Should
C                          equal a priori epoch.
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C      STKPFQ   I(fqid#,   Pointer for connecting freq to stokes for
C               freq#)     Phase-cal match of LSB <- USB
C   Input/output via common
C      CURSOU   I          Current source number in SU table.
C      NIF      I          Number of IF's in output file.
C      NPOLN    I          Number of polarizations in output file.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      CATBLK   I(256)     Catalog header.
C      CLBUFF   I(1024)    CL table buffer.
C      CLKOLS   I(50)      CL table column pointer array.
C      CLNUMV   I(50)      CL table element count in each column.
C      ICLRNO   I          Next scan number in CL table.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   VER, IANT, SUBA, LUN, NUMANT, NUMPOL, IIF, ITIME, IFP,
     *   IMK3FQ, LIM, FREQID, IAA, J, K, IFQID, ISIDEB, IBASE, IP1, IP2
      REAL      GMMOD, DT, FAROT
      LOGICAL   BADANT, WFOUND
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCLT.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKSTA.INC'
      INTEGER   REFA(2,MXM3FQ), MXM3X2, IMK, JSTOKE
      PARAMETER (MXM3X2 = 2*MXM3FQ)
      REAL      DOPOFF(MXM3FQ), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MXM3FQ), CIMAG(2,MXM3FQ),
     *   CDELAY(2,MXM3FQ), CRATE(2,MXM3FQ), WEIGHT(2,MXM3FQ)
      DOUBLE PRECISION    GEODLY(3)
      DOUBLE PRECISION    TIME8, TTT, FREQT
      INTEGER IANTCL(MXANT), NANTCL, NTERM, USBUSE(MXM3FQ)
      INCLUDE 'MK3IN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VER /1/
      DATA GMMOD /1.0/
      DATA REFA /MXM3X2*0/
      DATA DOPOFF/MXM3FQ*0.0/
      DATA GEODLY /3*0.0D0/
      DATA  MBDELY, CLOCK, DCLOCK /6*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, CDELAY, CRATE, WEIGHT /MXM3X2*0.0,
     *   MXM3X2*0.0, MXM3X2*0.0, MXM3X2*1.0/
      DATA CREAL /MXM3X2*1.0/
C-----------------------------------------------------------------------
      SUBA = 1
C                                       Give maximum antenna number
      LUN = 29
      VER = 1
      NUMANT = NANT
      NUMPOL = MIN (NPOLN, 2)
      IP1 = 1
      IP2 = 2
      IF (NUMPOL.EQ.1) IP2 = 1
      DT = MKTMCL(2) - MKTMCL(1)
      NTERM = 3
C                                       Open table
      CALL CALINI ('WRIT', CLBUFF, DISKO, CNOOUT, VER, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop over times
      DO 400 ITIME = 1,MKNTMC
         TIME8 = MKTMCL(ITIME) + IATOFF + DAYOFF
         NANTCL = 0
C                                       Loop over baselines
         DO 200 IBASE = 1,MKNBL
            IF ((MKBL(1,IBASE).LE.0).OR.(MKBL(2,IBASE).LE.0)) GO TO 200
C                                       Make look up table to find USB
C                                       corresponding to LSB IFs.
            CALL FILL (MXM3FQ, 0, USBUSE)
            IF (DOSIDB) THEN
               IFQID = MKBFQ(IBASE)
               LIM = MKNFQT(IFQID)
               DO 40 J = 1,LIM
                  IF (MKFQT(IFQID,J) .LT. 0) THEN
C                                        Find corresponding USB IF
                     FREQT = ABS (MKFQT(IFQID,J)) + LSBOFF
                     JSTOKE = STKPFQ(IFQID,J)
C                                        Lookup
                     DO 30 K = 1,LIM
                        IF ( (ABS(FREQT-MKFQT(IFQID,K)).LE.100.0D0)
     $                       .AND. (JSTOKE .EQ. STKPFQ(IFQID,K)) )
     *                     USBUSE(J) = K
 30                     CONTINUE

                  ELSE
                     USBUSE(J) = J
                     END IF
 40               CONTINUE
               END IF
C                                       Loop over antenna
            DO 180 IAA = 1,2
               IANT = MKBL(IAA,IBASE)
C                                       Avoid writing redundant CL
C                                       antenna entries for this time.
            WFOUND = .FALSE.
            DO 50 J = 1,NANTCL
               IF (IANTCL(J) .EQ. IANT) WFOUND = .TRUE.
  50           CONTINUE
            IF (.NOT. WFOUND) THEN
               NANTCL = NANTCL + 1
               IANTCL(NANTCL) = IANT
               END IF
C                                       Data for this antenna?
               BADANT = (MKGEO(1,IANT,1) .EQ. DBLANK) .OR. WFOUND
C                                       Phase cals
               DO 90 IIF = 1,NIF
                  CREAL(1,IIF) = FBLANK
                  CIMAG(1,IIF) = FBLANK
                  CREAL(2,IIF) = FBLANK
                  CIMAG(2,IIF) = FBLANK
 90               CONTINUE
               GEODLY(1) = DBLANK
               GEODLY(2) = DBLANK
               GEODLY(3) = DBLANK
               IF (.NOT. BADANT) THEN
C                                       Geometric model
                  GEODLY(1) = MKGEO(1,IANT,ITIME)
                  GEODLY(2) = MKGEO(2,IANT,ITIME)
                  GEODLY(3) = 0.5D0 * MKGEO(3,IANT,ITIME)
C                                       Atmosphere
                  ATMOS = MKATM(1,IANT,ITIME)
                  DATMOS = MKATM(2,IANT,ITIME)
C                                       Clocks
                  TTT = ((MKTMCL(ITIME) - MKTIMC(2,IBASE)) *
     *               86400.0D0) + ((MKPRT(IBASE) * 1.0D-3) -
     *               ANTEPO(IAA,IBASE))
                  CLOCK(IP1) = ANTSYN(IAA,IBASE) * 1.0D-6 +
     *               (ANTCLK(IAA,IBASE) * TTT)
                  DCLOCK(IP1) = ANTCLK(IAA,IBASE)
                  CLOCK(IP2) = CLOCK(IP1)
                  DCLOCK(IP2) = DCLOCK(IP1)
                  IFQID = MKBFQ(IBASE)
                  LIM = MKNFQT(IFQID)
                  DO 100 IMK3FQ = 1,LIM
                     IFP = MKAFNO(IFQID,IMK3FQ)
                     IF ((IFP.GT.NIF).OR.(IFP.LE.0)) GO TO 100
C                                       Determine the sideband.
                     ISIDEB  = 1
                     IF (MKFQT(IFQID,IMK3FQ) .LT. 0) ISIDEB = -1
C                                       Phase cal. USB only.
                     IMK = IMK3FQ
                     IF (ISIDEB.LT.0) IMK = USBUSE(IMK3FQ)
                     IF ((MKPCSC(1,IANT,IMK).NE.FBLANK) .AND.
     *                  (IMK.GT.0)) THEN
                        CREAL(IP1,IFP) = MKPCSC(1,IANT,IMK)
                        CIMAG(IP1,IFP) = -MKPCSC(2,IANT,IMK)
                        END IF
                     IF ((MKPCSC(3,IANT,IMK).NE.FBLANK) .AND.
     *                  (IMK.GT.0)) THEN
                        CREAL(IP2,IFP) = MKPCSC(3,IANT,IMK)
                        CIMAG(IP2,IFP) = -MKPCSC(4,IANT,IMK)
                        END IF
C                                       No phase cal possible for LSB.
                     IF (MKSIDB.EQ.-1) THEN
                        CREAL(IP1,IFP) = 1.0
                        CIMAG(IP1,IFP) = 0.0
                        CREAL(IP2,IFP) = 1.0
                        CIMAG(IP2,IFP) = 0.0
                        END IF
 100                 CONTINUE
C                                       Write record
                  FREQID = IFQID
                  FAROT = 0.0
                  CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *               NUMPOL, NIF, TIME8, DT, CURSOU, IANT, SUBA, FREQID,
     *               FAROT, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *               CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, CDELAY,
     *               CRATE, WEIGHT,  REFA, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 180           CONTINUE
 200        CONTINUE
 400     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IRET)
C
 999  RETURN
      END
      SUBROUTINE HFDUMP (IRET)
C-----------------------------------------------------------------------
C   Dumps valid FRNGE solutions to HF table.  Uses same buffers as
C   writing the CL table.
C   Input in common:
C      M3GOT    L(*)   If element i is true then write corresponding
C                      entry to table.
C      Table entries   FRNGE solution parameters are passes in common in
C                      local include MKFRNG.INC with coded names giving
C                      the data type and table column.  The comments in
C                      TABMX describe these.
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LOOP, IHFRNO, HFKOLS(77), HFNUMV(77), VER, LUN, I
      LOGICAL   GOOD
      INCLUDE 'MK3IN.INC'
      INCLUDE 'MKFRNG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Any solutions?
      GOOD = .FALSE.
      DO 10 LOOP = 1,MAXM3X
         GOOD = GOOD .OR. M3GOT(LOOP)
 10      CONTINUE
      IF (.NOT.GOOD) GO TO 999
C                                       Open table
      VER = 1
      LUN = 29
      CALL HFINI ('WRIT', CLBUFF, DISKO, CNOOUT, VER, CATBLK, LUN,
     *   IHFRNO, HFKOLS, HFNUMV, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       Write entries
      DO 100 I = 1,MAXM3X
         IF (M3GOT(I)) CALL TABHF ('WRIT', CLBUFF, IHFRNO, HFKOLS,
     *      HFNUMV, CC1(I), IC2(1,I), IC3(1,I), IC4(1,I), IC5(I),
     *      IC6(I), IC7(I), IC8(I), IC9(1,I), IC10(1,I), IC11(I),
     *      IC12(1,I), IC13(I), IC14(I), IC15(I), IC16(I), IC17(I),
     *      IC18(1,I), IC19(1,I), IC20(1,I), IC21(1,I), IC22(1,I),
     *      IC23(I), IC24(I), CC25(I), CC26(I), CC27(I), CC28(I),
     *      CC29(I), CC30(I), CC31(I), CC32(I), CC33(I), CC34(I),
     *      CC35(I), CC36(I), CC37(I), CC38(I), DC39(1,I), DC40(I),
     *      DC41(I), DC42(I), DC43(I), DC44(I), DC45(I), DC46(I),
     *      DC47(I), DC48(I), DC49(I), DC50(I), RC51(1,I), RC52(1,I),
     *      RC53(I),  RC54(I), RC55(I), RC56(I), RC57(I), RC58(I),
     *      RC59(1,I), RC60(1,I), RC61(I), RC62(1,I), RC63(1,I),
     *      RC64(I), RC65(I), RC66(I), RC67(I), RC68(I), RC69(I),
     *      RC70(I), RC71(I), RC72(I), RC73(I), RC74(I), RC75(I),
     *      RC76(I), RC77(I), IRET)
         IF (IRET.GT.0) GO TO 999
         M3GOT(I) = .FALSE.
 100     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, IHFRNO, CLBUFF, CLBUFF, IRET)
C
 999  RETURN
      END
      SUBROUTINE MK3TB
C-----------------------------------------------------------------------
C   MK3TB creates and fills a history file. Also write AN table
C   and prints the correlator model parameters.
C   Input from common
C      HISCRD   C(10)*64   History records.
C      INFILE   C*48       Input file name.
C      OBSDAT   C*8        Reference day ('dd/mm/yy').
C      RLSNAM   C*8        Release name.
C      KHOBS    H(2)       Observer name.
C      KHTEL    H(2)       Telescope name (eg 'VLA').
C      KIGCN    I          No of random parm groups (uv vis records)
C      APARM    R(10)      AIPS input parameter values.
C      NUMHIS   I          Number of history records.
C      NLUSER   I          User number.
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      CLAOUT   C*6        Output file type.
C      NAMOUT   C*12       Output file name.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      SEQOUT   I          Output file sewquence number.
C      CATBLK   I(256)     Catalog header.
C      CATH     R(256)     Catalog header.
C      ATSEL    R(2)       Input time range wrt ref Jul day no.
C                          (1=start time; 2=stop time).
C      MSGTXT   C*80       AIPS message string.
C      TSKNAM   C*6        AIPS task name.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8, ATIME*8, ADATE*12, TELE*8, OBSR*8
      INTEGER   LUN, I, IERR, TIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, CNOOUT, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       New history
      WRITE (HILINE,1011) TSKNAM, INFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Number of visibilities
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       AIPS release
      WRITE (HILINE,2003) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Reference date
      WRITE (HILINE,2004) TSKNAM, OBSDAT
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANGE
      CALL HITIME (ATSEL(1), ATSEL(2), LUN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       APARM
      WRITE (HILINE,2006) TSKNAM, APARM(1)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2007) TSKNAM, APARM(2)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2008) TSKNAM, APARM(3)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2009) TSKNAM, APARM(4)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2010) TSKNAM, APARM(5)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2011) TSKNAM, APARM(6)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2012) TSKNAM, APARM(7)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1050) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Print corr. parameters.
      CALL PRTPAR
C                                       Write ANtenna file.
      CALL MK3ANT
C                                       Write FQ table
      CALL MK3FQ (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2020) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MK3TB: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'INFILE =''',A,'''')
 1050 FORMAT (A6,' /')
 2001 FORMAT (A6,' / Number of visibilities copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT (A6,' Release = ''',A7,' ''')
 2004 FORMAT (A6,' REFDATE = ''',A7,' '' / Reference day for times')
 2006 FORMAT (A6,' APARM(1) = ',F10.3, ' / CL table incr. (min)')
 2007 FORMAT (A6,' APARM(2) = ',F10.3, ' / Max parity error rate')
 2008 FORMAT (A6,' APARM(3) = ',F10.3, ' / Max dropout rate')
 2009 FORMAT (A6,' APARM(4) = ',F10.0, ' / .ge. 1 => no FBS correction')
 2010 FORMAT (A6,' APARM(5) = ',F10.0, ' / No of files read from tape')
 2011 FORMAT (A6,' APARM(6) = ',F4.0,' /Ignore: 1=CRCC;2=XS,YS;3=both')
 2012 FORMAT (A6,' APARM(7) = ',F4.0,' / >0 => split sidebands in IF')
 2020 FORMAT ('MK3TB: ERROR',I3,' WRITING FQ TABLE')
      END
      SUBROUTINE MK3ANT
C-----------------------------------------------------------------------
C   MK3ANT creates and fills the antenna file.
C   Input from common
C      DTALOC   D(3,ant#)  X,Y,Z coordinates for antenna ant# using
C                          left-handed axes.
C      OBSDAT   C*8        Reference day ('dd/mm/yy').
C      IATOFF   R          IAT-UTC (seconds).
C      ANTNAM   C*8(ant#)  Antenna name.
C      NANT     I          No of antennas in antenna list.
C      ATJDRF   D          Reference Jul day no for output file.
C      DTGAST   D(day#)    GAST at 0 UT on day# (rad).
C      DTSIDD   D(day#)    d(GAST)/d(UT) on day# (rad/s).
C      DTUT1    D(day#)    UT1-UTC (s) on day#.
C      DTWOB    D(2,day#)  X(=1),Y(=2) pole posn on day# (arcsec).
C      NTJULD   I          No of entries in DTJULD.
C      FREQ     D          Frequency (Hz).
C   Input/output via common
C      CLBUFF   R(UVBFSS)  Work buffer.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      CATBLK   I(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C      DTJULD   D(day#)    Julian day no for run based parameters.
C-----------------------------------------------------------------------
      INCLUDE 'MK3IN.INC'
      DOUBLE PRECISION DMIN, DELTAT, DEGRD, RJD
      INTEGER   IERR, VER, LUN, I, IREFD, IDMIN, NKEY, LOCS(1), VALS(2),
     *   KEYTYP(1)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (NANT.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
C                                       Find date nearest to CATBLK
C                                       ref date in run parameter table.
         DMIN = 1.0D10
         IREFD = 0
         DO 10 I = 1,NTJULD
            DELTAT = ABS (ATJDRF - DTJULD(I))
            IF (DELTAT .LT. DMIN) THEN
               DMIN = DELTAT
               IREFD = I
               END IF
10          CONTINUE
         IF (IREFD .EQ. 0) GO TO 999
C                                       Set AN table reference date.
C                                       Should equal CATBLK ref date.
         IF (DMIN .GT. 0.5) THEN
            CALL GREG (DTJULD(IREFD), RDATE)
         ELSE
            RDATE = OBSDAT
            END IF
C                                       Position of the earth's pole
         POLRXY(1) = DTWOB(1,IREFD)
         POLRXY(2) = DTWOB(2,IREFD)
         UT1UTC = DTUT1(IREFD)
         DATUTC = IATOFF * 86400.0
C                                       Array name
         ANAME = 'VLBI'
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
         GSTIA0 = DTGAST(IREFD)
         DEGRD = 180.0D0 / 3.141592653589793D0
         GSTIA0 = DTGAST(IREFD) * DEGRD
         GSTIA0 = MOD (GSTIA0, 360.0D0)
         IF (GSTIA0 .LT. 0.0) GSTIA0 = GSTIA0 + 360.0D0
         DEGPDY = DTSIDD(IREFD) * 86400.0D0 * DEGRD
         SAFREQ = FREQ
         ANTNIF = MKAIF
         ANFQID = -1
         VER = 1
C                                       Which time system?
         IF (ABS (DATUTC).GT.0.1) THEN
            TIMSYS = 'IAT'
         ELSE
            TIMSYS = 'UTC'
            END IF
C                                       Create/init file
         CALL ANTINI ('WRIT', CLBUFF, DISKO, CNOOUT, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Check AN table ref. date
         NKEY = 1
         KEYTYP(1) = 3
         CALL TABKEY ('READ', 'RDATE   ', NKEY, CLBUFF, LOCS, VALS,
     *      KEYTYP, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL H2CHR (8, 1, VALS(LOCS(1)), RDATE)
         CALL JULDAY (RDATE, RJD)
         DMIN = ABS (ATJDRF - RJD)
         IF (DMIN.GT.0.5) THEN
C                                       Reference date in AN file does
C                                       not agree with that in the main
C                                       file - issue a warning.
            IDMIN = DMIN + 0.5D0
            MSGTXT = '************************************************'
            CALL MSGWRT (5)
            WRITE (MSGTXT,1010) IDMIN
            CALL MSGWRT (5)
            MSGTXT = 'Be advised to rerun M3TAR with correct REFDATE'
            CALL MSGWRT (5)
            MSGTXT = '************************************************'
            CALL MSGWRT (5)
            END IF
C                                       init basic AN record
         ANNAME = '        '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (2*ANTNIF, 0.0, POLCA)
         CALL RFILL (2*ANTNIF, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         CALL RFILL (ANTNIF, 0.0, FWHMAN)
C                                       AN records
         IANRNO = 1
         DO 20 I = 1,NANT
            IF ( .NOT. DOCON .OR. DTALOC(1,I) .NE. DBLANK ) THEN
               STAXYZ(1) = DTALOC(1,I)
               STAXYZ(2) = DTALOC(2,I)
               STAXYZ(3) = DTALOC(3,I)
               NOSTA = I
               ANNAME = ANTNAM(I)
               CALL TABAN ('WRIT', CLBUFF, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *            DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *            POLCB, IERR)
               IF (IERR.NE.0) GO TO 990
            ELSE
               IANRNO = IANRNO + 1
               END IF
 20         CONTINUE
C                                       Close
         CALL TABIO ('CLOS', 1, IANRNO, CLBUFF, CLBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('WARNING: Reference day in AN table changed by',I5,
     *   ' days')
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA FILE')
      END
      SUBROUTINE MK3HED (IRET)
C-----------------------------------------------------------------------
C   MK3HED is a routine in which the catalog header is constructed.
C   Output:
C      IRET           I   Return error code, 0=>OK, otherwise abort.
C   Input from common
C      REFDAY   C*8        Reference day ('dd/mm/yy').
C      KDARV    D          Alt ref pixel value (freq or vel).
C      KDCRV    D(7)       Coordinate value at ref pixel.
C      KDODE    D          Antenna pointing (DEC).
C      KDORA    D          Antenna pointing (RA).
C      KDRST    D          Rest frequency of line (Hz).
C      KHBUN    H(2)       Map units (i.e Jy/beam).
C      KHCTP    H(2)(7)    Coordinate type (ie 'RA--SIN').
C      KHDOB    H(2)       Observation date in format ('DD/MM/YY').
C      KHINS    H(2)       Receiver or correlator.
C      KHOBJ    H(2)       Source name.
C      KHOBS    H(2)       Observer name.
C      KHPTP    H(2)(14)   Random parameter types.
C      KHTEL    H(2)       Telescope name (eg 'VLA').
C      KIALT    I          Velocity ref frame (1=LSR,2=Helio,
C                          3=Observer+256 if radio definition.
C      KICTPN   =7         Max number of axes.
C      KIDIM    I          Number of coordinate axes.
C      KIGCN    I          No of random parm groups (uv vis records)
C      KIIMU    I          Image user ID number.
C      KINAX    I(7)       Number of pixels on each axis.
C      KINIT    I          Number of CLEAN iterations.
C      KIPCN    I          Number of random parameters.
C      KIPTPN   =14        Max number of labelled random parameters.
C      KITYP    I          Clean map type (1-4:normal, components,
C                          residual, points; UV = sort order).
C      KRARP    R          Alt ref pixel location (freq or vel).
C      KRBLK    R          Value of indeterminate pixel (real maps).
C      KRBMJ    R          Beam major axis in degrees.
C      KRBMN    R          Beam minor axis in degrees.
C      KRBPA    R          Beam position angle in degrees.
C      KRCIC    R(7)       Coordinate value increment along axis.
C      KRCRP    R(7)       Coordinate reference pixel.
C      KRCRT    R(7)       Coordinate rotation angles.
C      KRDMN    R          Real value of data minimum.
C      KRDMX    R          Real value of data maximum.
C      KREPO    R          Epoch of coordinates (years).
C      KRXSH    R          Offset in X(rot RA) of phase centre.
C      KRYSH    R          Offset in Y(rot DEC) of tangent point.
C      DOUVC    L          Compress output file ?
C      MKAFRQ   I          No of freq channels per spectrum in
C                          output file.
C      MKAIF    I          Number of IF's in output file.
C      MKNPOL   I          No of polarizations in output file.
C      MKPOL1   I          Min polzn code for output file
C                          (1=RR,2=LL,3=RL,4=LR).
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      MKAFQT   D(if#)     AIPS IF frequencies for the current
C                          scan baseline (Hz).
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKEPOC   D(base#)   Coordinate epoch (yr: eg 1950.0, 2000.0).
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C      NLUSER   I          User number.
C      DOSIDB   L          If true separate sidebands by IF
C   Input/output via common
C      OBSDAT   C*8        Reference day ('dd/mm/yy').
C      KLOCWT   I          Offset of compress weight factor in
C                          random parameter array.
C      CATBLK   I(256)     Catalog header.
C      CATD     D(128)     Catalog header.
C      CATH     R(256)     Catalog header.
C      CATR     R(256)     Catalog header.
C      MKSJD    D          Scan start time (Jul day no).
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER RTYPES(12)*8, TYPES(7)*8, UNITS*8
      INTEGER   I, NAXIS, NRAN, NDIM(7), INDEX
      REAL      CRINC(7), CRPIX(7)
      DOUBLE PRECISION CRVAL(7)
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKRFQ.INC'
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                        No. random parameters.
      DATA NRAN /8/
C                                        Rand. parm. names.
      DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN',
     *   'TIME1   ','SUBARRAY','SOURCE  ','FREQSEL',
     *   'INTTIM  ','ANTENNA1','ANTENNA2','WEIGHT','SCALE'/
C                                       Uniform axes.
C                                        No. axes.
      DATA NAXIS /6/
C                                        Axes names.
      DATA TYPES /'COMPLEX ','FREQ    ','STOKES  ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                        Axis dimensions
      DATA NDIM /3,1,1,1,1,1,0/
C                                        Reference values
      DATA CRVAL /1.0D0, -1.0D0, 0.0D0, 1.0D0, 3*0.0D0/
C                                        Reference pixel.
      DATA CRPIX /1.0, 0.5, 5*1.0/
C                                        Coordinate increment.
      DATA CRINC /1.0, -1.0, 0.0, 1.0, 1.0, 1.0, 0.0/
C                                       Units
      DATA UNITS /'CORREL. '/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL CATINI (CATBLK)
C                                       Compressed data?
      IF (DOUVC) THEN
         NRAN = 12
         KLOCWT = 10
         NDIM(1) = 1
      ELSE
         NRAN = 10
         KLOCWT = -1
         NDIM(1) = 3
         END IF
C                                       Actual random parameters.
      DO 20 I = 1,NRAN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
 20      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
C                                       Fill other character strings.
C                                       Observation date. 'DD/MM/YY'
      CALL GREG (MKSJD, OBSDAT)
      IF (REFDAY.NE. '        ') OBSDAT = REFDAY
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = MKEPOC(MKBASE)
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0

C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       Magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C-----------------------------------------------------------------------
C                                       NKAFRQ = Number of freq chan.
C                                       MKNPOL = number of polarization
C                                           correlators.
C                                       OBSDAT = Reference date of time
C                                         tags for data as "dd/mm/yy"
C                                       Insert values in header.
C                                       Number of vis. (Init. 1000)
      CATBLK(KIGCN) = 1000
C                                       Position.
      CATD(KDCRV+4) = 0.0D0
      CATD(KDCRV+5) = 0.0D0
C                                       Number of polarizations.
      IF (MKNPOL.GT.0) THEN
         CATBLK(KINAX+2) = MKNPOL
      ELSE
         MSGTXT = 'MK3HED:STOKES (OR FREQCODE) INADEQUATELY SPECIFIED'
         IRET = 5
         GO TO 990
         END IF
C                                       Stokes parameter observed.
      CATD(KDCRV+2) = -MKPOL1
C                                       Frequency - lowest IF.
      CATD(KDCRV+1) = MKAFQT(1)
C                                       Bandwidth - get from sampling
C                                       rate and number of channels
      CATR(KRCIC+1) = MKSMRT(MKBASE) * 0.5 / MKAFRQ
C                                       Number of frequencies.
      IF (MKAFRQ.GT.0) THEN
         CATBLK(KINAX+1) = MKAFRQ
      ELSE
         MSGTXT = 'MK3HED:NUMBER OF LAGS DESIRED INADEQUATELY SPECIFIED'
         IRET = 5
         GO TO 990
         END IF
      IF ((MKSIDB.EQ.0) .AND. (.NOT.DOSIDB)) THEN
C                                       Double sideband
         CATBLK(KINAX+1) = 2 * MKAFRQ
         CATR(KRCRP+1) = CATR(KRCRP+1) + MKAFRQ
         END IF
C                                       Number of IFS.
      IF (MKAIF.GT.0) THEN
         IF (MKAIF.LE.MAXIF) THEN
            CATBLK(KINAX+3) = MKAIF
         ELSE
            MSGTXT = 'MK3HED: EXCEED MAXIMUM NUMBER OF IFS'
            IRET = 5
            GO TO 990
            END IF
      ELSE
         MSGTXT = 'MK3HED:NUMBER OF IFS INADEQUATELY SPECIFIED'
         IRET = 5
         GO TO 990
         END IF
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, 'MULTI   ', 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, 'VLBI    ', 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, 'MKIII   ', 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, '        ', 1, CATH(KHOBS))
C
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE MK3DAT (NUMVIS, U, V, W, T, IA1, IA2, IAREF, IFQAID,
     *   VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C   Reads data from A tape and translates into the form needed by AIPS.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C                      No data is actually returned if NUMVIS=1 on call
C                      but the first baseline-scan is read and the
C                      relevant info is placed in common to allow
C                      creation of the output file.
C   Output:
C      U       R       U in wavelengths at the reference frequency.
C      V       R       V in wavelengths
C      W       R       W in wavelengths
C      T       R       Time in days since the midnight at the start of
C                      the reference date.
C      IA1     I       Antenna number of the first antenna.
C      IA2     I       Antenna number of the second antenna.
C                      NOTE: IA2 MUST be greater that IA1
C      IAREF   I       Ref antenna (ie one NOT fringe rotated)
C                      1=stn A; 2=stn B. Default is 2 for MKIII.
C      IFQAID  I       Frequency id.
C      RPARM   R       Modified random parameter array. NB U,V,W,
C                      time and baseline should not be modified in RPARM
C      VIS     R(3,*)  Visibilities.
C      IRET    I       Return code  -1 => End of data;
C                             0 => OK
C                            >0 => error, terminate.
C   Input from common
C      FBLANK   R          REAL value indicating blanking.
C      NBITWD   I          # bits /word
C      KINAX    I(7)       Number of pixels on each axis.
C      IATOFF   R          IAT-UTC (seconds).
C      KLOCWT   I          Offset of compress weight factor in
C                          random parameter array.
C      NIF      I          Number of IF's in output file.
C      REFJD    D          Reference Julian day no for output file.
C      DOUVC    L          Compress output file ?
C      ITAPE    I          Input tape number.
C      CATBLK   I(256)     Catalog header.
C      CNTVIS   I          Number of visibilities in output file
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKDATA   R(2,addr#) Visibility data from current scan-baseline
C                          addr# = (time# * Ncorr + corr# - 1) * Nlag
C                             + lag#
C                          time# = (data time - scan start) /int time
C                          corr# = correlator no; Ncorr = No. corr.
C                          lag# = lag no; Nlag = No of lags.
C      MKTOK    L(time#)   Valid visibility data for this time# in
C                          array MKDATA ?
C      MKWT     R(2,addw#) Lag function weight.
C                          addw# = time# * Ncorr + corr#.
C      MKNTIM   I          Max time index time# used in MKDATA.
C      MKAFRQ   I          No of freq channels per spectrum in
C                          output file.
C      MKAIF    I          Number of IF's in output file.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKNTYP   I          Highest correlation type accepted (now 2).
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      MKAFNO   I(fqid#,   AIPS IF no for freq# of frequency
C               freq#)     id fqid#.
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKBAFQ   I(base#)   AIPS IF id for baseline base#.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKSJD    D          Scan start time (Jul day no).
C      MKSUTC   D          Scan start time (hhmmss in days).
C      MKTOFF   D          Time offset of current scan baseline
C                          data from nominal scan start time (days).
C      MKINT    D(base#)   Correlator integration time (days).
C      ICOR0    I          Value of first Stokes value.
C      INCIF    I          Increment in data for IF.
C      INCS     I          Increment in data for Stokes.
C      LREC     I          Length in values of a visibility record.
C      NRPARM   I          Number of random parameters.
C   Input/output via common
C      DAYOFF   I          No days from ref day to current scan.
C      NCHAN    I          Number of frequency channels.
C      NPOLN    I          Number of polarizations in output file.
C      NFILES   I          No of files to advance on the input tape.
C      MKCTIM   I          Current time# on output.
C      MKTYPE   I          Current correlation type being written to
C                          output file.
C      FDVEC    I(50)      File descriptor vector for TAPIO.
C      TBUFF    I(2500)    Tape buffer.
C      MKFILE   I          Current tape file no wrt tape start posn.
C      TBIND    I          Pointer in TBUFF.
C      ATJDRF   D          Reference Jul day no for output file.
C      MSGTXT   C*80       AIPS message string.
C      JLOCS    I          Order in data of Stokes parameters.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, IRET, IFQAID, IAREF
      REAL      U, V, W, T, VIS(3,*), RPARM(*)
C
      INTEGER   MXTVIS
C                                       Double sideband, 28 IFs,
C                                       4 polarizations, 220 channels
C                                       maximum (= 49,280)
      PARAMETER (MXTVIS = 2 * 28 * 4 * 220)
      INTEGER   IFIL, I, J, K, IP, MP, MMP, IFNO, STKOFF, NCOPY,
     *   LIMFQ, IERR, JNCIF, JNCS, IFQID, ISIDEB, ICORR, ISTOKE,
     *   IFREQ
      REAL      TVIS(3,MXTVIS), WT, TUTC
      LOGICAL   GOOD, DSB
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Shutdown?
      IF (NUMVIS.LT.0) THEN
C                                       Close tape
         CALL TAPIO ('CLOS', FDVEC, TBUFF, TBIND, IRET)
         IRET = 0
         GO TO 999
         END IF
C                                       Initialize
      IF (NUMVIS.EQ.1) THEN
C                                       Reference Julian dayno.
         ATJDRF = REFJD
C                                       Initialize tape I/O
C                                       Initialize FDVEC
         CALL FILL (50, 0, FDVEC)
         FDVEC(1) = 129 - ITAPE
C                                       Logical record length for
C                                       TAR is 512 bytes, b u t
C                                       all this stuff seems to be
C                                       organized in blocks of 256 bytes,
C                                       so we keep the blocksize and
C                                       when we deal with the TAR header
C                                       we read twice.
         FDVEC(2) = 256
C                                       Buffer size for TAR's
C                                       should be 10 k
         FDVEC(3) = 10240 * (NBITWD / 8)
         FDVEC(5) = ITAPE
         FDVEC(6) = 16
C                                       Open tape
         CALL TAPIO ('OPRD', FDVEC, TBUFF, TBIND, IRET)
C
         IF (IRET.GT.1) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Initialise current file number
         MKFILE = 0
C                                       Skip to correct archive.
         IF (NFILES.GT.0) THEN
            CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IRET)
            END IF
         IFIL = 1 - NFILES
         IF (NFILES.LE.0) THEN
            CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         MKFILE = MKFILE + NFILES
C                                       Get first baseline.
         CALL ATREC ('INIT', IRET)
C
         IF (IRET.NE.0) GO TO 980
C                                       Init time pointer, correlation
C                                       type pointer.
         MKCTIM = 0
         MKTYPE = MKNTYP
C                                       Offset from reference day
         DAYOFF = MKSJD - REFJD
C
         IRET = 0
         GO TO 999
         END IF
C                                       Next visibility, or next
C                                       correlation type of curr vis.
 50   MKTYPE = MKTYPE + 1
      IF (MKTYPE .GT. MKNTYP) THEN
         MKCTIM = MKCTIM + 1
         MKTYPE = MKTYPE - MKNTYP - 1
         END IF
C                                       Time for another baseline?
      IF (MKCTIM.GT.MKNTIM) THEN
C                                       Update CATBLK on disk
C                                       Put vis. count in CATBLK
         CATBLK(KIGCN) = CNTVIS
         CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', TVIS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (6)
            END IF
C                                       Next datum
         ATJDRF = REFJD
         CALL ATREC ('READ', IRET)
C                                       Done?
         IF (IRET.NE.0) THEN
C                                       HF (FRNGE) table, error OK.
            CALL HFDUMP (IERR)
            IERR = 0
C                                       CL tables.
            CALL MK3CAL (IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C
         IF (IRET.NE.0) GO TO 980
C                                       Init time pointer, correlation
C                                       type pointer.
         MKCTIM = 1
         MKTYPE = 0
C                                       Offset from reference day
         DAYOFF = MKSJD - REFJD
C                                       Process source, antenna and
C                                       run parameters for this scan.
         CALL PROCSB (IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Is this time OK?
      IF (.NOT.MKTOK(MKCTIM)) GO TO 50
C                                       Time (including start time
C                                       offset) at AP midpoint (IAT).
      TUTC = MKSUTC + (MKCTIM-1) * MKINT(MKBASE) - MKTOFF -
     *   MKINT(MKBASE) / 2.0D0
      T = TUTC + IATOFF + DAYOFF
C                                       Antenna Numbers
      IA1 = MKBL(1,MKBASE)
      IA2 = MKBL(2,MKBASE)
C                                       Ref station for MKIII is stn B
      IAREF = 2
C                                       FQ ID
      IFQAID = MKBAFQ(MKBASE)
C                                       Autocorrelations?
      IF (MKTYPE.EQ.1) IA2 = IA1
      IF (MKTYPE.EQ.2) IA1 = IA2
C                                       Compute U,V and W
      CALL MK3UVW (TUTC, U, V, W)
C                                       Init. temporary array.
      IP = 1
      LIMFQ = MKAFRQ
      IF (MKSIDB.EQ.0) LIMFQ = LIMFQ * 2
C                                       Check for buffer overflow
      I = LIMFQ * NPOLN * MKAIF
      IF (I.GT.MXTVIS) THEN
         IRET = 9
         WRITE (MSGTXT,1080) I
         GO TO 990
         END IF
C                                       Set to zero
      DO 100 I = 1,MKAIF
         DO 90 J = 1,NPOLN
            DO 80 K = 1,LIMFQ
               TVIS(1,IP) = 0.0
               TVIS(2,IP) = 0.0
               TVIS(3,IP) = 0.0
               IP = IP + 1
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       Fetch vis data into temp array.
      GOOD = .FALSE.
      DSB = (MKSIDB.EQ.0) .AND. (.NOT.DOSIDB)
      JNCIF = INCIF / CATBLK(KINAX)
      JNCS = INCS / CATBLK(KINAX)
C                                       Loop over the correlators.
      IFQID = MKBFQ(MKBASE)
      DO 140 ICORR = 1,MKNCOR
C                                       Any data for this correlator ?
         IF (MKCTYP(ICORR) .LT. 0) GO TO 140
C                                       Freq, Stokes
         IFREQ = MKCFRQ(ICORR)
         ISTOKE = MKCSTK(ICORR)
C                                       Select only current corr. type.
         IF (MKCTYP(ICORR) .NE. MKTYPE) GO TO 140
C                                       AIPS Stokes pointer.
         STKOFF = (ABS (-ISTOKE - ICOR0)) * JNCS
C                                       AIPS IF pointer.
         IFNO = MKAFNO(IFQID,IFREQ)
C                                       Sideband.
         ISIDEB = 1
         IF (MKFQT(IFQID,IFREQ) .LT. 0) ISIDEB = -1
C                                       Start pointer for MKDATA.
         MMP = (MKCTIM-1) * MKNCOR + ICORR - 1
         MP = 1 + MMP * MKNLAG
         IF (ISIDEB .LT. 0) MP = MP + MKAFRQ
C                                       Start pointer for AIPS array.
         IP = 1 + (IFNO-1) * JNCIF + STKOFF
         IF (DSB .AND. (ISIDEB .GT. 0)) IP = IP + MKAFRQ
C                                       Weight
         WT = MKWT(1+MMP)
         GOOD = GOOD .OR. (WT.GT.0.0)
C                                       Frequency
         DO 120 J = 1,MKAFRQ
            IF (MKDATA(1,MP) .NE. FBLANK) TVIS(1,IP) = MKDATA(1,MP)
            IF (MKDATA(2,MP) .NE. FBLANK) TVIS(2,IP) = MKDATA(2,MP)
            TVIS(3,IP) = WT
            IP = IP + 1
            MP = MP + 1
 120        CONTINUE
 140     CONTINUE
C                                       Any valid data?
      IF (.NOT.GOOD) GO TO 50
C                                       Check AIPS baseline order
      IF (IA1.GT.IA2) THEN
C                                       Change ref station pointer
         IAREF = 1
         DO 150 I = 1,NIF
            IP = (I - 1) * JNCIF + 1
            CALL MK3FLP (NCHAN, NPOLN, ICOR0, JLOCS,
     *         TVIS(1,IP), TVIS(1,IP))
150         CONTINUE
         IP = IA1
         IA1 = IA2
         IA2 = IP
         U = -U
         V = -V
         W = -W
         END IF
C                                       Compressed data?
      NCOPY = LREC - NRPARM
      IF (DOUVC) THEN
         CALL ZUVPAK (NCOPY, TVIS, RPARM(1+KLOCWT), VIS)
      ELSE
         CALL RCOPY (NCOPY, TVIS, VIS)
         END IF
      IRET = 0
      GO TO 999
C                                       Tape Error
 980  IF (IRET.GT.0) CALL TAPIO ('CLOS', FDVEC, TBUFF, TBIND, IERR)
      GO TO 999
C                                       Error message
 990  CALL MSGWRT(8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I7,' OPENING ATAPE')
 1020 FORMAT ('ERROR',I7,' POSITIONING TAPE')
 1050 FORMAT ('ERROR ',I3,' UPDATING OUTPUT CATALOG HEADER')
 1080 FORMAT ('ERROR: BUFFER TOO SMALL:',I8,' CONTACT AIPS ADMIN.')
      END
      SUBROUTINE MK3FLP (NCHAN, NPOLN, IPOLRF, STORD, VIS, VIS2)
C-----------------------------------------------------------------------
C   Reverses a baseline.
C   Inputs:
C      NCHAN   I  Number of frequency channels.
C      NPOLN   I  Number of polarization channels (RR, LL, RL, LR)
C      IPOLRF  I  Ref Stokes value on polzn axis
C      STORD   I  Order of stokes axis, 1=> before freq
C   Input/Output
C      VIS     R(3,NPOLN,NCHAN)  Visibility data
C      VIS2    R(3,NCHAN,NPOLN)  Visibility data
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NPOLN, STORD, IPOLRF
      REAL      VIS(3,NPOLN,NCHAN), VIS2(3,NCHAN,NPOLN)
C
      INTEGER   IFREQ, IPOLRL, IPOLLR, IP, K
      REAL      TEMP
C-----------------------------------------------------------------------
C                                       RL,LR pointers
      IPOLRL = IPOLRF + 4
      IPOLLR = IPOLRL + 1
C                                       Check data order
C                                       Polarization before frequency
      IF (STORD.NE.1) GO TO 500
C
      DO 200 IFREQ = 1,NCHAN
         DO 80 IP = 1,NPOLN
C                                       Conjugate data.
            VIS(2,IP,IFREQ) = -VIS(2,IP,IFREQ)
 80         CONTINUE
C                                       Swap RL,LR if present
         IF ((IPOLRL .GE. 1) .AND. (IPOLLR .LE. NPOLN)) THEN
            DO 100 K = 1,3
               TEMP = VIS(K,IPOLRL,IFREQ)
               VIS(K,IPOLRL,IFREQ) = VIS(K,IPOLLR,IFREQ)
               VIS(K,IPOLLR,IFREQ) = TEMP
 100           CONTINUE
            END IF
 200     CONTINUE
      GO TO 999
C                                       Frequency before polarization

 500  CONTINUE
      DO 600 IFREQ = 1,NCHAN
         DO 520 IP = 1,NPOLN
C                                       Conjugate data.
            VIS2(2,IFREQ,IP) = -VIS2(2,IFREQ,IP)
 520        CONTINUE
C                                       Swap RL,LR if present
         IF ((IPOLRL .GE. 1) .AND. (IPOLLR .LE. NPOLN)) THEN
            DO 550 K = 1,3
               TEMP = VIS2(K,IFREQ,IPOLRL)
               VIS2(K,IFREQ,IPOLRL) = VIS2(K,IFREQ,IPOLLR)
               VIS2(K,IFREQ,IPOLLR) = TEMP
 550           CONTINUE
            END IF
 600     CONTINUE
C                                       Exit
 999  RETURN
      END
      SUBROUTINE MK3UVW (TUTC, U, V, W)
C-----------------------------------------------------------------------
C   Computes u, v, and w according to AIPS conventions at TUTC.
C   u,v and w are computed from the position of date and rotated such
C   that north is north of the standard epoch to remove the effects of
C   differential precession.
C   Inputs:
C      TUTC R   UTC time in days since reference date.
C   Output:
C      U    R   East-west component of projected baseline in wavelengths
C               at the reference frequency.
C      V    R   North-south component.
C      W    R   Up-down component.
C   Input from common
C      UVROT    R(2)       Cos, sin of rotation angle for uv axes
C                          to reference epoch (first order corr)
C      REFREQ   D          Reference frequency (Hz) in catalog hdr.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKRA     D(base#)   Right ascension of date (rad).
C      ANTLOC   D(stn#,3,  XYZ coordinates for stn#1=ref,2=rem
C               base#)     for baseline base# (left-handed).
C      MKSIDD   D(base#)   Derivative of GAST wrt UT (rad/s).
C      MKSIDT   D(base#)   GAST at a priori epoch (rad).
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C      MKVLIG   D(base#)   Speed of light (m/s).
C   Input/output via common
C      MKDEC    D(base#)   Declination of date (rad).
C-----------------------------------------------------------------------
      REAL      TUTC, U, V, W
C
      REAL      HA, SINHA, COSHA, COSD, SIND, LX, LY, LZ, VT, UT, GST,
     *   WAVEF
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKSTA.INC'
C-----------------------------------------------------------------------
C                                       Compute HA (rad)
      GST = MKSIDT(MKBASE) + MKSIDD(MKBASE) * 86400.0 *
     *   (TUTC - MKTIMC(2,MKBASE))
      HA = GST - MKRA(MKBASE)
      COSD = COS (MKDEC(MKBASE))
      SIND = SIN (MKDEC(MKBASE))
      SINHA = SIN (HA)
      COSHA = COS (HA)
C                                       Compute u, v, w
      LX = ANTLOC(1,1,MKBASE) - ANTLOC(2,1,MKBASE)
      LY = -(ANTLOC(1,2,MKBASE) - ANTLOC(2,2,MKBASE))
      LZ = ANTLOC(1,3,MKBASE) - ANTLOC(2,3,MKBASE)
      UT = LX * SINHA + LY * COSHA
      VT = SIND * (-LX * COSHA + LY * SINHA) + LZ * COSD
      W = COSD * ( LX * COSHA - LY * SINHA) + LZ * SIND
C                                       Rotate to Epoch orientation.
      U = UT * UVROT(1) + VT * UVROT(2)
      V = VT * UVROT(1) - UT * UVROT(2)
C                                       Scaling to wavelengths
      WAVEF = REFREQ / MKVLIG(MKBASE)
      U = U * WAVEF
      V = V * WAVEF
      W = W * WAVEF
C
 999  RETURN
      END
      SUBROUTINE ATREC (OPCODE, IRET)
C-----------------------------------------------------------------------
C   Reads a Haystack "A" tape and accumulates a baselines data for
C   the current scan.  Data is transformed from lags to frequency and
C   all necessary corrections are done.  Phase cal decoded signals are
C   also accumulated.
C      If the end of a scan is encountered the CL table for the previous
C   scan is written by a call to MK3CAL.
C      Note: the tape is assumed to have been opened and positioned
C   before the 'INIT' call.
C   Inputs:
C      OPCODE  C*4  Operation desired, 'INIT' = initialize I/O to tape,
C                   'READ' = read and accumulate a baseline-scan.
C   Outputs:
C      IRET    I      Error code, 0=OK, -1=> end of data, else error.
C   Input from common
C      FBLANK   R          REAL value indicating blanking.
C      IATOFF   R          IAT-UTC (seconds).
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCMOD   I(corr#)   No of lags per module for this correlator
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKFBS    R(4,time#) MKFBS(1,:) = average fractional bit delay
C                          error over accumulation period at time#.
C                          MKFBS(2,:) = number of delay steps in AP.
C                          MKFBS(3-4,:) = times from beginning/end
C                          of AP to next/previous delay step (bits).
C      MKTOK    L(time#)   Valid visibility data for this time# in
C                          array MKDATA ?
C      MKNTIM   I          Max time index time# used in MKDATA.
C      ATSEL    R(2)       Input time range wrt ref Jul day no.
C                          (1=start time; 2=stop time).
C      MKDFBS   L          True if full FBS correction is required.
C      MKDPHA   L          True if FBS phase correction is required.
C      MKMXFL   I          Max file no to read from tape.
C      MKPRNT   I          Print level (0..4).
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKLSCN   I          Nominal length of current scan (sec).
C      MKNTOF   I          No of type 51 data records used in
C                          computing the scan start time offset.
C      MKSJD    D          Scan start time (Jul day no).
C      MKSUTC   D          Scan start time (hhmmss in days).
C      LXTNT    I(256)     Pointer from EXTENT to BASLINE No.
C   Input/output via common
C      FTAB     I(*)       IO driving tables.
C      MKCTOT   I(corr#)   Total no of correlation functions for this
C                          correlator over the current scan baseline.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKGOOD   I(corr#)   No of good correlation functions for
C                          this correlator over the current scan
C                          baseline.
C      MKMNLG   I(corr#)   Min module delay offset for this corr.
C      MKMXLG   I(corr#)   Max module delay offset for this corr.
C      MKDATA   R(2,addr#) Visibility data from current scan-baseline
C                          addr# = (time# * Ncorr + corr# - 1) * Nlag
C                             + lag#
C                          time# = (data time - scan start) /int time
C                          corr# = correlator no; Ncorr = No. corr.
C                          lag# = lag no; Nlag = No of lags.
C      MKWT     R(2,addw#) Lag function weight.
C                          addw# = time# * Ncorr + corr#.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      FDVEC    I(50)      File descriptor vector for TAPIO.
C      TBUFF    I(2500)    Tape buffer.
C      MKFILE   I          Current tape file no wrt tape start posn.
C      TBIND    I          Pointer in TBUFF.
C      MKBASL   I          Baseline no of current ID record.
C      MKWANT   L          True if current scan wanted.
C      ATIME1   R          Start time of current scan (IAT
C                          days wrt ref Jul day no).
C      ATIME2   R          Nominal stop time of current scan
C                          (IAT days wrt ref Jul day no).
C      ATJDRF   D          Reference Jul day no for output file.
C      MKTOFF   D          Time offset of current scan baseline
C                          data from nominal scan start time (days).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   IRET
C                                   Length of file in TAR in blocks of
C                                   256 bytes (!). TAR is organized in
C                                   Blocks of 512 bytes, but all the
C                                   correllator data seems to be
C                                   organized in blocks of 256 bytes and
C                                   is read in this way.
      INTEGER   TLENGT,COUNTR,XTRA
      LOGICAL   MARKER
C                                    Unix filename on Tar
      CHARACTER LFILNM*100, LHDR*150
      INTEGER   TERR, IHDR, ITYPE, ID, IDX, ITIME, IBASE, IFREQ, NFREQ,
     *   IPOINT, JPOINT, TIND, PECNT, IEXT, ITPVOL, ISUBF, NID, IRET0,
     *   ILASTB, IFQID, ICORR, NBLNK, ISIDEB, J, K, ISTOKE, ITEMP,
     *   IOFFTM, NSPECT, JBASE
      LOGICAL   GOTDAT, NEWFIL, HEADER, DOEND, WSCANB, WFST51, WACFN
      REAL      FBSC(2,513), DLYERR, DLYCOR(4), TWORK(2,513)
      DOUBLE PRECISION TWOPI
C
      LOGICAL   WNTSRC
      EXTERNAL  WNTSRC
C
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCHR.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF, FBSC)
C-----------------------------------------------------------------------
C                                       Check for user signal
      CALL MK3TEL (IRET)
      IF (IRET.NE.0) GO TO 999
      IRET = 0
      TWOPI = 8.0D0 * ATAN (1.0D0)
      PECNT = 0
      GOTDAT = .FALSE.
      DOEND = .TRUE.
      NEWFIL = .TRUE.
      WSCANB = .FALSE.
      XTRA = 0
C                                       Initialize
      IF (OPCODE.EQ.'INIT') THEN
C                                       Full initialisation (first call)
         CALL ATINIT (0)
      ELSE
C                                       Clear baseline data only
         CALL ATINIT (2)
         END IF
C                                       Read/accumulate - next tape
C                                       record
C                                       Look if new header is already
C                                       read into the data buffer
100      IF (.NOT.MARKER) THEN
            CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, TERR)
         END IF
C                                       Count records
         COUNTR = COUNTR + 1
C
         IF (TERR.NE.0) THEN
C                                       End of tape or what ?
             IF ((TERR.EQ.4).OR.(TERR.EQ.5)
     *          .OR.(TERR.EQ.6)) THEN
                WRITE (MSGTXT,1400) TERR
                CALL MSGWRT (8)
                IRET = -1
                GO TO 400
             ELSE
                WRITE (MSGTXT,1100) TERR
                IRET = TERR
                GO TO 900
             END IF
          END IF
C                                       Read all records ? Actual rec.
C                                       is the header of a new file.
         IF (COUNTR.GT.TLENGT) THEN
C                                       Is there an empty block at and ?
            IF (XTRA.EQ.1) THEN
               CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, TERR)
            END IF
C
            MKFILE = MKFILE + 1
            COUNTR = 0
C                                       New header is already in buffer
C                                       in case we have data and start
C                                       over
            MARKER = .TRUE.
C
            IF (GOTDAT) GO TO 400
C                                       If no data so far keep looking.
C                                       Process last scan-baseline if
C                                       one was read from tape.
            IF (MKWANT .AND. WSCANB) CALL PROCFQ (IRET)
            IF (IRET .NE. 0) GO TO 999
            NEWFIL = .TRUE.
            GO TO 150
         END IF
C
         HEADER = .FALSE.
C                                       Decode the file header record
C                                       to determine the file type.
 150     IF (NEWFIL) THEN
C
            CALL ATHDR (TBUFF(TBIND), IHDR, ITYPE, IEXT, ITPVOL,
     *         ISUBF, LFILNM, LHDR, TLENGT, XTRA)
C                                       TAR header is 512 bytes long
C                                       so we have to do another read
C                                       of 256 bytes. Information in
C                                       this second part of header is
C                                       not needed in the momend
            CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, TERR)
C                                       Reset counter for new file
            MARKER = .FALSE.
            COUNTR = 0
C                                       If we have the EOF record
C                                       end it all.
            IF (IHDR .EQ. 2) THEN
               IRET = -1
C               GO TO 999
               GO TO 400
            END IF
C
            ILASTB = 0
            WFST51 = .TRUE.
            HEADER = .TRUE.
            END IF
C                                       Skip the file if it has
C                                       been deselected (IHDR=-1) or the
C                                       file type is not 50,51,52 or
C                                       this scan is not wanted.
C
         IF (((ITYPE.NE.51).AND.(ITYPE.NE.50).AND.(ITYPE.NE.52)) .OR.
     *      (IHDR.EQ.-1) .OR. ((.NOT.MKWANT).AND.(ITYPE.NE.50))) THEN
C
C                                       Print file header if required.
            IF ((MKPRNT .GE. 3) .AND. NEWFIL) THEN
C                                       Mark file as skipped.
               IF (LHDR(1:4).NE.'SKIP') LHDR(6:6) = 'X'
               MSGTXT = LHDR(:80)
               CALL MSGWRT (6)
               END IF
C                                       Skip to next file
            CALL TTSKIP (TLENGT,XTRA,TERR)
            IF (TERR.NE.0) THEN
C                                       End of tape?
               IF (TERR.EQ.6) THEN
                  IRET = -1
                  GO TO 999
                  END IF
C                                       Other - quit.
               WRITE (MSGTXT,1100) TERR
               IRET = TERR
               GO TO 900
               END IF
            NEWFIL = .TRUE.
            GO TO 100
            END IF
C                                       Print file header if required.
         IF (NEWFIL) THEN
            MSGTXT = LHDR(:80)
            IF (MKPRNT .GE. 3) CALL MSGWRT (6)
            NEWFIL = .FALSE.
            END IF
C                                       If end of scan do CL table stuff
         IF ((OPCODE.NE.'INIT') .AND. (ITYPE.EQ.50) .AND. DOEND) THEN
            DOEND = .FALSE.
C                                       HF (FRNGE) table, error OK.
            CALL HFDUMP (IRET)
            IRET = 0
C                                       CL tables.
            CALL MK3CAL (IRET)
            IF (IRET.NE.0) GO TO 999
C                                       If correct no of physical files
C                                       read then set EOT true.
            IF ((MKFILE.GT.MKMXFL).AND.(MKMXFL.GT.0)) THEN
               IRET = -1
               WRITE (MSGTXT,1000) MKFILE
               CALL MSGWRT (6)
               GO TO 999
               END IF
            END IF
C                                       New type 50 file
         IF ((ITYPE .EQ. 50) .AND. HEADER) THEN
            NID = 1
            MKWANT = .FALSE.
C
            END IF
C                                       Scan finished?
         IF (GOTDAT .AND. (ITYPE.EQ.50)) GO TO 400
C                                       Ignore header record for file
         IF (HEADER) GO TO 100
C                                       Branch by type
         GO TO (200, 300, 350), ITYPE-49
C                                       TYPE 50 records - Decode ID
 200        CALL ZI16IL (1,1, TBUFF(TBIND), IDX)
            ID = IDX / 100
C                                       Call appropriate routine:
C                                       Scan information:
            IF (ID.EQ.10) THEN
C                                       Reset scan info.
               IF (IDX.EQ.1000) CALL ATINIT (3)
               CALL AT10XX (TBUFF(TBIND), IRET)
C                                       No scan-baseline data yet.
               WSCANB = .FALSE.
C                                       Check timerange
               IF (ATJDRF.LT.1.0D0) ATJDRF = MKSJD
                  ATIME1 = (MKSJD - ATJDRF) + MKSUTC + IATOFF
                  ATIME2 = ATIME1 + MKLSCN / 86400.0
C                                       Do scan if any part selected
                  MKWANT = ((ATIME1.GE.ATSEL(1)) .AND.
     *               (ATIME1.LE.ATSEL(2))) .OR.
     *               ((ATIME2.GE.ATSEL(1)) .AND. (ATIME2.LE.ATSEL(2)))
               END IF
C                                       Baseline information:
            IF (MKWANT .AND. (ID .GT. 0)) THEN
               IF ((IDX.EQ.2000) .AND. WSCANB) THEN
                  CALL PROCFQ (IRET)
                  IF (IRET .NE. 0) GO TO 999
                  END IF
               IF (ID.EQ.20) CALL AT20XX (TBUFF(TBIND), IRET)
               IF (ID.EQ.21) THEN
                  CALL AT2100 (TBUFF(TBIND), IRET)
C                                       Check source name if not
C                                       blank (this appears to
C                                       happen in 2nd and subsequent
C                                       type 2100 records).
                  IF (MKSNAM(MKBASL).NE.' ')
     *               MKWANT = MKWANT.AND.WNTSRC(MKSNAM(MKBASL))
                  END IF
               IF (ID.EQ.22) CALL AT2200 (TBUFF(TBIND), IRET)
               IF (ID.EQ.23) CALL AT2300 (TBUFF(TBIND), IRET)
               IF (ID.EQ.24) CALL AT24XX (TBUFF(TBIND), IRET)
               IF (ID.EQ.25) CALL AT25XX (TBUFF(TBIND), IRET)
               IF (ID.EQ.26) CALL AT2600 (TBUFF(TBIND), IRET)
               IF (ID.EQ.27) CALL AT27XX (TBUFF(TBIND), IRET)
               IF (ID.EQ.28) CALL AT280X (TBUFF(TBIND), IRET)
               WSCANB = ID .GE. 20
C                                       Check ID record sequence.
               CALL IDCHK (NID, IDX, MKBASL, IRET0)
               IF (IRET0 .NE. 0) MKWANT = .FALSE.
               NID = NID + 1
               END IF
C
            IF (IRET.NE.0) GO TO 999
            GO TO 100
C                                       TYPE 51 = data records
C                                       Anything correlated?, wanted?
 300        IF (.NOT. MKWANT) GO TO 100
C                                       Extract baseline #
            CALL ZI8IL (1, 1, TBUFF(TBIND), ITEMP)
            IBASE = ITEMP - ((ITEMP / 64) * 64)
C                                       Check if IBASE matches expected
C                                       Extent. Basel. No. 0-2 may be
C                                       64-66
            JBASE = LXTNT(IEXT)
            IF ((IBASE .GE. 0) .AND. (IBASE .LE. 2)) THEN
               IF ((IBASE .NE. JBASE) .AND. (JBASE .GT. 0))IBASE = JBASE
            ELSE
C                                       Should not happen!
               IF ((IBASE .NE. JBASE) .AND. (JBASE .GT. 0)) THEN
                    WRITE (MSGTXT, 1300) IBASE, JBASE
                    CALL MSGWRT (6)
                    GO TO 100
                    ENDIF
               END IF
C                                       Skip if not selected.
            IF ((MKBL(1,IBASE) .LE. 0) .OR. (MKBL(2,IBASE) .LE. 0))
     *         GO TO 100
C                                       Generate a correlator index
C                                       for this extent.
            IF (WFST51) THEN
               CALL ATORDR (IBASE, IEXT, IRET)
               IF (IRET .NE. 0) GO TO 999
               WFST51 = .FALSE.
               END IF
C                                       Decode the type 51 record.
            CALL ATYP51 (TBUFF(TBIND), IEXT, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       One baseline per extent ?
            IF ((ILASTB .NE. 0) .AND. (MKBASE .NE. ILASTB)) THEN
               WRITE (MSGTXT,1110)
               CALL MSGWRT (6)
               MKWANT = .FALSE.
               GOTDAT = .FALSE.
               END IF
            ILASTB = MKBASE
C                                       Got data.
            GOTDAT = .TRUE.
            GO TO 100
C                                       Type 52 records (FRINGE)
350         CALL ATYP52 (TBUFF(TBIND), IRET)
            GO TO 100
C                                       End of file - process
C                                       Get data pointers.
 400  IF (.NOT. GOTDAT) GO TO 999
C                                       Flag correlators for which
C                                       no data was found.
      DO 420 ICORR = 1,MKNCOR
         IF (MKCTYP(ICORR) .EQ. 999) MKCTYP(ICORR) = -999
C                                       Warning for modules in AC mode
         IF ((MKCTYP(ICORR).EQ.1).OR.(MKCTYP(ICORR).EQ.2)) THEN
            WRITE (MSGTXT,1120)
            CALL MSGWRT (6)
            MKCTYP(ICORR) = -2
            END IF
420      CONTINUE
C                                       Calculate time offset for this
C                                       scan baseline.
      IOFFTM = 0
      IF (MKNTOF .NE. 0) IOFFTM = (ABS (MKTOFF) / MKNTOF) * 86400.0D0
     *   + 0.5
      IF (MKTOFF .LT. 0) THEN
         MKTOFF = -IOFFTM
      ELSE
         MKTOFF = IOFFTM
         END IF
      MKTOFF = MKTOFF / 86400.0D0
C                                       Compute and compare model delays
      CALL ATMODL (DLYERR)
      IFQID = MKBFQ(MKBASE)
      NFREQ = MKNFQT(IFQID)
      CURINT = MKINT(MKBASE) * 86400.0D0
      DLYCOR(1) = 0.0
C                                       Init bit shift array
      FBSC(1,1) = FBLANK
C                                       Loop over time
      DO 600 ITIME = 1,MKNTIM
         IF (MKTOK(ITIME)) THEN
C                                       Loop over the correlators.
            DO 550 ICORR = 1,MKNCOR
C                                       AC function ?
               WACFN = ((MKCTYP(ICORR) .EQ. 0) .AND.
     *            (MKBL(1,MKBASE) .EQ. MKBL(2,MKBASE))) .OR.
     *            (MKCTYP(ICORR) .EQ. 1) .OR. (MKCTYP(ICORR) .EQ. 2)
C                                       Any data for this correlator ?
               IF (MKCTYP(ICORR) .LT. 0) GO TO 550
C                                       Freq, Stokes
               IFREQ = MKCFRQ(ICORR)
               ISTOKE = MKCSTK(ICORR)
               IPOINT = ((ITIME-1) * MKNCOR + (ICORR-1)) * MKNLAG + 1
               JPOINT = (ITIME-1) * MKNCOR + ICORR
C                                       Do FBS correction?
               IF (MKDPHA) THEN
                  DO 440 J = 1,4
                     DLYCOR(J) = MKFBS(J,ITIME)
 440                 CONTINUE
                  END IF
C                                       Check for missing lags for
C                                       this correlator.
               MKCTOT(ICORR) = MKCTOT(ICORR) + 1
               NBLNK = 0
               NSPECT = MKCMAX(ICORR) - MKCMIN(ICORR) + 1
               DO 450 J = 1,NSPECT
                  K = IPOINT + J - 1
C                                       Don't check zero lag for AC
C                                       data (may be blanked by some
C                                       MKIII correlation lag ranges).
C                                       Set to 1.0 by ATXACF in
C                                       normalization later.
                  IF (WACFN.AND.((MKCMIN(ICORR)+J-1).EQ.0)) GO TO 450
                  IF ((MKDATA(1,K) .EQ. FBLANK) .OR.
     *               (MKDATA(2,K) .EQ. FBLANK)) NBLNK = NBLNK + 1
                  IF (NBLNK .GT. 2) GO TO 470
450               CONTINUE
C                                       Transform to freq. and make
C                                       the FBS delay correction.
470            IF (NBLNK .EQ. 0) THEN
                  ISIDEB = 1
                  IF (MKFQT(IFQID,IFREQ) .LT. 0) ISIDEB = -1
                  IF (WACFN) THEN
                     CALL ATXACF (MKNLAG, ISIDEB, MKCMIN(ICORR),
     *                  MKCMAX(ICORR), MKDATA(1,IPOINT), TWORK)
                  ELSE
                     CALL ATXFRM (MKNLAG, ISIDEB, DLYCOR, FBSC,
     *                  MKDFBS, MKCMIN(ICORR), MKCMAX(ICORR),
     *                  MKDATA(1,IPOINT), TWORK)
                     END IF
                  MKGOOD(ICORR) = MKGOOD(ICORR) + 1
C                                       Flag spectrum as bad.
               ELSE
                  MKWT(JPOINT) = -ABS (MKWT(JPOINT))
                  END IF
 550           CONTINUE
            END IF
 600     CONTINUE
C                                       Process Phase cal info
      CALL ATPCAL
      GO TO 999
C                                       Tape error try to recover from
C                                       parity errors
 900  IF (TERR.EQ.3) THEN
C                                       Don't do more that 5 PE / call
         PECNT = PECNT + 1
         IF (PECNT.GT.5) THEN
            MSGTXT = 'TOO MANY PARITY ERRORS'
            GO TO 990
         END IF
C                                       Muck with FTAB to cause TAPIO to
C                                       ignore the rest of the tape
C                                       block.
         TIND = FDVEC(40)
         FTAB(TIND+12) = FTAB(TIND+11) + 1
C                                       If reading data just go on.
         IF ((ITYPE.EQ.51) .AND. GOTDAT) GO TO 100
C                                       Force finding another TYPE 50
C                                       file.
         MKWANT = .FALSE.
      END IF
C                                       Next file
      GO TO 100
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STOPPED AT FILE ',I6)
 1100 FORMAT ('ATREC: ERROR ',I3,' READING TAPE')
 1110 FORMAT ('ATREC: More than one baseline per extent - skip')
 1120 FORMAT ('M3TAR requires development for modules in AC mode')
 1300 FORMAT ('BASELINE INDEX PROBLEM (', I3, '.NE.', I3,
     *        '): SKIPPING DATA')
 1400 FORMAT ('ATREC: ERROR ',I3,' FOUND END OF TAPE')
      END
      SUBROUTINE MK3TEL (IRET)
C-----------------------------------------------------------------------
C   Look for user signals (TELL), at present only honor 'QUIT' and
C   'ABOR' .
C   Output
C      IRET   I  Return code, 0=> continue, <0=quit gracefully, >0=die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER OPTELL*4
      INTEGER   NPARM, SBUFF(256), IERR
      REAL      DUMMY
C-----------------------------------------------------------------------
C                                       Check TELL file
      IRET = 0
      NPARM = 1
      CALL GTTELL (NPARM, OPTELL, DUMMY, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'Received ' // OPTELL // ' operation from TELL'
         CALL MSGWRT (4)
         END IF
      IF (IERR.EQ.2) IRET = -1
      IF (IERR.EQ.3) IRET = 9
C
 999  RETURN
      END
      SUBROUTINE AT10XX (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 10XX records from A tape and gets scan
C   information.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C                        NEW: Floating values now in IEEE 64-bit,
C                             not longer in old HP 64-bit format.
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      MKSNAM   C*8(base#) Source name.
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C   Input/output via common
C      NBITWD   I          # bits /word
C      TMPBUF   I(1024)    Work buffer.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKNBL    I          No of baselines in antenna table MKBL.
C      MKLSCN   I          Nominal length of current scan (sec).
C      MKSJD    D          Scan start time (Jul day no).
C      MKSUTC   D          Scan start time (hhmmss in days).
C      MKTMS    D          Scan start time (mmss in days).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, NBASE, ISYR, ISDOY, ISMON, ISDOM, ISHR,
     *   ISMIN, ISSEC, LSEC, BINFO(12,8), IP, IPS, I, IB, J0, J
      INTEGER IBASE, I20XX
      CHARACTER CHTEMP*8, PNAM*8, RNAM*8, SNAM*8, FNAM*6, FCOD*2,
     *   A1*8, A2*8, BID*2
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(4),NBASE),
     *   (TMPBUF(5),ISYR), (TMPBUF(6),ISDOY), (TMPBUF(7),ISMON),
     *   (TMPBUF(8),ISDOM), (TMPBUF(9),ISHR), (TMPBUF(10),ISMIN),
     *   (TMPBUF(11),ISSEC), (TMPBUF(12),LSEC), (TMPBUF(33),BINFO)
C-----------------------------------------------------------------------
C                                       Convert whole record to integer
      NCONV = 128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.10) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Scan start date and time
      IP = ISYR / 100
      ISYR = ISYR - IP * 100
      WRITE (CHTEMP,1002) ISDOM, ISMON, ISYR
      CALL JULDAY (CHTEMP, MKSJD)
      MKTMS = (ISMIN / 1440.0D0) + (ISSEC / 86400.0D0)
      MKSUTC = (ISHR / 24.0D0) + MKTMS
C                                       Length of scan
      MKLSCN = LSEC
C                                       Number of baselines in the scan
      MKNBL = NBASE
C                                       Find antenna numbers for this
C                                       set.
      IB = MOD (ID, 1000) * 8 + 1
      DO 100 I = 1,8
C                                       Good baseline?
         IF ((IB.LE.0) .OR. (IB.GT.MKNBL)) GO TO 100
         J0 = 32 + (I - 1) * 12
C                                       Ref. antenna
         J = J0 + 1
         CALL INDXEQ (J, 16, NBITWD, IP, IPS, 8)
         NCONV = 8
         CALL ZC8CL (NCONV, IPS, RECORD(IP), CHTEMP)
C                                       Convert name to number
         CALL ATANTS (CHTEMP, MKBL(1,IB))
C                                       Remote antenna
         J = J0 + 5
         CALL INDXEQ (J, 16, NBITWD, IP, IPS, 8)
         CALL ZC8CL (NCONV, IPS, RECORD(IP), CHTEMP)
C                                       Convert name to number
         CALL ATANTS (CHTEMP, MKBL(2,IB))
C                                       Has the baseline been deleted
C                                       from the root ?
         J = J0 + 12
         CALL INDXEQ (J, 16, NBITWD, IP, IPS, 16)
         CALL ZI16IL (1, IPS, RECORD(IP), I20XX)
         IF (I20XX .LT. 0) THEN
            MKBL(1,IB) = -999
            MKBL(2,IB) = -999
            END IF
         IB = IB + 1
 100     CONTINUE
C                                       Dump record?
         IF (MKDBUG.GE.1) THEN
            WRITE (9,2000) (TMPBUF(I),I=1,12)
C                                       Program name
            CALL INDXEQ (13, 16, NBITWD, IP, IPS, 8)
            NCONV = 8
            CALL ZC8CL (NCONV, IPS, RECORD(IP), PNAM)
C                                       Run name
            CALL INDXEQ (17, 16, NBITWD, IP, IPS, 8)
            CALL ZC8CL (NCONV, IPS, RECORD(IP), RNAM)
C                                       Source name
            SNAM = MKSNAM(IBASE)
C                                       File name
            CALL INDXEQ (25, 16, NBITWD, IP, IPS, 8)
            CALL ZC8CL (6, IPS, RECORD(IP), FNAM)
C                                       Frequency code
            CALL INDXEQ (28, 16, NBITWD, IP, IPS, 8)
            CALL ZC8CL (2, IPS, RECORD(IP), FCOD)
            WRITE (9,2001) PNAM, RNAM, SNAM, FNAM, FCOD
            WRITE (9,2002) (TMPBUF(I),I=29,32)
            IB = MOD (ID, 1000) * 8 + 1
            DO 800 I = 1,8
C                                       Good baseline?
               IF ((IB.LE.0) .OR. (IB.GT.MKNBL)) GO TO 800
               J0 = 32 + (I - 1) * 12
               IB = IB + 1
C                                       Ref. antenna
               J = J0 + 1
               CALL INDXEQ (J, 16, NBITWD, IP, IPS, 8)
               NCONV = 8
               CALL ZC8CL (NCONV, IPS, RECORD(IP), A1)
C                                       Remote antenna
               J = J0 + 5
               CALL INDXEQ (J, 16, NBITWD, IP, IPS, 8)
               CALL ZC8CL (NCONV, IPS, RECORD(IP), A2)
C                                       Baseline code
               J = J0 + 9
               CALL INDXEQ (J, 16, NBITWD, IP, IPS, 8)
               CALL ZC8CL (2, IPS, RECORD(IP), BID)
               WRITE (9,2003) A1, A2, BID, BINFO(10,I), BINFO(11,I),
     *            BINFO(12,I)
 800           CONTINUE
            END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT10XX: WRONG RECORD ID =',I5)
 1002 FORMAT (I2,'/',I2,'/',I2)
 2000 FORMAT (/' ID=',I5,' COREL ver',I5,' last extent',I5,
     *   ' NBASE=',I5/, ' ISYR=',I5, ' ISDOY=',I5,
     *   ' ISMON=',I5, ' ISDOM=',I5,/,' ISHR=',I5,
     *   ' ISMIN=',I5, ' ISSEC=',I5, ' LSEC=',I5)
 2001  FORMAT (' Program name=',A, ' Run name=',A,' Source name=',A,/,
     *   ' File name=',A, ' Freq seq. code=',A)
 2002  FORMAT (' Root creat. date=',I6, ' Root creat. time=',I5,
     *   ' Exp. serial no.=',I6,/, ' Checksum=',I8)
 2003  FORMAT (1X,A,2X,A,2X,A,3I8)
      END
      SUBROUTINE ATINIT (ICODE)
C-----------------------------------------------------------------------
C   Initializes "A" tape commons
C   Inputs:
C      ICODE   I  1 => full, 2 = baseline data only, 3=> header data
C                 only, 0 => first full initialisation.
C   Input from common
C      DBLANK   D          Magic value (= indeterminate).
C      FBLANK   R          REAL value indicating blanking.
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      NANT     I          No of antennas in antenna list.
C   Input/output via common
C      MKSNAM   C*8(base#) Source name.
C      MKGEO    D(parm#,   Antenna based geometric model by ant#
C               ant#,cl#)  and CL table index. Parm# 1,2,3 =
C                          (delay (us), rate (us/s), acc (us/s/s))
C                          respectively.
C      MKNTMC   I          Number of CL table entry times per
C                          scan/antenna/IF.
C      MKCTOT   I(corr#)   Total no of correlation functions for this
C                          correlator over the current scan baseline.
C      MKER51   I(mod#,    No of type 51 records rejected with error
C               error#)    number error# for module mod#.
C      MKGOOD   I(corr#)   No of good correlation functions for
C                          this correlator over the current scan
C                          baseline.
C      MKNR51   I(mod#)    Total no of type 51 data records read
C                          for module mod#.
C      MKSERL   I(mod#)    Serial no of correlator module mod#.
C      MKNSER   I          No of entries in MKSERL.
C      MKDATA   R(2,addr#) Visibility data from current scan-baseline
C                          addr# = (time# * Ncorr + corr# - 1) * Nlag
C                             + lag#
C                          time# = (data time - scan start) /int time
C                          corr# = correlator no; Ncorr = No. corr.
C                          lag# = lag no; Nlag = No of lags.
C      MKFBS    R(4,time#) MKFBS(1,:) = average fractional bit delay
C                          error over accumulation period at time#.
C                          MKFBS(2,:) = number of delay steps in AP.
C                          MKFBS(3-4,:) = times from beginning/end
C                          of AP to next/previous delay step (bits).
C      MKTOK    L(time#)   Valid visibility data for this time# in
C                          array MKDATA ?
C      MKWT     R(2,addw#) Lag function weight.
C                          addw# = time# * Ncorr + corr#.
C      MKNTIM   I          Max time index time# used in MKDATA.
C      MAXPOL   I          Max polzn code for current scan baseline.
C      MINPOL   I          Min polzn code for current scan
C                          baseline (1=RR,2=LL,3=RL,4=LR).
C      MKFRQ    D(freq#)   RF frequency at baseband in video for each
C                          of up to 28 frequencies for the current
C                          scan-baseline data (Hz) (>0 USB; < 0 LSB).
C      MKLOFF   D(2,freq#) LO offset for reference and remote stn.
C                          (1=ref,2=rem) in same order as MKFRQ (Hz).
C                          Sum of MKFRQ and MKLOFF gives RF freq.
C                          translated to DC at ref. station.(<0 LSB;
C                          >0 USB) LO offsets are not applied
C      MKNFRQ   I(base#)   No of freq in MKFRQ per baseline.
C      MKPCAL   R(parm#,   Phase calibration values from current
C               stn#,corr#) scan-baseline. parm#: 1=sum cos;
C                          2=sum cos**2; 3=sum sin; 4=sum sin**2;
C                          5=n. stn# = 1(ref),2(rem).
C      MKPCSC   R(parm#,    Scan-averaged phase cal values. parm#:
C               ant#,freq#) 1=Re(RCP);2=Im(RCP);3=Re(LCP);4=Im(LCP).
C      MKPFRQ   D(freq#)   Phase calibration tone frequencies for the
C                          current scan baseline data (Hz). In same
C                          order as MKFRQ.
C      MKNFID   I          No of entries in MKNFQT.
C      MKNPID   I          No of entries in MKNPFQ.
C      MKBAFQ   I(base#)   AIPS IF id for baseline base#.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBLNX   I(2,base#) 1=Start, 2=stop pointers for baseline
C                          base# in cross-ref index table MKXREF.
C      MKBPFQ   I(base#)   Phase cal id #pcid for baseline base#.
C      MKXREF   I(parm#,   Cross reference table for correlator
C               indx#)     module type 51 data records.
C                          indx# = corr module index wrt start
C                          index for curr baseline (MKBLNX(1,base#)
C                          parm#: 1= Type 51 module index (unused);
C                          2= Ref stn track no (unused); 3= Remote
C                          stn track no (unused); 4= Freq index in
C                          MKFRQ.; 5= Partner track on ref. stn tape
C                          (track on opposite side of same VC)
C                          (unused); 6= Module delay offset (bits);
C                          7= Pulsar gate-on (milli-periods)(unused);
C                          8= Pulsar gate-off (milli-periods) (unus-
C                          ed).; 9= Module termination code (from
C                          COREL at end of scan (2-11 ok); 10= Extent
C                          no; 11= Polarization code (1=RR,2=LL,
C                          3=RL,4=LR); 12= Correlator module serial
C                          no (<0:MK3A, >0: MK3).
C      LXTNT    I(256)     Pointer from EXTENT to BASLINE No.
C      MKINDU   I          Next available slot in MKXREF.
C      MKNBL    I          No of baselines in antenna table MKBL.
C      MKLSCN   I          Nominal length of current scan (sec).
C      MKNTOF   I          No of type 51 data records used in
C                          computing the scan start time offset.
C      MKSJD    D          Scan start time (Jul day no).
C      MKSUTC   D          Scan start time (hhmmss in days).
C      MKTOFF   D          Time offset of current scan baseline
C                          data from nominal scan start time (days).
C      MKKSEL   I(base#)   Corr phase increment update rate selector
C                          (unused).
C      MKMODE   I(base#)   Correlator mode (see COREL file for full
C                          listing: use only Bit 0 = hardware FBS
C                          correction active; bit 3 = spectral line
C                          mode.
C      MKNFP    I(base#)   No frames/parameter period used by corr.
C      MKNTRK   I(base#)   Number of tracks taken in this obs(unused)
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C      MKFFQ    D(*)       List of RF frequencies found
C      MKNFFQ   I          Number of entries in MKFFQ
C      LSBOFF   D          Correction for LSB IF frequencies.
C-----------------------------------------------------------------------
      INTEGER   ICODE, I, J, K
      INTEGER   LIMIT1, LIMIT2, LIMITA, LIMITF, LIMITB, LIMITT
      INTEGER   LIMITC, LIMITX, LIMITM
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKCLT.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
C
      INCLUDE'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Set loop limits.
      IF (ICODE .EQ. 0) THEN
C                                       Max. limits for first
C                                       initialisation call.
         LIMITA = 30
         LIMITF = MXM3FQ
         LIMITB = MXBAS
         LIMITC = MXCOR
         LIMITM = MXCOR
         LIMITT = MXTIME
         LIMITX = MXREF
         LIMIT1 = MXHUGE
         LIMIT2 = MXHUGE / 8
      ELSE
C                                       Use loop limits from last scan.
         LIMITA = NANT
         LIMITF = 0
         DO 5 I = 1,MKNBL
            LIMITF = MAX (LIMITF, MKNFRQ(I))
5           CONTINUE
         LIMITB = MKNBL
         LIMITT = MKNTIM
         LIMITC = MKNCOR
         LIMITM = MKNSER
         LIMITX = MKINDU
         LIMIT1 = MKNTIM * MKNLAG * MKNCOR
         LIMIT2 = MKNTIM * MKNCOR
         END IF
C                                       Scan info.
      IF ((ICODE.EQ.1) .OR. (ICODE.EQ.3) .OR. (ICODE .EQ. 0)) THEN
C                                       Init FRNGE table
         CALL ATINI2
         MINPOL = 999
         MAXPOL = -999
         MKSJD = 0.0D0
         MKSUTC = 0.0D0
         MKLSCN = 0
         MKNBL = 0
         MKNTMC = 0
         MKNFFQ = 0
         LSBOFF = 0.0D0
         DO 10 I = 1,LIMITB
            MKBL(1,I) = 0
            MKBL(2,I) = 0
            MKSNAM(I) = ' '
            MKBFQ(I) = 0
            MKBAFQ(I) = 0
            MKBPFQ(I) = 0
 10         CONTINUE
         DO 30 I = 1,LIMITF
C                                       Zero frequencies
            MKFRQ(I) = 0.0D0
            MKPFRQ(I) = 0.0D0
            MKLOFF(1,I) = 0.0D0
            MKLOFF(2,I) = 0.0D0
C                                       Blank phase cal values
            DO 20 J = 1,LIMITA
               DO 15 K = 1,4
                  MKPCSC(K,J,I) = FBLANK
 15               CONTINUE
 20            CONTINUE
 30         CONTINUE
C                                       Antenna Geometry
         DO 50 J = 1,LIMITA
            MKGEO(1,J,1) = DBLANK
 50         CONTINUE
C                                       Baseline index stuff
C                                       Lag range
         DO 60 I = 1,LIMITB
            MKNFP(I) = 0
            MKKSEL(I) = 0
            MKMODE(I) = 0
            MKNTRK(I) = 0
            MKBLNX(1,I) = 0
            MKBLNX(2,I) = 0
            MKTIMC(1,I) = 0.0D0
            MKTIMC(2,I) = 0.0D0
 60         CONTINUE
C                                       Reset cross-ref table.
         DO 80 I = 1,LIMITX
            DO 70 J = 1,12
               MKXREF(J,I) = 0
 70            CONTINUE
 80         CONTINUE
            MKINDU = 1
C                                       Reset Extent table
         DO 90 I = 1,256
 90         LXTNT(I) = -1
C                                       Reset RF freq, phase-cal tables.
         MKNFID = 0
         MKNPID = 0
         END IF
C                                       Baseline info.
      IF ((ICODE.EQ.1) .OR. (ICODE.EQ.2) .OR. (ICODE .EQ. 0)) THEN
C                                       Time pointer
         MKNTIM = 0
C                                       Time offset.
         MKTOFF = 0.0D0
         MKNTOF = 0
C                                       Lag data array
         DO 100 I = 1,LIMIT1
            MKDATA(1,I) = FBLANK
            MKDATA(2,I) = FBLANK
 100        CONTINUE
C                                       No. bits correlated.
         DO 110 I = 1,LIMIT2
            MKWT(I) = 0.0
 110        CONTINUE
C                                       Phase cal arrays
         DO 130 I = 1,LIMITC
            DO 120 J = 1,5
               MKPCAL(J,1,I) = 0.0
               MKPCAL(J,2,I) = 0.0
 120           CONTINUE
 130        CONTINUE
C                                       Time flags, FBS corr.
         DO 140 I = 1,LIMITT
            MKTOK(I) = .FALSE.
            DO 135 J = 1,4
               MKFBS(J,I) = 0.0
 135           CONTINUE
 140        CONTINUE
C                                       Reset the data quality stats
         DO 170 I = 1,LIMITC
            MKGOOD(I) = 0
            MKCTOT(I) = 0
170         CONTINUE
C                                       Reset type 51 error table.
         DO 180 I = 1,LIMITM
            DO 175 J = 1,20
               MKER51(I,J) = 0
175            CONTINUE
            MKNR51(I) = 0
            MKSERL(I) = 0
180         CONTINUE
         MKNSER = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE ATINI2
C-----------------------------------------------------------------------
C   Initializes FRNGE output commons (M3GOT)
C   Inputs:
C   Input/output via common
C      M3GOT  L(*)  If element i is true then valid values are loaded
C      into the corresponding values of the FRNGE common.
C-----------------------------------------------------------------------
      INCLUDE 'MKFRNG.INC'
C
      INTEGER   LOOP
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,MAXM3X
         M3GOT(LOOP) = .FALSE.
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ATANTS (ANAME, ANTNO)
C-----------------------------------------------------------------------
C   Looks up an antenna name and determines its number.
C   Input:
C      ANAME    C*8  Antenna name
C   Output:
C      ANTNO    I    Antenna number -999 => not found
C   Input from common
C      NANT     I          No of antennas in antenna list.
C   Input/output via common
C      ANTNAM   C*8(ant#)  Antenna name.
C-----------------------------------------------------------------------
      CHARACTER ANAME*8
      INTEGER   ANTNO
C
      INTEGER   I
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'ALIAS.INC'
C-----------------------------------------------------------------------
      ANTNO = -999
C                                       Perform alias substitution
      DO 10 I = 1, NALIAS
         IF (ANAME.EQ.ALIAS(1, I)) THEN
            ANAME = ALIAS(2, I)
            GO TO 20
            END IF
   10    CONTINUE
   20 CONTINUE
C
      DO 100 I = 1,NANT
C                                       If ANAME='ANY' then use ANAME
         IF (ANTNAM(I).EQ.'ANY') THEN
            ANTNAM(I) = ANAME
            ANTNO = I
            GO TO 999
            END IF
         IF (ANAME.EQ.ANTNAM(I)) THEN
            ANTNO = I
            GO TO 999
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE AT20XX (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 20xx records from A tape.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKFCOD   C*1(4)     Conversion table for polzn codes in
C                          order (RR,LL,RL,LR).
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKDOPL   L          True if multiple polarizations to be read.
C   Input/output via common
C      MAXPOL   I          Max polzn code for current scan baseline.
C      MINPOL   I          Min polzn code for current scan
C                          baseline (1=RR,2=LL,3=RL,4=LR).
C      TMPBUF   I(1024)    Work buffer.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBLNX   I(2,base#) 1=Start, 2=stop pointers for baseline
C                          base# in cross-ref index table MKXREF.
C      MKXREF   I(parm#,   Cross reference table for correlator
C               indx#)     module type 51 data records.
C                          indx# = corr module index wrt start
C                          index for curr baseline (MKBLNX(1,base#)
C                          parm#: 1= Type 51 module index (unused);
C                          2= Ref stn track no (unused); 3= Remote
C                          stn track no (unused); 4= Freq index in
C                          MKFRQ.; 5= Partner track on ref. stn tape
C                          (track on opposite side of same VC)
C                          (unused); 6= Module delay offset (bits);
C                          7= Pulsar gate-on (milli-periods)(unused);
C                          8= Pulsar gate-off (milli-periods) (unus-
C                          ed).; 9= Module termination code (from
C                          COREL at end of scan (2-11 ok); 10= Extent
C                          no; 11= Polarization code (1=RR,2=LL,
C                          3=RL,4=LR); 12= Correlator module serial
C                          no (<0:MK3A, >0: MK3).
C      LXTNT    I(256)     Pointer from EXTENT to BASLINE No.
C   Module termination code:
C                           1 = no data   2 = 0-10% done 3 = 10-20%
C                          4 = 20-30%    5 = 30-40%     6 = 40-50%
C                          7 = 50-60%    8 = 60-60%     9 = 70-80%
C                          10 = 80-90%    11 = 90-100%
C                          12 = missing tape
C                          13 = no data on tape or unreadable
C                          14 = Wrong data on tape
C                          15 = Physical tape or reeel damage
C                          16 = Tape drive problem
C                          17 = LT file error
C                          18 = wrong tape mounted
C                          19 = I goofed?
C                          20 = #SK file error!
C                          21 = Fringe search
C                          22 = Tape sync problems
C                          23 = Test correlation
C                          24 = Other
C      MKBASL   I          Baseline no of current ID record.
C      MKINDU   I          Next available slot in MKXREF.
C      MKWANT   L          True if current scan wanted.
C      MKKSEL   I(base#)   Corr phase increment update rate selector
C                          (unused).
C      MKMODE   I(base#)   Correlator mode (see COREL file for full
C                          listing: use only Bit 0 = hardware FBS
C                          correction active; bit 3 = spectral line
C                          mode.
C      MKNFP    I(base#)   No frames/parameter period used by corr.
C      MKNTRK   I(base#)   Number of tracks taken in this obs(unused)
C      MKTIM0   D(base#)   Time since BOY (s) of a priori calc
C                          (Should be the same time as PRT).
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, IEYR, IEDOY, IEMON, IEDOM, IEHR,
     *   IEMIN, IESEC, NFP, KSEL, KMODE, NTRKS, LSTIDX, IXREF(18,6), I,
     *   INDX, IP, JP, NWD, JNDX, KNDX, K
      DOUBLE PRECISION JD
      CHARACTER CHTMP*1, CCODS*24, TMPDAT*8, BID*2, RM*2, FC*2, TC*1
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(5),IEYR),
     *   (TMPBUF(6),IEDOY), (TMPBUF(7),IEMON), (TMPBUF(8),IEDOM),
     *   (TMPBUF(9),IEHR), (TMPBUF(10),IEMIN), (TMPBUF(11),IESEC),
     *   (TMPBUF(13),NFP), (TMPBUF(15),KSEL), (TMPBUF(16),KMODE),
     *   (TMPBUF(17),NTRKS), (TMPBUF(18),LSTIDX), (TMPBUF(21),IXREF)
      DATA CCODS /'Z0123456789ABDEFGHJKRSTO'/
C-----------------------------------------------------------------------
      NWD = NBITWD / 16
C                                       Convert whole record to integer
      NCONV = 128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.20) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Save Baseline Values
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
C                                       Time and date
      IP = IEYR / 100
      IEYR = IEYR - IP * 100
      WRITE (TMPDAT,1002) IEDOM, IEMON, IEYR
      CALL JULDAY (TMPDAT, JD)
      MKTIMC(1,IBASE) = JD
      MKTIMC(2,IBASE) = (IEHR / 24.0D0) + (IEMIN / 1440.0D0)  +
     *   (IESEC / 86400.0D0)
      MKTIM0(IBASE) = (IEDOY-1) * 86400.0D0 + IEHR * 3600.0D0 +
     *   IEMIN * 60.0D0 + IESEC
C                                       Other info
      MKNFP(IBASE) = NFP
      MKKSEL(IBASE) = KSEL
      MKMODE(IBASE) = KMODE
      MKNTRK(IBASE) = NTRKS
      IF (MKBLNX(1,IBASE).LE.0) THEN
         INDX = MKINDU
         MKBLNX(1,IBASE) = INDX
      ELSE
         INDX = MKBLNX(1,IBASE)
         END IF
      KNDX = INDX
      JNDX = 1
C                                       De-select baseline on first
C                                       entry. Reselect if any
C                                       active modules found.
      IF (ID .EQ. 2000) MKBL(1,IBASE) = -ABS (MKBL(1,IBASE))
C                                       Get index array
      DO 200 I = 1,6
         IF (IXREF(1,I) .GT. 0) THEN
C                                       Re-select baseline
            IF (MKBL(1,IBASE) .NE. -999)
     *         MKBL(1,IBASE) = ABS (MKBL(1,IBASE))
            JNDX = INDX + IXREF(1,I) - 1
C                                       "Freq. code" = Stokes.
            IP = 20 + (I-1) * 18 + 7
            JP = (2 * MOD (IP, NWD)) + 1
            IP = (IP / NWD) + 1
            NCONV = 1
            CALL ZC8CL (NCONV, JP, RECORD(IP), CHTMP)
            IP = ABS (IXREF(6,I))
            MKXREF(11,JNDX) = 0
            IF (MKDOPL) THEN
C                                       Multiple polarizations
               IF (IP.GT.0) THEN
                  IF (CHTMP.EQ.MKFCOD(1)) MKXREF(11,JNDX) = 1
                  IF (CHTMP.EQ.MKFCOD(2)) MKXREF(11,JNDX) = 2
                  IF (CHTMP.EQ.MKFCOD(3)) MKXREF(11,JNDX) = 3
                  IF (CHTMP.EQ.MKFCOD(4)) MKXREF(11,JNDX) = 4
C                                       Unidentified polzn. code.
                  IF (MKXREF(11,JNDX) .EQ. 0) THEN
                     WRITE (MSGTXT,1010) CHTMP
                     GO TO 990
                     END IF
C                                       Update polzn. range.
                  MINPOL = MIN (MINPOL, MKXREF(11,JNDX))
                  MAXPOL = MAX (MAXPOL, MKXREF(11,JNDX))
                  END IF
            ELSE
C                                       Single polarization
               MKXREF(11,JNDX) = ABS (MKPOL1)
               MINPOL = ABS (MKPOL1)
               MAXPOL = ABS (MKPOL2)
               END IF
C                                       Extent number.
            MKXREF(10,JNDX) = IXREF(2,I)
C                                       Save the baseline No in list
C                                       of extents to overcome mod 64
            LXTNT(IXREF(2,I)) = IBASE
C                                       Index number
            MKXREF(1,JNDX) = IXREF(1,I)
C                                       Reference station track number
            MKXREF(2,JNDX) = IXREF(4,I)
C                                       Remote station track number
            MKXREF(3,JNDX) = IXREF(5,I)
C                                       Frequency index no.
            MKXREF(4,JNDX) = IXREF(6,I)
C                                       Partner index
            MKXREF(5,JNDX) = IXREF(7,I)
C                                       Peculiar delay
            MKXREF(6,JNDX) = IXREF(11,I)
C                                       Pulsar gate-on
            MKXREF(7,JNDX) = IXREF(16,I)
C                                       Pulsar gate-off
            MKXREF(8,JNDX) = IXREF(17,I)
C                                       Termination Code
            IP = 20 + (I-1) * 18 + 17
            JP = (2 * MOD (IP, NWD)) + 1
            IP = (IP / NWD) + 1
            NCONV = 1
            CALL ZC8CL (NCONV, JP, RECORD(IP), CHTMP)
            MKXREF(9,JNDX) = INDEX (CCODS, CHTMP)
C                                       Correlator module serial no.
            MKXREF(12,JNDX) = IXREF(15,I)
            END IF
         KNDX = MAX (KNDX, JNDX)
         IF (IXREF(1,I).EQ.0) GO TO 210
 200     CONTINUE
 210  MKBLNX(2,IBASE) = MAX (MKBLNX(2,IBASE), KNDX)
C                                       Check limit
      IF (KNDX.GE.MXREF) THEN
         MSGTXT = 'WARNING:OVERFLOWING CROSS REFERENCE ARRAYS'
         CALL MSGWRT (7)
C                                       Bail out of this scan
         MKWANT = .FALSE.
         CALL ATINIT (3)
         GO TO 999
         END IF
      KNDX = KNDX + 1
      MKINDU = MAX (MKINDU, KNDX)
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         IF (IXREF(1,1).EQ.0) GO TO 999
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         IP = 19
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), RM)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID, (TMPBUF(I),I=4,19),
     *      RM
         DO 800 I = 1,6
            IF (IXREF(1,I) .EQ. 0) GO TO 810
            IF (IXREF(1,I) .GT.0) THEN
               IP = 20 + (I-1) * 18 + 7
               JP = (2 * MOD (IP, NWD)) + 1
               IP = (IP / NWD) + 1
               CALL ZC8CL (2, JP, RECORD(IP), FC)
               IP = 20 + (I-1) * 18 + 17
               JP = (2 * MOD (IP, NWD)) + 1
               IP = (IP / NWD) + 1
               CALL ZC8CL (1, JP, RECORD(IP), TC)
               WRITE (9,2001) (IXREF(K,I), K=1,7), FC, (IXREF(K,I),
     *            K=9,17), TC
               END IF
 800        CONTINUE
 810     CONTINUE
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT20XX: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT20XX: BASELINE NUMBER OUT OF RANGE ',I5)
 1002 FORMAT (I2,'/',I2,'/',I2)
 1010 FORMAT ('AT20XX: Unidentified Stokes parameter: ',A1)
 2000 FORMAT (/' ID=',I5, ' IBASE=',I5,' baseline =',A, ' IHPOS1=',I7/,
     *   1X,'IEYR=',I5, 1X,'IEDOY=',I5, 1X,'IEMON=',I5, 1X,'IEDOM=',I5,
     *   1X,'IEHR=',I5, 1X,'IEMIN=',I5, 1X,'IESEC=',I5/,
     *   1X,'IHPOS2=',I7, 1X,'NFP=',I7, 1X,'KTPASS=',I7, 1X,'KSEL=',I7,
     *   1X,'KMODE=',I7, 1X,'NTRKS=',I7, 1X,'LSTIDX=',I7,
     *   1X,'ITPASS=',I7, ' Rec. mode=',A/, ' IXREF:')
 2001 FORMAT (1X,7I7,1X,A,9I7,1X,A)
      END
      SUBROUTINE AT2100 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 2100 records from A tape.
C   These records contain ASCII information including source name.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      NBITWD   I          # bits /word
C      MKSNAM   C*8(base#) Source name.
C      MKCVER   C*8        COREL revision level.
C      MKBASL   I          Baseline no of current ID record.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   ID, IBASE, IP, IPS, NCOMNT, NCONV
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Extract rec ID, baseline #
      NCONV = 1
      CALL ZI16IL (NCONV, 1, RECORD, ID)
      CALL INDXEQ (2, 16, NBITWD, IP, IPS, 16)
      CALL ZI16IL (NCONV, IPS, RECORD(IP), IBASE)
C                                       Check ID
      IF ((ID/100) .NE. 21) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE .LE. 0) .OR. (IBASE .GT. MXBAS)) THEN
         WRITE (MSGTXT,1010) IBASE
         IRET = 2
         GO TO 990
         END IF
C
      IF ((MKBL(1,IBASE) .LE. 0) .OR. (MKBL(2,IBASE) .LE. 0))GO TO 999
C                                       Source name.
      NCONV = 8
      CALL INDXEQ (13, 16, NBITWD, IP, IPS, 8)
      CALL ZC8CL (NCONV, IPS, RECORD(IP), MKSNAM(IBASE))
C                                       COREL revision level.
      CALL INDXEQ (37, 16, NBITWD, IP, IPS, 8)
      CALL ZC8CL (NCONV, IPS, RECORD(IP), MKCVER)
C                                       Any comments ?
      CALL INDXEQ (57, 16, NBITWD, IP, IPS, 16)
      CALL ZI16IL (1, IPS, RECORD(IP), NCOMNT)
      IF (NCOMNT .GT. 0) THEN
         MSGTXT = '2100 COMMENTS'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C
990   CALL MSGWRT (6)
C
 999  RETURN
C---------------------------------------------------------------------
1000  FORMAT ('AT2100: WRONG RECORD ID=',I5)
1010  FORMAT ('AT2100: BASELINE NUMBER OUT OF RANGE ',I5)
      END
      SUBROUTINE AT2200 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 2200 records from A tape.
C   These records have antenna coordinates.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKBASL   I          Baseline no of current ID record.
C      ANIDLY   D(2,base#) Antenna instr. delay (ref,rem stn)
C                          (us) (unused).
C      ANTCLK   D(2,base#) Offset clock rate (ref,rem stn) (s/s)
C                          > 0 = fast.
C      ANTEPO   D(2,base#) Antenna clock ref epoch (s since BOY).
C      ANTLOC   D(stn#,3,  XYZ coordinates for stn#1=ref,2=rem
C               base#)     for baseline base# (left-handed).
C      ANTSYN   D(2,base#) Clock sync (ref,rem stn) (us) >0 = fast.
C      ANTZEN   D(2,base#) Zenith atmosphere elec. delay (ns).
C      MKVLIG   D(base#)   Speed of light (m/s).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, ICTYP1, ICTYP2, IA1, IA2, I, IDD, IHH,
     *   IMM, NWD, IP, JP, IANT
      CHARACTER BID*2
      DOUBLE PRECISION DPTMP(32), COORD1(2), COORD2(2), COORD3(2),
     *   DETAB(2), DITAB(2), DRTAB(2), ZENATM(2), DEPOCH(2), XXX(2),
     *   YYY(2), ZZZ(2), VLIGHT, TEMP, DTEMP(32)
      REAL      RTEMP(64)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (DTEMP, RTEMP)
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE),
     *   (TMPBUF(4),ICTYP1), (TMPBUF(5),ICTYP2)
      EQUIVALENCE (TMPBUF(513), DPTMP), (DPTMP(3),COORD1),
     *   (DPTMP(5),COORD2), (DPTMP(7),COORD3), (DPTMP(9),DETAB),
     *   (DPTMP(11),DITAB), (DPTMP(13),DRTAB), (DPTMP(15),ZENATM),
     *   (DPTMP(17),DEPOCH), (DPTMP(19),XXX), (DPTMP(21),YYY),
     *   (DPTMP(23),ZZZ), (DPTMP(25),VLIGHT)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.22) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert double precision
      NCONV = 30
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 3, DTEMP, DPTMP(3))
      DO 200 IANT = 1,2
C                                       Save values: antenna locations.
         ANTLOC(IANT,1,IBASE) = XXX(IANT) * VLIGHT * 1.0D-6
         ANTLOC(IANT,2,IBASE) = YYY(IANT) * VLIGHT * 1.0D-6
         ANTLOC(IANT,3,IBASE) = ZZZ(IANT) * VLIGHT * 1.0D-6
C                                       Clock sync
         ANTSYN(IANT,IBASE) = DETAB(IANT)
C                                       Instrumental delay
         ANIDLY(IANT,IBASE) = DITAB(IANT)
C                                       Clock rate
         ANTCLK(IANT,IBASE) = DRTAB(IANT)
C                                       Zenith opacity
         ANTZEN(IANT,IBASE) = ZENATM(IANT)
C                                       Clock reference epoch
         ANTEPO(IANT,IBASE) = 0.0D0
         IF (DEPOCH(IANT) .NE. 0) THEN
            TEMP = DEPOCH(IANT) * 1.0D-4
            IDD = TEMP + 0.0001D0
            TEMP = (TEMP - IDD) * 100.0D0
            IHH = TEMP + 0.0001D0
            IMM = (TEMP - IHH) * 100.0D0 + 0.0001D0
            ANTEPO(IANT,IBASE) = (IDD-1) * 86400.0D0 + IHH * 3600.0D0 +
     *         IMM * 60.0D0
            END IF
200      CONTINUE
C                                       Speed of light
      MKVLIG(IBASE) = VLIGHT
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID, TMPBUF(4), TMPBUF(5)
         WRITE (9,2001) (DPTMP(I),I=3,25)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT2200: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT2200: BASELINE NUMBER OUT OF RANGE ',I5)
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A,
     *   1X,'ICTYP1=',I5, 1X,'ICTYP2=',I5)
 2001 FORMAT (1X,'COORD1=',1PD25.15,D25.15/, 1X,'COORD2=',2D25.15/,
     *   1X,'COORD3=',2D25.15/,
     *   1X,'DETAB=',2D25.15, 1X,'DITAB=',2D25.15/,
     *   1X,'DRTAB=',2D25.15, 1X,'ZENATM=',2D25.15/,
     *   1X,'DEPOCH=',2D25.15/,
     *   1X,'X=',2D25.15/,
     *   1X,'Y=',2D25.15/,
     *   1X,'Z=',2D25.15/,
     *   1X,'VLIGHT=',D25.15)
      END
      SUBROUTINE AT2300 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 2300 records from A tape.
C   These records have baseline and source information.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKNFP    I(base#)   No frames/parameter period used by corr.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKBASL   I          Baseline no of current ID record.
C      MKDEC    D(base#)   Declination of date (rad).
C      MKDECI   D(base#)   DEC of source (epoch MKEPOC) (rad).
C      MKEPOC   D(base#)   Coordinate epoch (yr: eg 1950.0, 2000.0).
C      MKPPHA   D(3)       Pulsar phase polynomial coeff. (unused).
C      MKRA     D(base#)   Right ascension of date (rad).
C      MKRAIN   D(base#)   RA of source (epoch MKEPOC) (rad).
C      MKPDIS   D          Pulsar dispersion measure (unused).
C      MKPEPO   D          Epoch for MKPPHA (unused).
C      MKINT    D(base#)   Correlator integration time (days).
C      MKPMDC   D(base#)   Derivative of apparent DEC wrt UT (rad/s)
C      MKSIDD   D(base#)   Derivative of GAST wrt UT (rad/s).
C      MKSIDT   D(base#)   GAST at a priori epoch (rad).
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C      MKUT1    D(base#)   UT1-UTC of date (s).
C      MKWOB    D(2,base#) X,Y pole position of date (arc-sec).
C      MKPMRA   D          Derivative of apparent RA wrt UT (rad/s)
C                          (unused).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, IA1, IA2, I, NWD, IP, JP
      CHARACTER BID*2
      DOUBLE PRECISION DPTMP(32), BX, BY, BZ, BLEN, RAIN, DECIN, EPOCIN,
     *   RA, DEC, PPHASE(3), PEPOCH, PDISPR, SRATE, UT1OF, RARAD,
     *   DECRAD, SIDTIM(2), WOB(2), USCALE, JACOB(2,2), DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (DTEMP, RTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE)
      EQUIVALENCE (TMPBUF(513), DPTMP), (DPTMP(3),BX), (DPTMP(4),BY),
     *   (DPTMP(5),BZ), (DPTMP(6),BLEN), (DPTMP(7),RAIN),
     *   (DPTMP(8),DECIN), (DPTMP(9),EPOCIN), (DPTMP(10),RA),
     *   (DPTMP(11),DEC), (DPTMP(12),PPHASE), (DPTMP(16),PEPOCH),
     *   (DPTMP(17),PDISPR), (DPTMP(19),SRATE), (DPTMP(20),UT1OF),
     *   (DPTMP(22),RARAD), (DPTMP(23),DECRAD), (DPTMP(24),SIDTIM),
     *   (DPTMP(26),WOB), (DPTMP(28),USCALE), (DPTMP(29),JACOB)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.23) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert double precision
      NCONV = 30
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 3, DTEMP, DPTMP(3))
C                                       Positions and epoch
      CALL ATF2RA (RAIN, 1, MKRAIN(IBASE))
      CALL ATF2RA (DECIN, 2, MKDECI(IBASE))
      MKEPOC(IBASE) = EPOCIN
      CALL ATF2RA (RA, 1, MKRA(IBASE))
      CALL ATF2RA (DEC, 2, MKDEC(IBASE))
C                                       Pulsar stuff
      MKPPHA(1) = PPHASE(1)
      MKPPHA(2) = PPHASE(2)
      MKPPHA(3) = PPHASE(3)
      MKPEPO = PEPOCH
      MKPDIS = PDISPR
C                                       Sample rate
      MKSMRT(IBASE) = SRATE
C                                       Integration time (days)
      MKINT(IBASE) = ((MKNFP(IBASE) * 20000.0D0) / SRATE) / 86400.0D0
      IF (MKINT(IBASE).LE.1.0E-10) MKINT(IBASE) = 0.1D0/86400.0D0
C                                       UT1-UTC
      MKUT1(IBASE) = UT1OF
C                                       Proper motion
      MKPMRA = RARAD
      MKPMDC(IBASE) = DECRAD
C                                       GAST and deriv.
      MKSIDT(IBASE) = SIDTIM(1)
      MKSIDD(IBASE) = SIDTIM(2)
C                                       Polar position
      MKWOB(1,IBASE) = WOB(1)
      MKWOB(2,IBASE) = WOB(2)
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID
         WRITE (9,2001) (DPTMP(I),I=3,32)
         WRITE (9,2002) MKRAIN(IBASE), MKDECI, MKRA(IBASE),
     *      MKDEC(IBASE), MKINT(IBASE)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT2300: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT2300: BASELINE NUMBER OUT OF RANGE ',I5)
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A)
 2001 FORMAT (1X,'BX=',1PD25.15, 1X,'BY=',D25.15, 1X,'BZ=',D25.15/,
     *   1X,'BLEN=',D25.15/,
     *   1X,'RAIN=',D25.15, 1X,'DECIN=',D25.15, 1X,'EPOCIN=',D20.10/,
     *   1X,'RA=',D25.15, 1X,'DEC=',D25.15/,
     *   1X,'PPHASE=',4D25.15/,
     *   1X,'PEPOCH=',D25.15, 1X,'PDISPR=',D25.15, 1X,'Unused=',D20.10/,
     *   1X,'SRATE=',D25.15, 1X,'UT1OF=',D25.15, 1X,'TWOPI=',D25.15/,
     *   1X,'RARAD=',D25.15, 1X,'DECRAD=',D25.15, 1X,'SIDTIM=',2D25.15/,
     *   1X,'WOB=',2D25.15, 1X,'USCALE=',D25.15/,
     *   1X,'JACOB=',4D25.15)
 2002 FORMAT (1X,'MKRAIN=',1PD25.15, 1X,'MKDECI=',D25.15/,
     *   1X,'MKRA=',D25.15, 1X,'MKDEC=',D25.15, 1X,'MKINT=',D25.15)
      END
      SUBROUTINE ATF2RA (FUNKY, ITYPE, RADIAN)
C-----------------------------------------------------------------------
C   Converts funky Haystack position notation to radians.
C   Input:
C      FUNKY   D  Time or position in HHMMSS.SS or DDMMSS.SSS
C      ITYPE   I  1 => HHMMSS, 2 = DDMMSS
C   Output:
C      RADIAN  D  Value in radians.
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNKY, RADIAN
      INTEGER   ITYPE
C
      INTEGER   DD, MM
      REAL      SEC
      DOUBLE PRECISION WORK, XSIGN, DTOR
C-----------------------------------------------------------------------
      DTOR = (8.0D0 * ATAN(1.0D0)) / 360.0D0
C                                       Sign
      XSIGN = 1.0D0
      IF (FUNKY.LT.0) XSIGN = -1.0D0
C                                       Crunch value
      WORK = ABS (FUNKY)
      DD = WORK * 1.0D-4 + 0.0001D0
      WORK = WORK - DD * 1.0D4
      MM = WORK * 1.0D-2 + 0.0001D0
      SEC = WORK - MM * 1.0D2
C                                       Put it together
      RADIAN = XSIGN * DTOR * (DD + (MM/60.0D0) + (SEC/3600.0D0))
      IF (ITYPE.EQ.1) RADIAN = RADIAN * 15.0D0
C
 999  RETURN
      END
      SUBROUTINE AT24XX (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 24XX records from A tape.
C   These records have RF frequency tables.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      DOSIDB   L          If true separate sidebands by IF
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKFRQ    D(freq#)   RF frequency at baseband in video for each
C                          of up to 28 frequencies for the current
C                          scan-baseline data (Hz) (>0 USB; < 0 LSB).
C      MKNFRQ   I(base#)   No of freq in MKFRQ per baseline.
C      MKLOST   L          If true then LO table exists (2700 rec).
C      MKBASL   I          Baseline no of current ID record.
C      MKFFQ    D(*)       List of RF frequencies found
C      MKNFFQ   I          Number of entries in MKFFQ
C      LSBOFF   D          Correction for LSB IF frequencies.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, NFR, NFREQ, LOFST, ICONT, IP, LOOP,
     *   IA1, IA2, NWD, JP
      CHARACTER BID*2
      DOUBLE PRECISION DPTMP(32), FRQTAB(28), DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (RTEMP, DTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(4),NFR),
     *   (TMPBUF(5), NFREQ), (TMPBUF(6),LOFST)
      EQUIVALENCE (TMPBUF(513), DPTMP), (DPTMP(3),FRQTAB)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.24) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert double precision
      NCONV = 30
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 3, DTEMP, DPTMP(3))
C                                       Check no. freq.
      IF (NFREQ.GT.MXM3FQ) THEN
         WRITE (MSGTXT,1002) NFREQ, MXM3FQ
         IRET = 3
         GO TO 990
         END IF
C                                       Default = 2 Mhz bandpass
      IF (MKSMRT(IBASE).LE.0.0) MKSMRT(IBASE) = 4.0E6
C                                       If separating the sidebands
C                                       change the frequency of LSB to
C                                       the first channel
      IF (DOSIDB) THEN
C                                        Correction for lower sideband
         LSBOFF = MKSMRT(IBASE) * 0.5
         DO 50 LOOP = 1,NFREQ
            IF (FRQTAB(LOOP).LT.0)
     *         FRQTAB(LOOP) = -(ABS (FRQTAB(LOOP)) -  LSBOFF)
 50         CONTINUE
         END IF
C                                       Update "found frequencies" list.
      CALL UPFLIS (NFR, FRQTAB, MXM3FQ, MKNFFQ, MKFFQ)
C                                        Take care of double sideband
C                                        with one missing.
      CALL DSBFIX (LSBOFF, MXM3FQ, MKNFFQ, MKFFQ)
C                                       Get data
      MKLOST = LOFST.EQ.1
      MKNFRQ(IBASE) = NFREQ
      ICONT = MOD (ID, 2400)
      IP = ICONT * 28
      DO 100 LOOP = 1,NFR
         MKFRQ(IP+LOOP) = FRQTAB(LOOP)
 100     CONTINUE
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID, TMPBUF(4), TMPBUF(5),
     *      TMPBUF(6)
         IP = 3 + NFR - 1
         WRITE (9,2001) (DPTMP(LOOP),LOOP=3,IP)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT24XX: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT24XX: BASELINE NUMBER OUT OF RANGE ',I5)
 1002 FORMAT ('AT24XX: TOO MANY FREQUENCIES ',I3,' > MAX. ',I3)
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A,
     *   1X,'NFR=',I5, 1X,'NFREQ=',I5, 1X,'LOFST=',I5)
 2001 FORMAT (1X,'FRQTAB:'/, 6(1X,5D23.15/))
      END
      SUBROUTINE AT25XX (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 25XX records from A tape.
C   These records have phase cal tone frequencies.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKPFRQ   D(freq#)   Phase calibration tone frequencies for the
C                          current scan baseline data (Hz). In same
C                          order as MKFRQ.
C      MKBASL   I          Baseline no of current ID record.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, NPC, NPCFRQ, ICONT, IP, LOOP, IA1,
     *   IA2, NWD, JP
      CHARACTER BID*2
      DOUBLE PRECISION DPTMP(32), PCFREQ(28), DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (DTEMP, RTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(4),NPC),
     *   (TMPBUF(5), NPCFRQ)
      EQUIVALENCE (TMPBUF(513), DPTMP), (DPTMP(3),PCFREQ)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.25) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert double precision
      NCONV = 30
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 3, DTEMP, DPTMP(3))
C                                       Check no. freq.
      IF (NPCFRQ.GT.MXM3FQ) THEN
         WRITE (MSGTXT,1002) NPCFRQ, MXM3FQ
         IRET = 3
         GO TO 990
         END IF
C                                       Get data
      ICONT = MOD (ID, 2500)
      IP = ICONT * 28
      DO 100 LOOP = 1,NPC
         MKPFRQ(IP+LOOP) = PCFREQ(LOOP)
 100     CONTINUE
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID, TMPBUF(4), TMPBUF(5)
         IP = 3 + NPC - 1
         WRITE (9,2001) (DPTMP(LOOP),LOOP=3,IP)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT25XX: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT25XX: BASELINE NUMBER OUT OF RANGE ',I5)
 1002 FORMAT ('AT25XX: TOO MANY FREQUENCIES ',I3,' > MAX. ',I3)
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A2,
     *   1X,'NPC=',I5, 1X,'NPCFRQ=',I5)
 2001 FORMAT (1X,'PCFREQ:'/, 6(1X,5D23.15/))
      END
      SUBROUTINE AT2600 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 2600 records from A tape.
C   These records have geometric and time information.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKBASL   I          Baseline no of current ID record.
C      MKAZ     D(ant#)    Antenna azimuth at PRT (deg) (unused).
C      MKEL     D(ant#)    Antenna elevation at PRT (deg) (unused).
C      MKACC    D(base#)   Total a priori delay acc at PRT (us/s/s).
C      MKDHAD   D(base#)   Hour angle rate (rad/s).
C      MKDLY    D(base#)   Total a priori delay at PRT (us).
C      MKGHA    D(base#)   GHA of source at PRT (deg) (unused).
C      MKPRT    D(base#)   Processing reference time (PRT) as used by
C                          MKIII correlator (ms since BOY). Should
C                          equal a priori epoch.
C      MKRAT    D(base#)   Total a priori delay rate at PRT (us).
C      MKSRAT   D(base#)   Sidereal rate (rad/UT ms).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, IA1, IA2, LOOP, NWD, IP, JP
      CHARACTER BID*2
      DOUBLE PRECISION DPTMP(32), DACELT, DELAYT, DRATET, U, V, UF, VF,
     *   AZ(2), EL(2), GHA, TIME0, UTOST1, BQA, BQB, BQC, DHADT, BTR0,
     *   EPOCH0, BTE0, BTR, BTI, TWOPI, U0, V0, UF0, VF0, DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (RTEMP, DTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE)
      EQUIVALENCE (TMPBUF(513), DPTMP), (DPTMP(2), DACELT),
     *   (DPTMP(3),DELAYT), (DPTMP(4),DRATET), (DPTMP(6),U),
     *   (DPTMP(7),V), (DPTMP(8),UF), (DPTMP(9),VF), (DPTMP(10),AZ),
     *   (DPTMP(12),EL), (DPTMP(14),GHA), (DPTMP(15),TIME0),
     *   (DPTMP(16),UTOST1), (DPTMP(17),BQA), (DPTMP(18),BQB),
     *   (DPTMP(19),BQC), (DPTMP(20),DHADT), (DPTMP(21),BTR0),
     *   (DPTMP(22),EPOCH0), (DPTMP(23),BTE0), (DPTMP(24),BTR),
     *   (DPTMP(25),BTI), (DPTMP(26),TWOPI), (DPTMP(27),U0),
     *   (DPTMP(28),V0), (DPTMP(29),UF0), (DPTMP(30),VF0)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 2
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.26) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert double precision
      NCONV = 31
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 2, DTEMP, DPTMP(2))
C                                       Save data
      MKPRT(IBASE) = TIME0
      MKDLY(IBASE) = DELAYT
      MKRAT(IBASE) = DRATET
      MKACC(IBASE) = DACELT
      MKAZ(IA1) = AZ(1)
      MKAZ(IA2) = AZ(2)
      MKEL(IA1) = EL(1)
      MKEL(IA2) = EL(2)
      MKGHA(IBASE) = GHA
      MKSRAT(IBASE) = UTOST1
      MKDHAD(IBASE) = DHADT
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID
         WRITE (9,2001) (DPTMP(LOOP),LOOP=2,30)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT2600: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT2600: BASELINE NUMBER OUT OF RANGE ',I5)
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A)
 2001 FORMAT (1X,'DACELT=',1PD25.15, 1X,'DELAYT=',D25.15,
     *   1X,'DRATET=',D25.15, 1X,'Unused=',D20.10/,
     *   1X,'U=',D25.15, 1X,'V=',D25.15, 1X,'UF=',D25.15,
     *   1X,'VF=',D25.15/,
     *   1X,'AZ=',2D25.15, 1X,'EL=',2D25.15/,
     *   1X,'GHA=',D25.15, 1X,'TIME0=',D25.15, 1X,'UTOST1=',D25.15/,
     *   1X,'BQA=',D25.15, 1X,'BQB=',D25.15, 1X,'BQC=',D25.15/,
     *   1X,'DHADT=',D25.15, 1X,'BTR0=',D25.15, 1X,'EPOCH0=',D25.15/,
     *   1X,'BTE0=',D25.15, 1X,'BTR=',D25.15, 1X,'BTI=',D25.15,
     *   1X,'TWOPI=',D25.15/,
     *   1X,'U0=',D25.15, 1X,'V0=',D25.15, 1X,'UF0=',D25.15,
     *   1X,'VF0=',D25.15)
      END
      SUBROUTINE AT27XX (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 27XX records from A tape.
C   These records have LO offset tables.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKLOFF   D(2,freq#) LO offset for reference and remote stn.
C                          (1=ref,2=rem) in same order as MKFRQ (Hz).
C                          Sum of MKFRQ and MKLOFF gives RF freq.
C                          translated to DC at ref. station.(<0 LSB;
C                          >0 USB) LO offsets are not applied
C      MKBASL   I          Baseline no of current ID record.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      LOGICAL WNZERO
      INTEGER   NCONV, ID, IBASE, NFR, NFREQ, LOFSTB(2,28), IP, ICONT,
     *   LOOP, IA1, IA2, NWD, JP
      CHARACTER BID*2
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(4),NFR),
     *   (TMPBUF(5),NFREQ), (TMPBUF(9),LOFSTB)
C-----------------------------------------------------------------------
C                                       Initialisation
      WNZERO = .FALSE.
      IRET = 0
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.27) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Convert long integers
      NCONV = 60
      CALL ZI32IL (NCONV, 5, RECORD, TMPBUF(9))
C                                       Get data
      ICONT = MOD (ID, 2700)
      IP = ICONT * 28
      DO 100 LOOP = 1,NFR
         MKLOFF(1,IP+LOOP) = LOFSTB(1,LOOP) * 1.0D-3
         MKLOFF(2,IP+LOOP) = LOFSTB(2,LOOP) * 1.0D-3
         WNZERO = WNZERO .OR. (LOFSTB(1,LOOP) .NE. 0) .OR.
     *      (LOFSTB(2,LOOP) .NE. 0)
 100     CONTINUE
C                                       Warn that M3TAR requires
C                                       development for LO offsets.
      IF (WNZERO) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         IF (DOLOFF) THEN
            IRET = 0
         ELSE
            IRET = 3
            END IF
         END IF
C                                       Debug?
      IF (MKDBUG.GE.1) THEN
         NWD = NBITWD / 16
         IP = 2
         JP = (2 * MOD (IP, NWD)) + 1
         IP = (IP / NWD) + 1
         CALL ZC8CL (2, JP, RECORD(IP), BID)
         WRITE (9,2000) TMPBUF(1), TMPBUF(2), BID, TMPBUF(4), TMPBUF(5)
         WRITE (9,2001) (TMPBUF(LOOP),LOOP=9,64)
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT27XX: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT27XX: BASELINE NUMBER OUT OF RANGE ',I5)
 1010 FORMAT ('M3TAR: Requires development for non-zero LO offsets')
 2000 FORMAT (/' ID=',I5, 1X,'IBASE=',I5, 1X,'baseline=',A2,
     *   1X,'NFR=',I5, 1X,'NFREQ=',I5)
 2001 FORMAT (1X,'LOFSTB:'/, 6(1X,10I12/))
      END
      SUBROUTINE AT280X (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 280X records from A tape.
C   These records have random data; nothing is copied to common but the
C   data is translated.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C   Input/output via common
C      TMPBUF   I(1024)    Work buffer.
C      MKBASL   I          Baseline no of current ID record.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, STAID, IATYPE, NA, NI, IP, IA1, IA2
      DOUBLE PRECISION DPTMP(32), DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (RTEMP, DTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE),
     *   (TMPBUF(5),IATYPE), (TMPBUF(6),NA), (TMPBUF(7),NI)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 5
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.28) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check baseline
      MKBASL = IBASE
      IF ((IBASE.LE.0).OR.(IBASE.GT.MXBAS)) THEN
         WRITE (MSGTXT,1001) IBASE
         IRET = 2
         GO TO 990
         END IF
      IA1 = MKBL(1,IBASE)
      IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
      IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
      IF (ID.EQ.2800) THEN
         STAID = IA1
      ELSE
         STAID = IA2
         END IF
C                                       Convert double precision
      NCONV = NA
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 3, DTEMP, DPTMP(3))
C                                       Convert other integers
      NCONV = NI
      IP = (3 + NA - 1) * 4 + 1
      CALL ZI16IL (NCONV, IP, RECORD, TMPBUF(IP))
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT280X: WRONG RECORD ID =',I5)
 1001 FORMAT ('AT280X: BASELINE NUMBER OUT OF RANGE ',I5)
      END
      SUBROUTINE ATYP51 (RECORD, IEXT, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 51 records from A tape.
C   These records have correlation data.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C      IEXT     I      Extent No.
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C   Input from common
C      NBITWD   I          # bits /word
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCMOD   I(corr#)   No of lags per module for this correlator
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKMNLG   I(corr#)   Min module delay offset for this corr.
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKDBUG   I          Debug dump flag. 0=no debug dump;
C                          1=dump headers only; 2=dump hdrs and
C                          data.
C      MKDROP   R          Max dropout rate allowed [0,1]
C                          (default 0.5).
C      MKMPER   R          Max parity error rate allowed [0,1]
C                          (default 0.01).
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKNTYP   I          Highest correlation type accepted (now 2).
C      MKPRNT   I          Print level (0..4).
C      MKPFQT   D(pcid#,   Set of phase cal freq in same order as
C               freq#)     MKFRQ. Each set is identified by a
C                          phase cal id pcid#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBLNX   I(2,base#) 1=Start, 2=stop pointers for baseline
C                          base# in cross-ref index table MKXREF.
C      MKBPFQ   I(base#)   Phase cal id #pcid for baseline base#.
C      MKNBL    I          No of baselines in antenna table MKBL.
C      MKLSCN   I          Nominal length of current scan (sec).
C      MKTMS    D          Scan start time (mmss in days).
C      MKINT    D(base#)   Correlator integration time (days).
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C      MKDOPL   L          True if multiple polarizations to be read.
C      LXTNT    I(256)     Pointer from EXTENT to BASLINE No.
C   Input/output via common
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKER51   I(mod#,    No of type 51 records rejected with error
C               error#)    number error# for module mod#.
C      MKNR51   I(mod#)    Total no of type 51 data records read
C                          for module mod#.
C      MKSERL   I(mod#)    Serial no of correlator module mod#.
C      MKNSER   I          No of entries in MKSERL.
C      MKDATA   R(2,addr#) Visibility data from current scan-baseline
C                          addr# = (time# * Ncorr + corr# - 1) * Nlag
C                             + lag#
C                          time# = (data time - scan start) /int time
C                          corr# = correlator no; Ncorr = No. corr.
C                          lag# = lag no; Nlag = No of lags.
C      MKFBS    R(4,time#) MKFBS(1,:) = average fractional bit delay
C                          error over accumulation period at time#.
C                          MKFBS(2,:) = number of delay steps in AP.
C                          MKFBS(3-4,:) = times from beginning/end
C                          of AP to next/previous delay step (bits).
C      MKTOK    L(time#)   Valid visibility data for this time# in
C                          array MKDATA ?
C      MKWT     R(2,addw#) Lag function weight.
C                          addw# = time# * Ncorr + corr#.
C      MKFRPS   L          True if fringe rate > 0 on baseline.
C      MKNTIM   I          Max time index time# used in MKDATA.
C      TMPBUF   I(1024)    Work buffer.
C      MKPCAL   R(parm#,   Phase calibration values from current
C               stn#,corr#) scan-baseline. parm#: 1=sum cos;
C                          2=sum cos**2; 3=sum sin; 4=sum sin**2;
C                          5=n. stn# = 1(ref),2(rem).
C      MKXREF   I(parm#,   Cross reference table for correlator
C               indx#)     module type 51 data records.
C                          indx# = corr module index wrt start
C                          index for curr baseline (MKBLNX(1,base#)
C                          parm#: 1= Type 51 module index (unused);
C                          2= Ref stn track no (unused); 3= Remote
C                          stn track no (unused); 4= Freq index in
C                          MKFRQ.; 5= Partner track on ref. stn tape
C                          (track on opposite side of same VC)
C                          (unused); 6= Module delay offset (bits);
C                          7= Pulsar gate-on (milli-periods)(unused);
C                          8= Pulsar gate-off (milli-periods) (unus-
C                          ed).; 9= Module termination code (from
C                          COREL at end of scan (2-11 ok); 10= Extent
C                          no; 11= Polarization code (1=RR,2=LL,
C                          3=RL,4=LR); 12= Correlator module serial
C                          no (<0:MK3A, >0: MK3).
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKNTOF   I          No of type 51 data records used in
C                          computing the scan start time offset.
C      MKTOFF   D          Time offset of current scan baseline
C                          data from nominal scan start time (days).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      LOGICAL WACFN, WERROR(20), WBAD, WCRCC, WXSYS
      INTEGER   NCONV, HALF, BTEMP(16), BITS(16,8), I,
     *   IP1, POWERS(16), IBASE, IA1, IA2, INDEX, CMODE, NOCCOR,
     *   DELAYZ, XCOR(2,8), NCOS, NSIN, XPCCOS, XPCSIN, NXPC, XERR,
     *   YPCCOS, YPCSIN, NYPC, YERR, MM, IBFULL, IBPART, TIMNO, LAGNO,
     *   ADDR, INDX, ISH, FREQNO, IT1, IT2, ISTOKE, ICORR, ITSTAT, J
      INTEGER IM, KL, IWORD2, ADDRW, MAXPER, MAXDRP, N1, N2, ICLAG,
     *   IMODLE, IERRV, JBASE, IEXT
      REAL SS, FB, F, XNC, XNS, C, FRSIGN, FBSCOR, DD, SH, XC, XS, YC,
     *   YS, TC, TS, ROT, RS, RC, XPHS, YPHS, FXNORM, FANORM
      DOUBLE PRECISION TIMSCN, MMSS, DPFREQ, DERRTM, DSCANL, TIMTMP,
     *   DERRV
C
      INTEGER IJ
      REAL HIGHNB
C
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKDAT.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKOTH.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(5), DELAYZ), (TMPBUF(7),XCOR),
     *   (TMPBUF(23),NCOS), (TMPBUF(24),NSIN), (TMPBUF(25),XPCCOS),
     *   (TMPBUF(26),XPCSIN), (TMPBUF(27),NXPC), (TMPBUF(28),XERR),
     *   (TMPBUF(29),YPCCOS), (TMPBUF(30),YPCSIN), (TMPBUF(31),NYPC),
     *   (TMPBUF(32),YERR)
      DATA POWERS /1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     *   16383,32768/
C                                       Normalisation factors for MKIII
C                                       correlator: both include
C                                        1st order Van Vleck (pi/2).
C                                        Cross-correlations (includes
C                                       fringe rotator and FBS losses)
      DATA FXNORM /1.03705/
C                                       Autocorrelations
      DATA FANORM /1.57079633/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Loop over half
      DO 500 HALF = 1,2
         IP1 = (HALF-1) * 64 / (NBITWD / 16)
C                                       Bitshit - get bytes as integers
         NCONV = 12
         CALL ZI8IL (NCONV, 1, RECORD(IP1+1), BTEMP)
C                                       Baseline code byte 1, bits 8:13
         IBASE = BTEMP(1) - ((BTEMP(1)/64) * 64)
C                                       Extent. Basel. No. 0-2 may be
C                                       64-66
         JBASE = LXTNT(IEXT)
         IF ((IBASE .GE. 0) .AND. (IBASE .LE. 2)) THEN
            IF ((IBASE .NE. JBASE) .AND. (JBASE .GT. 0))IBASE = JBASE
         ELSE
C                                       Should not happen!
            IF ((IBASE .NE. JBASE) .AND. (JBASE .GT. 0)) THEN
               WRITE (MSGTXT, 1500) IBASE, JBASE
               CALL MSGWRT (6)
               GO TO 500
            ENDIF
         END IF
C                                       Check baseline
         IF (IBASE.EQ.0) GO TO 500
         MKBASE = IBASE
         IF ((IBASE.LE.0).OR.(IBASE.GT.MKNBL)) THEN
            WRITE (MSGTXT,1001) IBASE
            CALL MSGWRT (6)
            GO TO 500
            END IF
         IA1 = MKBL(1,IBASE)
         IA2 = MKBL(2,IBASE)
C                                       Desired baseline?
         IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 999
C                                       Index-byte 2, bits 0:7
         INDEX = BTEMP(2)
C                                       Skip debug records.
         CALL ZI16IL (1, 2, RECORD(IP1+1), IWORD2)
         IF (IWORD2 .EQ. -1) GO TO 500
C                                       Bits for word 1 (bits 15-8).
         CALL ZGTBIT (8, BTEMP(1), BITS(9,1))
C                                       Bits for word 2 (bits 15-0).
         CALL ZGTBIT (8, BTEMP(3), BITS(9,2))
         CALL ZGTBIT (8, BTEMP(4), BITS(1,2))
C                                       Fringe rate positive?
         MKFRPS = BITS(16,2).EQ.0
C                                       Correlator mode
         CMODE = BITS(11,2)*POWERS(3) + BITS(10,2)*POWERS(2) +
     *      BITS(9,2)
C                                       No of lags in module.
         ICLAG = 8
         IF (BITS(12,2) .EQ. 1) ICLAG = 16
C                                       Skip unexpected modes.
         IF (CMODE .GT. MKNTYP) GO TO 500
C                                       AC or XC ?
         WACFN = ((CMODE .EQ. 0) .AND.
     *      (MKBL(1,MKBASE) .EQ. MKBL(2,MKBASE))) .OR.
     *      (CMODE .EQ. 1) .OR. (CMODE .EQ. 2)
C                                       Select normalisation factor
         F = FXNORM
         IF (WACFN) F = FANORM
C                                       Set high number threshold
         HIGHNB = 0.65
         IF (WACFN) HIGHNB = 1.0
C
C
         NOCCOR = 8 + BITS(12,2) * 8
C                                       Correlator status
C                                       Fractional bit stuff
C                                       Full-byte 7, bits 4:7
C                                       part-byte 7, bits 1:3 and
C                                       byte 8 bits 0:7
         IBFULL = BTEMP(7) / 8
         IBPART = (BTEMP(7) - IBFULL*8) * 256 + BTEMP(8)
         IF (IBFULL.GT.128) IBFULL = IBFULL - 128
C                                       Time (would you believe BCD???)
C                                       min (as mm) byte 9
         IT1 = BTEMP(9) / 16
         IT2 = BTEMP(9) - IT1*16
         MM = 10 * IT1 + IT2
C                                       Integer sec as (ss) byte 10
         IT1 = BTEMP(10) / 16
         IT2 = BTEMP(10) - IT1*16
         SS = 10.0 * IT1 + IT2
C                                       0.01 sec as (.ss) byte 11
         IT1 = BTEMP(11) / 16
         IT2 = BTEMP(11) - IT1*16
         SS = SS + 0.1 * IT1 + 0.01 * IT2
C                                       0.001 sec byte 12 bits 4:7
         IT1 = BTEMP(12) / 16
         SS = SS + IT1 * 0.001
C                                       Get bits for byte 12 (word 6)
         CALL ZGTBIT (8, BTEMP(12), BITS(1,6))
C                                       Get time since start of scan
         MMSS = (MM / 1440.0D0) + (SS / 86400.0D0)
C                                       New hour?
         IF (MMSS.LT.MKTMS) MMSS = MMSS + 1.0D0 / 24.0D0
         TIMSCN = MMSS - MKTMS
C                                       Fractional bit error at center
         NCONV = 1
         CALL ZRHPRL (NCONV, 6, RECORD(IP1+1), FB)
C                                       Convert long integers
         NCONV = 28
         CALL ZI32IL (NCONV, 5, RECORD(IP1+1), TMPBUF(5))
C
C                                       High number threshold. A bit
C                                       arbitrary... FOURFIT seems to
C                                       take 30% correl. for XC
C                                       Skip this data for now! or what?
         C = MAX (NCOS, NSIN) * F
         DO 40 IJ = 7, 22
            IF( TMPBUF(IJ) .GT. INT(C*HIGHNB) ) THEN
               WRITE (MSGTXT,1600)
               CALL MSGWRT (6)
               GO TO 500
            ENDIF
 40      CONTINUE
C
C                                       Convert to correlation coef. and
C                                       save.
         INDX = MKBLNX(1,IBASE) + INDEX - 1
C                                       One rel frequency no.
         FREQNO = ABS (MKXREF(4,INDX))
C                                       Stokes index.
         ISTOKE = MKXREF(11,INDX)
C                                       Search correlator table.
         ICORR = 0
         DO 50 I = 1,MKNCOR
            IF (MKCTYP(I) .LT. 0) GO TO 50
            IF ((FREQNO .NE. MKCFRQ(I)) .OR. (ISTOKE .NE. MKCSTK(I))
     *         .OR. (ICLAG .NE. MKCMOD(I))
     *         .OR. ((CMODE .NE. MKCTYP(I)) .AND. (MKCTYP(I) .NE. 999)))
     *         GO TO 50
C                                       Found:
C                                       If first data record for this
C                                       correlator then determine max
C                                       symmetrical lag range.
            IF (MKCTYP(I).EQ.999) CALL ATLAG (WACFN, I, IRET)
            IF (IRET .NE. 0) GO TO 990
            IF (MKCTYP(I).LT.0) GO TO 50
C                                       Correlator has correct lag range
               ICORR = I
               MKCTYP(ICORR) = CMODE
               GO TO 60
50          CONTINUE
C                                       Skip if correlator not found
C                                       or de-selected.
         IF (ICORR .EQ. 0) GO TO 500
         IF (MKCTYP(ICORR) .LT. 0) GO TO 500
C                                       Check module termination status.
60       ITSTAT = MKXREF(9,INDX)
C                                       Module delay offset.
         LAGNO = MKXREF(6,INDX)
C                                       Zero rel time number. Add 1ms
C                                       to prevent truncation errors.
         TIMTMP = TIMSCN + 1.0D0 / 86400.0D3
         TIMNO = TIMTMP / MKINT(MKBASE)
         IT1 = TIMNO + 1
C                                       Time mark offset from start
C                                       time should = n*AP + k*(x),
C                                       n,k = integers; x=frac(AP).
C                                       Calculate residual in ms.
         DERRTM = (MKINT(MKBASE) * TIMNO - TIMSCN) * 86400.0D3
         DERRTM = ABS (DERRTM) + 0.5D0
         DERRV = MKINT(MKBASE) * 86400.0D0
         IERRV = DERRV
C                                        Frac(AP) rounded to ms.
         IERRV = ((DERRV - IERRV) * 1000.0 + 0.5)
C                                       Set x=1s for integral AP.
         DERRV = 1000.0D0
C                                       Else x=frac(AP) (ms)
         IF (IERRV .GT. 1) DERRV = IERRV
         DERRTM = MOD (DERRTM, DERRV) - 0.5
C
         DSCANL = MKLSCN / 86400.0D0
C                                       Update type 51 error table.
         IMODLE = 0
         IF (MKPRNT .GE. 4) THEN
            DO 65 I = 1,MKNSER
               IF (MKXREF(12,INDX) .EQ. MKSERL(I)) IMODLE = I
65             CONTINUE
            IF (IMODLE .EQ. 0) THEN
               MKNSER = MKNSER + 1
               MKSERL(MKNSER) = MKXREF(12,INDX)
               IMODLE = MKNSER
               END IF
            END IF
         IF (IMODLE .EQ. 0) THEN
            IMODLE = 1
            MKNSER = 1
            END IF
C                                       Check module error status.
         MKNR51(IMODLE) = MKNR51(IMODLE) + 1
C                                       Correlate suppress.
         WERROR(1) = BITS(15,2) .GT. 0
C                                       Tapes out of sync.
         WERROR(2) = BITS(14,2) .GT. 0
C                                       A priori or AP array overrun.
         WERROR(3) = (BITS(13,2)+BITS(15,1)) .GT. 0
C                                       No PP update.
         WERROR(4) = BITS(8,2) .GT. 0
C                                       Y slipped sync.
         WERROR(5) = BITS(6,2) .GT. 0
C                                       X slipped sync.
         WERROR(6) = BITS(5,2) .GT. 0
C                                       No Y clock or X clock.
         WERROR(7) = (BITS(2,2)+BITS(1,2)) .GT. 0
C                                       Unexpected tape time.
         WERROR(8) = (BITS(3,6) .GT. 0) .OR. (TIMSCN .GT. DSCANL) .OR.
     *      (DERRTM .GT. 1) .OR. (BITS(4,6) .GT. 0)
C                                       CRCC error in rem stat sync blk
         WERROR(9) = BITS(2,6) .GT. 0
C                                       CRCC error in ref stat sync blk
         WERROR(10) = BITS(1,6) .GT. 0
C                                       Max PE count per AP.
         MAXPER = MKMPER * MKSMRT(IBASE) * MKINT(IBASE) * 86400.0 / 8.0
C                                       X parity errors exceed limit.
         WERROR(11) = XERR .GT. MAXPER
C                                       Y parity errors exceed limit.
         WERROR(12) = YERR .GT. MAXPER
C                                       Compute max dropout level/AP.
         MAXDRP = MKDROP * MKINT(MKBASE) * MKSMRT(MKBASE) * 86400.0
C                                       Total bits corr cos < MAXDRP.
         WERROR(13) = NCOS .LT. MAXDRP
C                                       Total bits corr sin < MAXDRP.
         WERROR(14) = NSIN .LT. MAXDRP
C                                       Module termination status bad.
         WERROR(15) = (ITSTAT .LT. 2) .OR. (ITSTAT .GT. 11)
C                                       MKIIIA module error.
         WERROR(16) = (MKXREF(12,INDX).LT.0) .AND. (BTEMP(5).NE.0)
C                                       Discard this record ?
         WBAD = .FALSE.
         DO 70 I = 1,16
C                                       Ignore sine channel for AC.
            IF (WACFN .AND. (I .EQ. 14)) GO TO 70
            IF (WERROR(I)) MKER51(IMODLE,I) = MKER51(IMODLE,I) + 1
C                                       Suppress CRCC/XS/YS errors
C                                       if requested.
            WXSYS = (I .EQ. 5) .OR. (I .EQ. 6)
            WCRCC = (I .EQ. 9) .OR. (I .EQ. 10)
            IF (WXSYS.AND.(MKMASK.GE.2)) GO TO 70
            IF (WCRCC.AND.((MKMASK.EQ.1).OR.(MKMASK.EQ.3))) GO TO 70
            WBAD = WBAD .OR. WERROR(I)
70          CONTINUE
C                                       Skip record if bad.
         IF (WBAD) GO TO 500
C                                       Flag time as good.
         IF (IT1 .GT. MXTIME) THEN
            WRITE (MSGTXT,1010)
            IRET = 2
            GO TO 990
            END IF
C                                       Determine time offset from
C                                       scan start time (in days).
         MKTOFF = MKTOFF + (MKINT(MKBASE) * TIMNO - TIMSCN)
         MKNTOF = MKNTOF + 1
         MKNTIM = MAX (MKNTIM, IT1)
         MKTOK(IT1) = .TRUE.
C                                       Correction for negative fringe
C                                       rates.
         IF (MKFRPS) THEN
            FRSIGN = 1.0
         ELSE
            FRSIGN = -1.0
            END IF
C                                       Fractional bit correction value
C                                       SH= bit shift rate; ISH = actual
C                                       number of bit shifts in AP.
         SH = IBFULL + IBPART / 2048.0
         N1 = ABS (-SH/2 + FB) + 0.5
         N2 = ABS (SH/2 + FB) + 0.5
         ISH = N1 + N2
         IF (ISH.EQ.((ISH/2)*2)) THEN
C                                       ISH even
            DD = 0.0
         ELSE
C                                       ISH odd
            DD = - SIGN (0.5, FB)
            END IF
         IF (SH .NE. 0) THEN
            FBSCOR = (FB + DD) * (1.0 - (ISH / SH))
         ELSE
            FBSCOR = 0.0
            END IF
         MKFBS(1,TIMNO+1) = FBSCOR
         MKFBS(2,TIMNO+1) = ISH
         MKFBS(3,TIMNO+1) = 0.5 - FB + 0.5*SH + NINT (FB - 0.5*SH)
         MKFBS(4,TIMNO+1) = 0.5 + FB + 0.5*SH - NINT (FB + 0.5*SH)
C                                       Compute address in buffer.
         ADDRW = TIMNO * MKNCOR + ICORR
         ADDR = (TIMNO * MKNCOR + (ICORR-1)) * MKNLAG
C                                       Check address range.
         IF (((ADDRW+MKNCOR) .GT. (MXHUGE/8)) .OR.
     *      ((ADDR+MKNLAG) .GT. MXHUGE)) THEN
            WRITE (MSGTXT,1003)
            IRET = 1
            GO TO 990
            END IF
C                                       Save bits correlated
         C = MAX (NCOS, NSIN) * F
         MKWT(ADDRW) = MAX (C, MKWT(ADDRW))
         XNC = NCOS * 1.0
         XNS = NSIN * 1.0
C                                        This loop requires modification
C                                       for correlator modules in AC
C                                       mode (not yet fully supported
C                                       by MK3 correlator software).
C                                       MKCMOD=16,32; MKCTYP=1,2.
         IM = MKCMOD(ICORR) / 2 + 1
         DO 100 I = 1,8
            KL = (IM - I) + LAGNO
            J = KL - MKCMIN(ICORR) + 1
C                                       Skip lags out of range.
            IF ((J .LT. 1) .OR. (J .GT. MKNLAG)) GO TO 100
            MKDATA(1,ADDR+J) = (F * (2*XCOR(1,I) - XNC)) / XNC
            IF ((XNS .EQ. 0) .OR. (WACFN)) THEN
               MKDATA(2,ADDR+J) = 0.0
            ELSE
               MKDATA(2,ADDR+J) = (-FRSIGN*F*(2*XCOR(2,I)-XNS)) / XNS
               END IF
 100        CONTINUE
C                                       Do phase cals only for lag #1
C                                       and phase cal present
         DPFREQ = MKPFQT(MKBPFQ(MKBASE),FREQNO)
         IF ((ABS (DPFREQ) .LT. 1.0D0) .OR.
     *      (LAGNO .NE. MKMNLG(ICORR))) GO TO 490
C                                       Accumulate Phase cal. Ref ant.
         IF ((NXPC.GT.5000) .AND.
     *      ((XPCCOS.LE.NXPC).AND.(XPCSIN.LE.NXPC))) THEN
            XNC = NXPC * 1.0
            TC = (2*XPCCOS - NXPC) / XNC
            TS = (2*XPCSIN - NXPC) / XNC
C                                       Remove rotation =
C                                       bit offset*PCFREQ/sampling rate
            ROT = 6.283185308 * MKXREF(6,INDX) * DPFREQ /MKSMRT(MKBASE)
            RC = COS (ROT)
            RS = SIN (ROT)
            XC = TC * RC - TS * RS
            XS = TC * RS + TS * RC
            MKPCAL(1,1,ICORR) = MKPCAL(1,1,ICORR) + XC
            MKPCAL(2,1,ICORR) = MKPCAL(2,1,ICORR) + XC * XC
            MKPCAL(3,1,ICORR) = MKPCAL(3,1,ICORR) + XS
            MKPCAL(4,1,ICORR) = MKPCAL(4,1,ICORR) + XS * XS
            MKPCAL(5,1,ICORR) = MKPCAL(5,1,ICORR) + 1.0
            END IF
C                                       Accumulate Phase cal. Rem. ant.
         IF ((NYPC.GT.5000) .AND.
     *      ((YPCCOS.LE.NYPC).AND.(YPCSIN.LE.NYPC))) THEN
            XNC = NYPC * 1.0
            TC = (2*YPCCOS - NYPC) / XNC
            TS = (2*YPCSIN - NYPC) / XNC
C                                       Remove rotation =
C                                       DELAYZ*PCFREQ/sampling rate
            ROT = 6.283185308 * (DELAYZ + MKXREF(6,INDX)) *
     *         DPFREQ / MKSMRT(MKBASE)
            RC = COS (ROT)
            RS = SIN (ROT)
            YC = TC * RC - TS * RS
            YS = TC * RS + TS * RC
            MKPCAL(1,2,ICORR) = MKPCAL(1,2,ICORR) + YC
            MKPCAL(2,2,ICORR) = MKPCAL(2,2,ICORR) + YC * YC
            MKPCAL(3,2,ICORR) = MKPCAL(3,2,ICORR) + YS
            MKPCAL(4,2,ICORR) = MKPCAL(4,2,ICORR) + YS * YS
            MKPCAL(5,2,ICORR) = MKPCAL(5,2,ICORR) + 1.0
            END IF
C                                       Debug?
 490     IF (MKDBUG.GE.2) THEN
            WRITE (9,2000) MM, SS, IA1, IA2, IBASE
            WRITE (9,2001) IBFULL, IBPART, FB, FBSCOR
            WRITE (9,2002) LAGNO, FREQNO, TIMNO, ADDR, DELAYZ
            WRITE (9,2003) (TMPBUF(I), I = 7,32)
            WRITE (9,2005) ICORR
            XPHS = 57.296 * ATAN2 (XS, XC+1.0E-20)
            YPHS = 57.296 * ATAN2 (YS, YC+1.0E-20)
            WRITE (9,2004) XPHS, YPHS
            END IF
 500     CONTINUE
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ATYP51: BASELINE NUMBER OUT OF RANGE ',I5)
 1003 FORMAT ('M3TAR: PARAMETER MXHUGE TOO SMALL')
 1010 FORMAT ('M3TAR: PARAMETER MXTIME TOO SMALL')
 1500 FORMAT ('BASELINE INDEX PROBLEM (', I3, '.NE.', I3,
     *        '): SKIPPING DATA')
 1600 FORMAT (' FOUND HIGH CORRELATION VALUE! SKIPPED')
 2000 FORMAT (/' Data record at ', I3, F5.1,1X,I2,'-',I2,'(',I3,')')
 2001 FORMAT ('IBFILL =',I6,' IBPART = ',I6,' FB =',F10.7,
     *   ' FBSCOR =',F10.7)
 2002 FORMAT (' LAGNO=',I4,' FREQNO =',I4,' TIMNO=',I4,' ADDR=',I8,
     *   ' DELAYZ=',I10)
 2003 FORMAT (' XCOR:',8I10/,6X,8I10/,
     *   1X,'no. cos bits',I12, 1X,'no. sin bits',I12/,
     *   1X,'X pc cos',I12, 1X,'X pc sin',I12, 1X,'X no. bits',I12,
     *   1X,'X PE',I12/,
     *   1X,'Y pc cos',I12, 1X,'Y pc sin',I12, 1X,'Y no. bits',I12,
     *   1X,'Y PE',I12)
 2004 FORMAT (' X phase cal =',F10.3, ' Y phase cal =',F10.3)
 2005 FORMAT (' CORRNO=',I4)
      END
      SUBROUTINE ATYP52 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 52 records from A tape.
C   These records are the output of FRNGE.
C   Input:
C      RECORD   I(*)   Record from tape in HP format
C   Output:
C      IRET     I      Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   TYPE, IT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Determine record type
      CALL ZI16IL (1, 1, RECORD, TYPE)
C                                       Only want some records
      IF ((TYPE.LT.4000) .OR. (TYPE.GT.4500)) GO TO 999
      IT = (TYPE /100) - 39
      GO TO (100,200,300,400,500,600), IT
C                                       Branch by record type
 100  CALL AT4000 (RECORD, IRET)
      GO TO 999
 200  CALL AT4100 (RECORD, IRET)
      GO TO 999
 300  CALL AT4200 (RECORD, IRET)
      GO TO 999
 400  CALL AT4300 (RECORD, IRET)
      GO TO 999
 500  CALL AT4400 (RECORD, IRET)
      GO TO 999
 600  CALL AT4500 (RECORD, IRET)
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE AT4000 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4000 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input/output via common:
C      HF table entries
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, IXY, UTCTAG(6), VLB2TA(6), ARCHIV(4),
     *   SAMPRA, FRPP, PASSNO, NOCHAN, NOOFAP(2,14), RECTRA(2,2,14),
     *   CORELV, UTCMTA(6), LUOFPR, REFTAP, REMTAP, SPCOPT, PRM250,
     *   IP, JP, NWD
      CHARACTER XY*2
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE), (TMPBUF(3),IXY),
     *   (TMPBUF(7),UTCTAG), (TMPBUF(13),VLB2TA), (TMPBUF(19),ARCHIV),
     *   (TMPBUF(23),SAMPRA), (TMPBUF(24),FRPP), (TMPBUF(25),PASSNO),
     *   (TMPBUF(26),NOCHAN), (TMPBUF(27),NOOFAP), (TMPBUF(55),RECTRA),
     *   (TMPBUF(111),CORELV), (TMPBUF(112),UTCMTA),
     *   (TMPBUF(118),LUOFPR), (TMPBUF(119),REFTAP),
     *   (TMPBUF(120),REMTAP), (TMPBUF(121),SPCOPT),
     *   (TMPBUF(122),PRM250)
C-----------------------------------------------------------------------
      NWD = NBITWD / 16
C                                       Convert whole record to integer
      NCONV = 128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.40) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Get baseline code
      IP = 2
      JP = (2 * MOD (IP, NWD)) + 1
      IP = (IP / NWD) + 1
      CALL ZC8CL (2, JP, RECORD(IP), XY)
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Save values in common
      M3GOT(IBASE) = .TRUE.
      CC1(IBASE) = XY
      CALL COPY (6, UTCTAG, IC2(1,IBASE))
      CALL COPY (6, VLB2TA, IC3(1,IBASE))
      CALL COPY (4, ARCHIV, IC4(1,IBASE))
      IC5(IBASE) = SAMPRA
      IC6(IBASE) = FRPP
      IC7(IBASE) = PASSNO
      IC8(IBASE) = NOCHAN
      CALL COPY (28, NOOFAP, IC9(1,IBASE))
      CALL COPY (56, RECTRA, IC10(1,IBASE))
C                                       Corel Version in bits 0:7
C                                       Floating point format bit 8 must
C                                       be 1
      IF ( (CORELV - ((CORELV/512)*512))/256 .NE. 1 ) THEN
         WRITE (MSGTXT,1002)
         IRET = 1
         GO TO 990
         END IF
      IC11(IBASE) = CORELV - ((CORELV/256)*256)
      CALL COPY (6, UTCMTA, IC12(1,IBASE))
      IC13(IBASE) = LUOFPR
      IC14(IBASE) = REFTAP
      IC15(IBASE) = REMTAP
      IC16(IBASE) = SPCOPT
      IC17(IBASE) = PRM250
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4000: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
 1002 FORMAT ('WRONG FLOATING POINT FORMAT ENCOUNTERED')
      END
      SUBROUTINE AT4100 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4100 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input/output via common
C      FRNGE table entries
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, CORELX(2,14), CALBYF(3,2,14)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE),
     *   (TMPBUF(7),CORELX), (TMPBUF(35),CALBYF)
C-----------------------------------------------------------------------
C                                       Convert whole record to integer
      NCONV = 128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.41) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Save values in common
      CALL COPY (28, CORELX, IC18(1,IBASE))
      CALL COPY (84, CALBYF, IC19(1,IBASE))
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4100: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
      END
      SUBROUTINE AT4200 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4200 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C   Input/output via common
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, PROCUT(2,14), ERRORA(2,2,14),
     *   INDEX(2,14), FRNGEC, SBDOFF
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE),
     *   (TMPBUF(7),PROCUT), (TMPBUF(35),ERRORA), (TMPBUF(91),INDEX),
     *   (TMPBUF(119),FRNGEC), (TMPBUF(120),SBDOFF)
C-----------------------------------------------------------------------
C                                       Convert whole record to integer
      NCONV = 128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.42) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Save values in common
      CALL COPY (28, PROCUT, IC20(1,IBASE))
      CALL COPY (56, ERRORA, IC21(1,IBASE))
      CALL COPY (28, INDEX, IC22(1,IBASE))
      IC23(IBASE) = FRNGEC
      IC24(IBASE) = SBDOFF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4200: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
      END
      SUBROUTINE AT4300 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4300 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C   Input/output via common
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, IP, JP, NWD
      CHARACTER STARID*8, BASELI(2)*8, CORELF*6, TAPEID(2)*8,
     *   VLB2PR*6, RUNCOD*8, FRNGQC*1, FRQGRP*2, OCOREL*6, TAPEQC*6,
     *   REFSOC*8, REMSOC*8, CHTMP*100
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE)
C-----------------------------------------------------------------------
      NWD = NBITWD / 16
C                                       Convert integers
      NCONV = 2
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.43) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Convert characters
      IP = 6
      JP = (2 * MOD (IP, NWD)) + 1
      IP = (IP / NWD) + 1
      CALL ZC8CL (100, JP, RECORD(IP), CHTMP)
C                                       Source name
      STARID = CHTMP(1:8)
C                                       Baseline name
      BASELI(1) = CHTMP(9:16)
      BASELI(2) = CHTMP(17:24)
C                                       COREL correlation output file
C                                       name.
      CORELF = CHTMP(25:30)
C                                       Raw-data tape ID labels for each
C                                       station.
      TAPEID(1) = CHTMP(31:38)
      TAPEID(2) = CHTMP(39:46)
C                                       FRNGE program version YYMMDD
      VLB2PR = CHTMP(47:52)
C                                       Run code, e.g. "329-1300"
      RUNCOD = CHTMP(53:60)
C                                       FRNGE quality code
      FRNGQC = CHTMP(62:62)
C                                       Frequency group code
      FRQGRP = CHTMP(63:64)
C                                       Original COREL file name
      OCOREL = CHTMP(65:70)
C                                       Tape Q code
      TAPEQC = CHTMP(71:76)
C                                       Ref station occupation code
      REFSOC = CHTMP(85:92)
C                                       Rem station occupation code
      REMSOC = CHTMP(93:100)
C                                       Save values in common
      CC25(IBASE) = STARID
      CC26(IBASE) = BASELI(1)
      CC27(IBASE) = BASELI(2)
      CC28(IBASE) = CORELF
      CC29(IBASE) = TAPEID(1)
      CC30(IBASE) = TAPEID(2)
      CC31(IBASE) = VLB2PR
      CC32(IBASE) = RUNCOD
      CC33(IBASE) = FRNGQC
      CC34(IBASE) = FRQGRP
      CC35(IBASE) = OCOREL
      CC36(IBASE) = TAPEQC
      CC37(IBASE) = REFSOC
      CC38(IBASE) = REMSOC
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4300: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
      END
      SUBROUTINE AT4400 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4400 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C   Input/output via common
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE, I
      DOUBLE PRECISION DPTMP(32), RFREQ(14), REFFRE, DELOBS, RATOBS,
     *   NBDELA, DGPD, BTE0, EPOCH0, DELOBM, RATOBM, DLY2, DLY3,
     *   DTEMP(32)
      REAL      RTEMP(64)
      EQUIVALENCE (DTEMP, RTEMP)
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE)
      EQUIVALENCE (DPTMP(2), RFREQ), (DPTMP(16),REFFRE),
     *   (DPTMP(17),DELOBS), (DPTMP(18),RATOBS), (DPTMP(19),NBDELA),
     *   (DPTMP(20),DGPD), (DPTMP(21),BTE0), (DPTMP(22),EPOCH0),
     *   (DPTMP(23),DELOBM), (DPTMP(24),RATOBM), (DPTMP(25),DLY2),
     *   (DPTMP(26),DLY3)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 2
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.44) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Convert double precision
      NCONV = 31
      CALL RCOPY (64, RECORD, RTEMP)
      CALL CU2DBL (NCONV, 2, DTEMP, DPTMP(2))
C                                       Save values in common
      DO 200 I = 1,14
         DC39(I,IBASE) = RFREQ(I)
 200     CONTINUE
      DC40(IBASE) = REFFRE
      DC41(IBASE) = DELOBS
      DC42(IBASE) = RATOBS
      DC43(IBASE) = NBDELA
      DC44(IBASE) = DGPD
      DC45(IBASE) = BTE0
      DC46(IBASE) = EPOCH0
      DC47(IBASE) = DELOBM
      DC48(IBASE) = RATOBM
      DC49(IBASE) = DLY2
      DC50(IBASE) = DLY3
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4400: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
      END
      SUBROUTINE AT4500 (RECORD, IRET)
C-----------------------------------------------------------------------
C   Decodes and interprets type 4500 records from A tape and gets scan
C   information.  These are FRNGE output records.
C   Input parameters:
C      RECORD   I(*)     Record from tape in HP format
C   Output:
C      IRET      I    Return error code, 0=>OK, else failed.
C   Input from common
C   Input/output via common
C-----------------------------------------------------------------------
      INTEGER   RECORD(*), IRET
C
      INTEGER   NCONV, ID, IBASE
      REAL      SPTMP(64), AMBYFR(2,14), PHASEC(2), DELRES,
     *   DELSIG, RATRES, RATSIG, COHERC, TOTPHA, UVFASE(2), STAREL(2),
     *   AAMP, URURSE(2), SRCHPA(6), DEPSBR, SNR, PROB, INCOH, EARP,
     *   REARP, START, STOP, EPD, DUR, DELSS, QB, DISCD, TOTPM
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'MKFRNG.INC'
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TMPBUF(1), ID), (TMPBUF(2),IBASE)
      EQUIVALENCE (SPTMP(2),AMBYFR), (SPTMP(30),PHASEC),
     *   (SPTMP(32),DELRES), (SPTMP(33),DELSIG), (SPTMP(34),RATRES),
     *   (SPTMP(35),RATSIG), (SPTMP(36),COHERC), (SPTMP(37),TOTPHA),
     *   (SPTMP(38),UVFASE), (SPTMP(40),STAREL), (SPTMP(42),AAMP),
     *   (SPTMP(43),URURSE), (SPTMP(45),SRCHPA), (SPTMP(51),DEPSBR),
     *   (SPTMP(52),SNR),    (SPTMP(53),PROB),   (SPTMP(54),INCOH),
     *   (SPTMP(55),EARP),   (SPTMP(56),REARP),  (SPTMP(57),START),
     *   (SPTMP(58),STOP),   (SPTMP(59),EPD),    (SPTMP(60),DUR),
     *   (SPTMP(61),DELSS),  (SPTMP(62),QB),     (SPTMP(63),DISCD),
     *   (SPTMP(64),TOTPM)
C-----------------------------------------------------------------------
C                                       Convert integers
      NCONV = 2
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                       Check ID
      IF ((ID/100).NE.45) THEN
         WRITE (MSGTXT,1000) ID
         IRET = 1
         GO TO 990
         END IF
C                                       Check dimensions
      IF (IBASE.GT.MAXM3X) THEN
         WRITE (MSGTXT,1001) IBASE, MAXM3X
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Convert single precision
      NCONV = 63
      CALL ZRHPRL (NCONV, 2, RECORD, SPTMP(2))
C                                       Save values in common
      CALL RCOPY (28, AMBYFR, RC51(1,IBASE))
      CALL RCOPY (2, PHASEC, RC52(1,IBASE))
      RC53(IBASE) = DELRES
      RC54(IBASE) = DELSIG
      RC55(IBASE) = RATRES
      RC56(IBASE) = RATSIG
      RC57(IBASE) = COHERC
      RC58(IBASE) = TOTPHA
      CALL RCOPY (2, UVFASE, RC59(1,IBASE))
      CALL RCOPY (2, STAREL, RC60(1,IBASE))
      RC61(IBASE) = AAMP
      CALL RCOPY (2, URURSE, RC62(1,IBASE))
      CALL RCOPY (6, SRCHPA, RC63(1,IBASE))
      RC64(IBASE) = DEPSBR
      RC65(IBASE) = SNR
      RC66(IBASE) = PROB
      RC67(IBASE) = INCOH
      RC68(IBASE) = EARP
      RC69(IBASE) = REARP
      RC70(IBASE) = START
      RC71(IBASE) = STOP
      RC72(IBASE) = EPD
      RC73(IBASE) = DUR
      RC74(IBASE) = DELSS
      RC75(IBASE) = QB
      RC76(IBASE) = DISCD
      RC77(IBASE) = TOTPM
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AT4500: WRONG RECORD ID =',I5)
 1001 FORMAT ('FRNGE INTERNAL TABLES TOO SMALL, ',I4,' > ',I4)
      END
      SUBROUTINE ATXFRM (NLAGS, ISB, DLYCOR, FBSC, DOAMP, MINL, MAXL,
     *   DATA, TWORK)
C-----------------------------------------------------------------------
C   Fourier transforms data from lags to spectra and makes FBS
C   correction.  If NLAGS is a power of two a fast, vectorized FFT is
C   used; if not a  somewhat slower routine is used.
C   Also takes conjugate of lower sideband data and makes phase
C   corrections.
C   Input:
C      NLAGS    I       Number of lags of data
C      ISB      I       Desired sideband from FFT (1=upper,-1=lower)
C      DLYCOR   R(4)    (1) = average fractional bit delay
C                       error over accumulation period (bits).
C                       (2) = number of delay steps in AP.
C                       (3-4) = times from beginning/end
C                       of AP to next/previous delay step (bits).
C      DOAMP    L       If true, do FBS amplitude correction
C      MINL     I       Min usable lag for the correlation function.
C      MAXL     I       Max usable lag for the correlation function.
C   Input/output:
C      FBSC     R(2,*)  FBS cos/sin array, (1,1) = FBS factor used;
C                       (1,2:) contains the sine/cosine array
C      DATA     R(2,*)  Data array
C      TWORK    R(2,*)  Work array.
C-----------------------------------------------------------------------
      INTEGER   NLAGS, ISB, MINL, MAXL, NSTEPS
      REAL      DLYCOR(4), FBSC(2,*), DATA(2,*), TWORK(2,*)
      LOGICAL   DOAMP
C
      LOGICAL   WOFFST
      INTEGER   K, I, GAMMA, NSPEC, ISIGN
      REAL      FBS, TAVG, T1, T2, TAP, W, RE, IM, SCALE, ARG1, ARG2,
     *          FACT1, FACT2, FACT3, BWMIDP
      LOGICAL   POWTWO
C-----------------------------------------------------------------------
C                                       Determine if NLAGS is power of
C                                       two.
      NSPEC = NLAGS / 2
      POWTWO = .FALSE.
      DO 10 GAMMA = 2,15
         IF ((2**GAMMA) .EQ. NLAGS) POWTWO = .TRUE.
 10      CONTINUE
C                                       Rearrange the correlation
C                                       function so that the zero
C                                       lag channel winds up
C                                       in the first element of the
C                                       array to be transformed.
      WOFFST = ABS(MINL) .GT. ABS(MAXL)
      DO 30 I = 1,NLAGS
         K = NSPEC + I - 1
         IF (WOFFST) K = K + 1
         IF (K .GT. NLAGS) K = K - NLAGS
C                                       Van Vleck correction.
         TWORK(1,I) = SIN (DATA(1,K))
         TWORK(2,I) = SIN (DATA(2,K))
30       CONTINUE
C
      DO 35 I = 1,NLAGS
         DATA(1,I) = TWORK(1,I)
         DATA(2,I) = TWORK(2,I)
35       CONTINUE
      ISIGN = 1
      IF(ISB.EQ.-1) ISIGN = -1
C                                       Phase ramp to shift frequency
C                                       sampling.
      CALL ATFSHT (DATA, NLAGS, ISIGN)
C                                       Transform, ISIGN determines
C                                       which sideband will end  up in
C                                       first half of data.
      IF (POWTWO) THEN
         CALL QUIKFT (DATA, NLAGS, -ISIGN, TWORK)
      ELSE
         CALL FOURG (DATA, NLAGS, ISIGN, TWORK)
         END IF
C                                       Fractional Bit Shift correction
      FBS = DLYCOR(1)
      TAVG = ABS (FBS)
      NSTEPS = DLYCOR(2)
C                                       Must be T1 > T2
      IF (DLYCOR(3) .GT. DLYCOR(4)) THEN
         T1 = DLYCOR(3)
         T2 = DLYCOR(4)
      ELSE
         T1 = DLYCOR(4)
         T2 = DLYCOR(3)
         END IF
C                                       Delay range in AP if NSTEP=0
      TAP = T1 + T2 - 1.0
C                                       Correct limits of integration
C                                       when there are delay steps
      T1 = T1 + TAVG
      T2 = T2 - TAVG
C
      IF (ABS (FBSC(1,1)-FBS) .GT. 1.0E-5) THEN
C                                       Need to recompute table
         FBSC(1,1) = FBS
         FACT1 = 6.283185308 / NLAGS
         FACT2 = -FACT1 * FBS
         IF (NSTEPS .GE. 1) THEN
            FACT3 = 1.035 / (NSTEPS - 1.0 + T1 + T2)
            END IF
         BWMIDP = NSPEC / 2.0 + 0.5
         DO 40 I = 1,NLAGS
            W = I - BWMIDP
            ARG2 = FACT2 * W
C                                       FBS amplitude correction
            IF (.NOT. DOAMP) THEN
               SCALE = 1.0
            ELSE IF (W .EQ. 0.0) THEN
               SCALE = 1.035
            ELSE IF (NSTEPS .GE. 1) THEN
               ARG1 = FACT1 * W
               SCALE = FACT3 / ARG1 *
     *              ( NSTEPS * SIN (ARG1 * (0.5 - TAVG))
     *              + NSTEPS * SIN (ARG1 * (0.5 + TAVG))
     *              - SIN (ARG1 * (0.5 - T1))
     *              - SIN (ARG1 * (0.5 - T2)) )
C                                       No delay steps in AP
            ELSE IF (TAP .NE. 0.0) THEN
               ARG1 = 0.5 * FACT1 * W * TAP
               SCALE = 1.035 * SIN (ARG1) / ARG1
            ELSE
               SCALE = 1.035
               END IF
C                                       Compute corrrection table
            FBSC(1,I+1) = COS (ARG2) / SCALE
            FBSC(2,I+1) = SIN (ARG2) / SCALE
 40         CONTINUE
         END IF
      IF (ISB.GT.0) THEN
C                                       FBS, by sideband
         DO 50 I = 1,NLAGS
C                                       Upper sideband
            RE = DATA(1,I) * FBSC(1,I+1) - DATA(2,I) * FBSC(2,I+1)
            IM = DATA(2,I) * FBSC(1,I+1) + DATA(1,I) * FBSC(2,I+1)
            DATA(1,I) = RE
            DATA(2,I) = IM
 50         CONTINUE
      ELSE
         DO 60 I = 1,NLAGS
C                                       Lower sideband
C                                       Correct for FBS and conjugate.
            K = I - NSPEC
            IF (K .LE. 0) K = K + NLAGS
            RE =   DATA(1,I) * FBSC(1,K+1) + DATA(2,I) * FBSC(2,K+1)
            IM =   DATA(2,I) * FBSC(1,K+1) - DATA(1,I) * FBSC(2,K+1)
            DATA(1,I) = RE
            DATA(2,I) = -IM
 60         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ATPCAL
C-----------------------------------------------------------------------
C   Process Phase Cal info.  Output values are blanked if the phase cal
C   amplitude is too low or the scatter too large.  If there is already
C   a valid value for a given antenna and frequency a new one is not
C   computed.
C   Input from common
C      FBLANK   R          REAL value indicating blanking.
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKPCAL   R(parm#,   Phase calibration values from current
C               stn#,corr#) scan-baseline. parm#: 1=sum cos;
C                          2=sum cos**2; 3=sum sin; 4=sum sin**2;
C                          5=n. stn# = 1(ref),2(rem).
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBPFQ   I(base#)   Phase cal id #pcid for baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C   Input/output via common
C      MKPCSC   R(parm#,    Scan-averaged phase cal values. parm#:
C               ant#,freq#) 1=Re(RCP);2=Im(RCP);3=Re(LCP);4=Im(LCP).
C      MKPFQT   D(pcid#,   Set of phase cal freq in same order as
C               freq#)     MKFRQ. Each set is identified by a
C                          phase cal id pcid#.
C      STKPFQ   I(fqid#,   Pointer for connecting freq to stokes for
C               freq#)     Phase-cal match of LSB <- USB
C-----------------------------------------------------------------------
      INTEGER   IA, IAA, NFREQ, IFREQ, PIX, IPCID, IFQID, ISTOKE, ICORR
      REAL      RE, IM, RE2, IM2, VARRE, VARIM, AMP2, AMP, DENOM, TOLER
      LOGICAL   BAD
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       Tolerance for uncertainty
C                                       10 %
      DATA TOLER /0.01/
C-----------------------------------------------------------------------
C                                       Loop over frequencies
      IFQID = MKBFQ(MKBASE)
      NFREQ = MKNFQT(IFQID)
      DO 200 ICORR = 1,MKNCOR
C                                       Valid correlator for this
C                                       baseline ?
         IF (MKCTYP(ICORR) .LT. 0) GO TO 200
C                                       Freq, Stokes
         IFREQ = MKCFRQ(ICORR)
         ISTOKE = MKCSTK(ICORR)
C                                       Loop over antenna:
         DO 150 IA = 1,2
            IAA = MKBL(IA,MKBASE)
C                                       Store pointer for IFREQ -> ISTOKE
            STKPFQ(IFQID,IFREQ) = ISTOKE
C                                       Which polarization ?
            PIX = 0
            IF (ISTOKE .EQ. 2) PIX = 2
            IF ((ISTOKE .EQ. 3) .AND. (IA .EQ. 2)) PIX = 2
            IF ((ISTOKE .EQ. 4) .AND. (IA .EQ. 1)) PIX = 2
C                                       Don't do if there is already a
C                                       good value or antenna ignored.
            IF ((IAA.LE.0) .OR. (MKPCSC(PIX+1,IAA,IFREQ).NE.FBLANK))
     *         GO TO 150
            MKPCSC(PIX+1,IAA,IFREQ) = 1.0
            MKPCSC(PIX+2,IAA,IFREQ) = 0.0
C                                       Phase cal present?
            IPCID = MKBPFQ(MKBASE)
            IF (ABS (MKPFQT(IPCID,IFREQ)) .LE. 1.0D0) GO TO 150
            BAD = .TRUE.
            IF (MKPCAL(5,IA,ICORR).GE.3) THEN
               DENOM = 1.0 / MKPCAL(5,IA,ICORR)
               RE = MKPCAL(1,IA,ICORR) * DENOM
               RE2 = MKPCAL(2,IA,ICORR) * DENOM
               IM = MKPCAL(3,IA,ICORR) * DENOM
               IM2 = MKPCAL(4,IA,ICORR) * DENOM
               VARRE = (RE2 - RE*RE) * DENOM
               VARIM = (IM2 - IM*IM) * DENOM
               AMP2 = RE*RE + IM*IM
C                                       Require amp>1.0e-8
               IF (AMP2.GT.1.0E-16) THEN
                  AMP = SQRT (AMP2)
                  MKPCSC(PIX+1,IAA,IFREQ) = RE / AMP
                  MKPCSC(PIX+2,IAA,IFREQ) = IM / AMP
                  VARRE = VARRE / AMP2
                  VARIM = VARIM / AMP2
C                                       Require RSS of real and
C                                       imaginary parts to be < 10% of
C                                       the amplitude
                  BAD =  (VARRE + VARIM) .GT. TOLER
                  END IF
               END IF
C                                       Flag bad values
            IF (BAD) THEN
               MKPCSC(PIX+1,IAA,IFREQ) = FBLANK
               MKPCSC(PIX+2,IAA,IFREQ) = FBLANK
               END IF
 150        CONTINUE
 200     CONTINUE
 999  RETURN
      END
      SUBROUTINE ATMODL (DLYERR)
C-----------------------------------------------------------------------
C   Compute station based model and compare with the value on tape.
C   Only computes values for the current baseline (MKBASE)
C   Output:
C      DLYERR  R      Difference between the model the Haystack
C                     correlator thought it was using and the computed
C                     model.  In sec. to be added to the baseline delay.
C                     Value for the scan reference time.
C   Input from common
C      MKCLIN   R          Input CL table increment (days).
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      ANTCLK   D(2,base#) Offset clock rate (ref,rem stn) (s/s)
C                          > 0 = fast.
C      ANTEPO   D(2,base#) Antenna clock ref epoch (s since BOY).
C      ANTSYN   D(2,base#) Clock sync (ref,rem stn) (us) >0 = fast.
C      MKLSCN   I          Nominal length of current scan (sec).
C      MKSUTC   D          Scan start time (hhmmss in days).
C      MKDLY    D(base#)   Total a priori delay at PRT (us).
C      MKPRT    D(base#)   Processing reference time (PRT) as used by
C                          MKIII correlator (ms since BOY). Should
C                          equal a priori epoch.
C      MKTIM0   D(base#)   Time since BOY (s) of a priori calc
C                          (Should be the same time as PRT).
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C   Input/output via common
C      MKATM    R(parm#,    Neutral atmosphere model. parm#: 1=delay
C               ant#,cl#)   (us); 2=rate (us/s); 3=acc (us/s/s).
C      MKGEO    D(parm#,   Antenna based geometric model by ant#
C               ant#,cl#)  and CL table index. Parm# 1,2,3 =
C                          (delay (us), rate (us/s), acc (us/s/s))
C                          respectively.
C      MKTMCL   D(cl#)     UTC times for CL table entries for current
C                          scan (days).
C      MKIREF   I          CL time pointer cl# for processor ref.
C                          time PRT.
C      MKNTMC   I          Number of CL table entry times per
C                          scan/antenna/IF.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      REAL      DLYERR
C
      INTEGER   NUM, LOOP, IREF, IA1, IA2, HH, MM, SS, IANT, IA
      DOUBLE PRECISION BTIME, ETIME, RTIME, TEMP, TIMEM, DLY1, DLY2,
     *   RAT1, RAT2, ACC1, ACC2, TA1, RAT, AC, DLY, BFP(35,2), TTT,
     *   CLK1, TMPINC
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCLT.INC'
      INCLUDE 'MKSTA.INC'
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Decide times for CL table
C                                       entries.
C                                       Only PRT entry?
      IF (MKCLIN.LT.0.0) THEN
         MKNTMC = 1
         MKIREF = MKNTMC
         MKTMCL(MKNTMC) = MKTIMC(2,MKBASE)
         MKTMCL(MKNTMC+1) = MKTMCL(MKNTMC) +  MKLSCN * 1.157407D-5
         END IF
C                                       Multiple CL table entries
      IF (MKNTMC.LE.0) THEN
         TMPINC = MKCLIN
         BTIME = MKSUTC
C                                       First at nominal start time
         MKNTMC = 0
         MKTMCL(1) = BTIME
C                                       Assume same times used on all
C                                       baselines.
         RTIME = MKTIMC(2,MKBASE)
C                                       Day change
         IF (RTIME.LT.BTIME) RTIME = RTIME + 1.0D0
         ETIME = BTIME + MKLSCN * 1.157407D-5
C                                       Don't do more than 500 entries
         IF (((ETIME-BTIME) / TMPINC) .GT. 498.0D0)
     *      TMPINC = (ETIME-BTIME) / 498.0D0
C                                       If RTIME not in range
C                                       (BTIME,ETIME) quit.
         IF ((RTIME.LT.BTIME) .OR. (RTIME.GT.ETIME)) GO TO 999
         MKNTMC = 1
C                                       Need more before MKIII ref time?
         IF ((RTIME-BTIME) .GT. (1.5*TMPINC)) THEN
            NUM = ((RTIME-BTIME) / (TMPINC*1.0001))
            DO 50 LOOP = 1,NUM
               MKNTMC = MKNTMC + 1
               MKTMCL(MKNTMC) = BTIME + LOOP * (RTIME - BTIME) /
     *            (NUM + 1.0D0)
 50            CONTINUE
            END IF
C                                       Reference time
         MKNTMC = MKNTMC + 1
         MKIREF = MKNTMC
         MKTMCL(MKNTMC) = RTIME
         IREF = MKNTMC
C                                       Need more before end time?
         IF ((ETIME-RTIME) .GT. (1.5*TMPINC)) THEN
            NUM = ((ETIME-RTIME) / (TMPINC*1.0001))
            DO 60 LOOP = 1,NUM
               MKNTMC = MKNTMC + 1
               MKTMCL(MKNTMC) = RTIME + LOOP * (ETIME - RTIME) /
     *            (NUM + 1.0D0)
 60            CONTINUE
            END IF
C                                       End time
         MKNTMC = MKNTMC + 1
         MKTMCL(MKNTMC) = ETIME
      ELSE
C                                       Not recomputing CL table times.
         IREF = MKIREF
         RTIME = MKTIMC(2,MKBASE)
         END IF
C                                       Set parameters
      CALL BFPSET (1, BFP(1,1))
      CALL BFPSET (2, BFP(1,2))
C                                       Recompute correlator model
      DO 300 LOOP = 1,MKNTMC
         TIMEM = MKTIM0(MKBASE) * 1000.0D0 + ((MKTMCL(LOOP) - RTIME ) *
     *      86400000.0D0)
         DO 250 IA = 1,2
            IANT = MKBL(IA,MKBASE)
C                                       Haystack correlator model.
            CALL XELAY (TIMEM, BFP(1,IA), DLY1, RAT1, ACC1, DLY2, RAT2,
     *         ACC2)
C                                       Convert to sec and sec/sec
            MKGEO(1,IANT,LOOP) = DLY1 * 1.0D-6
            MKGEO(2,IANT,LOOP) = RAT1 * 1.0D-6
            MKGEO(3,IANT,LOOP) = ACC1 * 1.0D-6
            MKATM(1,IANT,LOOP) = DLY2 * 1.0D-6
            MKATM(2,IANT,LOOP) = RAT2 * 1.0D-6
            MKATM(3,IANT,LOOP) = ACC2 * 1.0D-6
C                                       Correct clock drift
            MKGEO(1,IANT,LOOP) = MKGEO(1,IANT,LOOP) +
     *         MKGEO(1,IANT,LOOP) * ANTCLK(IA,MKBASE)
 250        CONTINUE
 300     CONTINUE
C                                       Compare with values on tape
      IA1 = MKBL(1,MKBASE)
      IA2 = MKBL(2,MKBASE)
C                                       Convert model delays to usec
      DLY1 = (MKGEO(1,IA1,IREF) + MKATM(1,IA1,IREF)) * 1.0D6
      DLY2 = (MKGEO(1,IA2,IREF) + MKATM(1,IA2,IREF)) * 1.0D6
C                                       Model rates in usec/sec
      RAT1 = (MKGEO(2,IA1,IREF) + MKATM(2,IA1,IREF)) * 1.0D6
      RAT2 = (MKGEO(2,IA2,IREF) + MKATM(2,IA2,IREF)) * 1.0D6
      RAT = RAT2 - RAT1
C                                       Model acc. in usec/sec/sec
      ACC1 = (MKGEO(3,IA1,IREF) + MKATM(3,IA1,IREF)) * 0.5D6
      ACC2 = (MKGEO(3,IA2,IREF) + MKATM(3,IA2,IREF)) * 0.5D6
      AC = ACC2 - ACC1
C                                       Time to compute model delay at
      TTT = ((MKTMCL(IREF) - MKTIMC(2,MKBASE)) * 86400.0D0) +
     *   ((MKPRT(MKBASE) * 1.0D-3) - ANTEPO(1,MKBASE))
      CLK1 = ANTSYN(1,MKBASE) + (ANTCLK(1,MKBASE) * TTT * 1.0D6)
C                                       Remove ref station clock offset.
      TA1 = (DLY1 - CLK1) * 1.0D-6
      TA1 = ((DLY1 - CLK1) + TA1 * (RAT1 + ACC1 * TA1)) * 1.0D-6
      DLY = (DLY2 - DLY1)  - TA1 * (RAT - AC * TA1)
C                                       Tell if delays differ by more
C                                       than 1 psec
      DLYERR = DLY - MKDLY(MKBASE)
      IF (ABS (DLYERR) .GT. 1.0D-6) THEN
C                                       Tell time, baseline and
C                                       difference
         TEMP = RTIME * 24.0D0
         HH = TEMP
         TEMP = (TEMP - HH) * 60.0D0
         MM = TEMP
         SS = (TEMP - MM) * 60.0D0 + 0.5D0
         WRITE (MSGTXT,1600) IA1, IA2, HH, MM, SS, DLYERR
         CALL MSGWRT (6)
         END IF
C                                       Convert to seconds
      DLYERR = DLYERR * 1.0D-6
C                                       Remove clock terms from MKGEO.
      DO 500 LOOP = 1,MKNTMC
         DO 450 IA = 1,2
            IANT = MKBL(IA,MKBASE)
            TTT = ((MKTMCL(LOOP) - MKTIMC(2,MKBASE)) * 86400.0D0)
     *         + ((MKPRT(MKBASE) * 1.0D-3) - ANTEPO(IA,MKBASE))
            CLK1 = ANTSYN(IA,MKBASE) * 1.0D-6 +
     *         (ANTCLK(IA,MKBASE) * TTT)
C                                        Delay
            MKGEO(1,IANT,LOOP) = MKGEO(1,IANT,LOOP) - CLK1
C                                       Rate
            MKGEO(2,IANT,LOOP) = MKGEO(2,IANT,LOOP) - ANTCLK(IA,MKBASE)
 450        CONTINUE
 500     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('DELAY ERROR ON BASELINE',I3,'-',I3,' AT ',3I3,' IS ',
     *   1PE12.5, ' USEC ')
      END
      SUBROUTINE BFPSET (IAA, BFP)
C-----------------------------------------------------------------------
C   Routine to fill in the BFP array needed by XELAY to compute
C   geometric delays used by the Haystack MKIII correlator.
C   This routine makes station based values with the reference station
C   at the earth center.
C   Inputs:
C      IAA    I      Antenna index (1=stn A, 2=stn B of baseline).
C   Output:
C      BFP  D(35)  Array of geometric parameters etc.
C                  1=SRATE, Sample rate (bits/sec)
C                  2=DAP, AP length (msec)
C                  3=DPP, PP length (msec)
C                  4=TIME0, UT time for which root a prioris are
C                    calculated (msec from BOY).
C                  5=UTOST, Sidereal rate (rad/UT msec)
C                  6=BQA, Invariant source/baseline parameter (usec).
C                  7=BQB, Invariant source/baseline parameter (usec).
C                  8=BQC, Invariant source/baseline parameter (usec).
C                  9=DHADT, Hour angle rate (rad/sec)
C                  10=BTR0, Ref. station a priori clock rate error
C                    (sec/sec).
C                  11=EPOCH0, Ref station a priori clock error at TIME0
C                    (msec).
C                  12=BTE0, A priori clock sync error difference at
C                    TIME0 (usec).
C                  13=BTR, A priori clock rate error difference
C                    (usec/sec).
C                  14=BTI, Instrumental delay (usec)
C                  15=TWOPI, 4.0*ATAN(1.0)
C                  16=TSTART, Nominal UT start time (msec from BOY).
C                  17=TIMEND, Nominal UT stop time for observation.
C                  18=ELQA1, Sin(LAT)*Sin(DEC) for ref. station.
C                  19=ELQA2, Sin(LAT)*Sin(DEC) for remote station.
C                  20=ELQB1, Cos(LAT)*Cos(Dec) for reference station.
C                  21=ELQB2, Cos(LAT)*Cos(Dec) for remote station.
C                  22=ZENAT1, Zenith atmospheric thickness at reference
C                    station (usec).
C                  23=ZENAT2, Zenith atmospheric thickness at remote
C                    station (usec).
C                  24=TMER1, UT time of meridian passage at ref station
C                    (msec since midnight)
C                  25=TMER2, UT time of meridian passage at remote
C                    station.
C                  26=TIME, UT time (msec from BOY) of last PPW update.
C                  27=BQADOT, Invariant source/baseline parameter
C                    (usec/sec).
C                  28=BQBDOT, Invariant source/baseline parameter
C                    (usec/sec).
C                  29=BQCDOT, Invariant source/baseline parameter
C                    (usec/sec).
C                  30=BQD, Invariant source/baseline parameter
C                    (dimensionless).
C                  31=BQE, Invariant source/baseline parameter
C                    (dimensionless).
C                  32=PHSITE, Pulsar ,phase at reference site
C                    (rotations)
C                  33=PHSDOT, Time derivative of PHSITE (rot/sec)
C                  34=Base time (msec from BOY) for PPW updates.
C                  35=PHSDD, Time derivitive of PHSDOT. (rot/sec**2)
C   Input from common
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKRA     D(base#)   Right ascension of date (rad).
C      ANIDLY   D(2,base#) Antenna instr. delay (ref,rem stn)
C                          (us) (unused).
C      ANTCLK   D(2,base#) Offset clock rate (ref,rem stn) (s/s)
C                          > 0 = fast.
C      ANTEPO   D(2,base#) Antenna clock ref epoch (s since BOY).
C      ANTLOC   D(stn#,3,  XYZ coordinates for stn#1=ref,2=rem
C               base#)     for baseline base# (left-handed).
C      ANTSYN   D(2,base#) Clock sync (ref,rem stn) (us) >0 = fast.
C      ANTZEN   D(2,base#) Zenith atmosphere elec. delay (ns).
C      MKDHAD   D(base#)   Hour angle rate (rad/s).
C      MKNFP    I(base#)   No frames/parameter period used by corr.
C      MKPMDC   D(base#)   Derivative of apparent DEC wrt UT (rad/s)
C      MKPRT    D(base#)   Processing reference time (PRT) as used by
C                          MKIII correlator (ms since BOY). Should
C                          equal a priori epoch.
C      MKSIDD   D(base#)   Derivative of GAST wrt UT (rad/s).
C      MKSIDT   D(base#)   GAST at a priori epoch (rad).
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C      MKSRAT   D(base#)   Sidereal rate (rad/UT ms).
C      MKUT1    D(base#)   UT1-UTC of date (s).
C      MKVLIG   D(base#)   Speed of light (m/s).
C      MKWOB    D(2,base#) X,Y pole position of date (arc-sec).
C   Input/output via common
C      MKDEC    D(base#)   Declination of date (rad).
C      MKTIM0   D(base#)   Time since BOY (s) of a priori calc
C                          (Should be the same time as PRT).
C-----------------------------------------------------------------------
      INTEGER   IAA
      DOUBLE PRECISION BFP(*)
C
      DOUBLE PRECISION DELRA, COSD, SIND, SINDEC, COSDEC, BQ1, BQ2,
     *   BX, BY, BZ, SX2, SY2, ALAT, ALONG, RHO, DTIM, STM00R
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'HAYSTACK'
C-----------------------------------------------------------------------
C                                       Set up BFP array.
      BFP(SRATE) = MKSMRT(MKBASE)
      BFP(DAP) = 1000.0 * (MKNFP(MKBASE) * 20000.0) / MKSMRT(MKBASE)
      BFP(DPP) = BFP(DAP)
      BFP(TIME0) = MKPRT(MKBASE)
      BFP(UTOST1) = MKSRAT(MKBASE)
      BFP(TWOPI) = 8.0D0 * ATAN (1.0D0)
      DELRA = MKSIDT(MKBASE) - MKRA(MKBASE) + MKUT1(MKBASE) *
     *   (BFP(TWOPI) / 86400.0D0)
      COSD = COS (DELRA)
      SIND = SIN (DELRA)
      SINDEC = SIN (MKDEC(MKBASE))
      COSDEC = COS (MKDEC(MKBASE))
      BX = ANTLOC(IAA,1,MKBASE) * 1.0D6 / MKVLIG(MKBASE)
      BY = ANTLOC(IAA,2,MKBASE) * 1.0D6 / MKVLIG(MKBASE)
      BZ = ANTLOC(IAA,3,MKBASE) * 1.0D6 / MKVLIG(MKBASE)
C                                       Include polar motion
      BX = BX - MKWOB(1,MKBASE) * BZ * 4.8481368D-6
      BY = BY - MKWOB(2,MKBASE) * BZ * 4.8481368D-6
      BZ = BZ + (MKWOB(1,MKBASE) * BX + MKWOB(2,MKBASE) * BY) *
     *   4.8481368D-6
      SX2 = ANTLOC(IAA,1,MKBASE) * 1.0D6 / MKVLIG(MKBASE)
      SY2 = ANTLOC(IAA,2,MKBASE) * 1.0D6 / MKVLIG(MKBASE)
      BQ1 = BX * COSD + BY * SIND
      BQ2 = -BX * SIND + BY * COSD
      BFP(BQA) = COSDEC * BQ1
      BFP(BQB) = COSDEC * BQ2
      BFP(BQC) = - BZ * SINDEC
      BFP(DHADT) = MKDHAD(MKBASE)
C                                       "Perfect" clocks at earth center
      BFP(BTR0) = 0.0D0
      BFP(EPOCH0) = 0.0D0
C                                       Remote - reference
      BFP(BTR) = ANTCLK(IAA,MKBASE) * 1.0D6
      DTIM = MKTIM0(MKBASE) - ANTEPO(IAA,MKBASE)
      BFP(BTE0) = ANTSYN(IAA,MKBASE) + DTIM * BFP(BTR)
      BFP(BTI) = ANIDLY(IAA,MKBASE)
C                                       Not used:
      BFP(TSTART) = 0.0D0
      BFP(TIMEND) = 0.0D0
      BFP(TIME) = 0.0D0
      BFP(TIME1) = 0.0D0
C                                       Values for atmospheric
C                                       corrections, earth center at
C                                       lat=0, long = 0
      RHO = SQRT (BX*BX + BY*BY)
C                                       Compute Lat. as in T2200
      ALAT = ATAN2 (BZ ,RHO)
      BFP(ELQA1) = 0.0D0
      BFP(ELQA2) = SIN (ALAT) * SINDEC
      BFP(ELQB1) = COSDEC
      BFP(ELQB2) = COS (ALAT) * COSDEC
      BFP(ZENAT1) = 0.0D0
      BFP(ZENAT2) = ANTZEN(IAA,MKBASE) / 1000.0D0
      STM00R = MKSIDT(MKBASE) - MKSIDD(MKBASE) *
     *   MOD(MKTIM0(MKBASE), 86400.0D0)
      BFP(TMER1) = (MKRA(MKBASE) - STM00R) / MKSRAT(MKBASE)
      ALONG = ATAN2 (BY, BX)
      BFP(TMER2) = (MKRA(MKBASE) - STM00R + ALONG) / MKSRAT(MKBASE)
C                                       Time varying source position
C                                       correction quantities.
      BFP(BQADOT) = -SINDEC * BQ1 * MKPMDC(MKBASE)
      BFP(BQBDOT) = -SINDEC * BQ2 * MKPMDC(MKBASE)
      BFP(BQCDOT) = - COSDEC * BZ * MKPMDC(MKBASE)
C                                       Invariant aberration quantities
      BFP(BQD) = MKSIDD(MKBASE) * COSDEC * (BY * COSD - BX * SIND) *
     *   1.0D-6
      BFP(BQE) = -MKSIDD(MKBASE) * COSDEC * (BY * SIND + BX * COSD) *
     *   1.0D-6
C***??? STUB PULSAR STUFF FOR NOW
      BFP(PHSITE) = 0.0D0
      BFP(PHSDOT) = 0.0D0
      BFP(PHSDD) = 0.0D0
 999  RETURN
      END
      SUBROUTINE XELAY (TIMEM, BFP, DLY, DRATE, DACCEL, ATMD, ATMR,
     *   ATMA)
C-----------------------------------------------------------------------
C   Haystack routine to compute the model delay, rate and acceleration
C   for the MKIII correlator.
C   Inputs:
C      TIMEM  D     "true" UTC time (msec) for which delay, rate and
C                   acceleration are to be calculated.
C      BFP    D(*)  Baseline parameter array containing all necessary
C                   parameters to do the computation.
C   Outputs:
C      DLY    D     Total delay (usec) at TIMEM
C      DRATE  D     Total delay rate (usec/sec) at TIMEM
C      DACCEL D     Total acceleration (usec/sec**2) at TIMEM
C      ATMD   D     Atmos. delay (usec) at TIMEM
C      ATMR   D     Atmos. delay rate (usec/sec) at TIMEM
C      ATMA   D     Atmos. acceleration (usec/sec**2) at TIMEM
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIMEM, BFP(*), DLY, DRATE, DACCEL, ATMD, ATMR,
     *   ATMA
C
      INCLUDE 'HAYSTACK'
      DOUBLE PRECISION DT, DRA, COSDRA, SINDRA, BQX, BQY, BQYDOT, DLYG,
     *   DRATEG, AQ, AQDOT, TIMM, A1, A2
C-----------------------------------------------------------------------
C                                       Compute geometric values
      DT = TIMEM - BFP(TIME0)
      DRA = DT * BFP(DHADT) / 1.0D3
      COSDRA = COS (DRA)
      SINDRA = SIN (DRA)
      BQX = BFP(BQA) * COSDRA + BFP(BQB) * SINDRA
      BQY = BFP(BQCDOT) - BFP(BQADOT) * COSDRA - BFP(BQBDOT) * SINDRA
      BQYDOT = (BFP(BQADOT) * SINDRA - BFP(BQBDOT) * COSDRA) *
     *   BFP(DHADT)
      DLYG = BFP(BQC) - BQX
      DLY = DLYG + BFP(BTE0) + (BFP(BTR) + BQY) * DT / 1000.0D0
      DRATEG = (BFP(BQA) * SINDRA - BFP(BQB) * COSDRA) * BFP(DHADT)
      DRATE = DRATEG + BFP(BTR) + BQYDOT * DT / 1.0D3 + BQY
      DACCEL = BFP(DHADT) * (BFP(DHADT) * BQX - BQY + BFP(BQCDOT)) +
     *   2.0D0 * BQYDOT
C                                       Correct for diurnal abberation
      AQ = -BFP(BQD) * COSDRA - BFP(BQE) * SINDRA
      AQDOT = (BFP(BQD) * SINDRA - BFP(BQE) * COSDRA) * BFP(DHADT)
      DLY = DLY + AQ * DLYG
      DRATE = DRATE + AQ * DRATEG + AQDOT * DLYG
      DACCEL = DACCEL + AQ * DACCEL + 2.0D0 * DRATEG * AQDOT -
     *   DLYG * AQ * BFP(DHADT) * BFP(DHADT)
C                                       Atmospheric values
C                                       TIMM must be relative to the
C                                       same midnight as STM00 and hence
C                                       TMER1&2
      TIMM = MOD (BFP(TIME0), 86400000.0D0) + DT
      A1 = (TIMM - BFP(TMER2)) / 1000.0D0
      A2 = BFP(UTOST1) * 1000.0D0
      CALL ATMOS (A1, A2, BFP(ELQA2), BFP(ELQB2), BFP(ZENAT2), ATMD,
     *   ATMR, ATMA)
 999  RETURN
      END
      SUBROUTINE ATMOS (TIME, UTOST, ELQA, ELQB, ZENATM, ATMDLY, ATMRAT,
     *   ATMACL)
C-----------------------------------------------------------------------
C   Haystack routine to compute atmospheric model used for the MKIII
C   correlator.  Model atmosphere from C. C. Chao, JPL, Oct. 1970,
C   TM391-129.
C   Inputs:
C      TIME    D   Time from meridian passage (sec)
C      UTOST   D   Sidereal rate (rad/UT sec)
C      ELQA    D   Sin (Lat) * Sin (Dec)
C      ELQB    D   Cos (Lat) * Cos (Dec)
C      ZENATM  D   Zenith atmospheric electrical thickness (usec).
C   Outputs:
C      ATMDLY  D   Atmospheric delay (usec)
C      ATMRAT  D   Atmospheric rate (usec/sec)
C      ATMACL  D   Atmospheric accelaration (usec/sec**2)
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME, UTOST, ELQA, ELQB, ZENATM, ATMDLY, ATMRAT,
     *   ATMACL
C
      DOUBLE PRECISION A, B, TIMX, COSHA, SINHA, SINEL, COSEL, EDOT1,
     *   EDOT2, P, Q, R, S, T, RDOT, SDOT, TDOT
      DATA A, B /0.00143D0, 0.0445D0/
C-----------------------------------------------------------------------
      TIMX = -UTOST * TIME
      COSHA = COS (TIMX)
      SINHA = SIN (TIMX)
      SINEL = ELQA + ELQB * COSHA
      COSEL = SQRT (1.0D0 - SINEL * SINEL)
      EDOT1 = UTOST * ELQB * SINHA / COSEL
      EDOT2 = (EDOT1 * EDOT1 * SINEL - UTOST * UTOST * ELQB * COSHA) /
     *   COSEL
      P = SINEL + B * COSEL
      Q = A * COSEL / P
      R = - A / (P * P)
      S = SINEL + Q
      T = COSEL + R
      RDOT = 2.0D0 * A * EDOT1 * (COSEL - B * SINEL) / (P * P * P)
      SDOT = EDOT1 * (COSEL + R)
      TDOT = RDOT - EDOT1 * SINEL
      ATMDLY = ZENATM / S
      ATMRAT = -ZENATM * EDOT1 * T / (S * S)
      ATMACL = -ZENATM * ( S * (EDOT2 * T + EDOT1 * TDOT) - 2.0D0 *
     *   EDOT1 * T * SDOT) / (S * S * S)
C                                       Take care of the case where ELEV
C                                       = +/- 90 deg, where EDOT1,2 are
C                                       undefined.
      IF (ABS (SINEL).GE.1.0D0) THEN
         ATMRAT = 0.0D0
         ATMACL = 0.0D0
         END IF
 999  RETURN
      END
      SUBROUTINE QUIKFT (X, M, ISIG, W)
C-----------------------------------------------------------------------
C   This program will produce a fast Fourier transform of a complex data
C   set using quarter-length tables by based on an algorithm devised by
C   William Newman, Cornell September 1, 1973.
C   Currently will handle up to M=4096
C      The file INCLUDEd should contain the compiler No-dependency
C   directive on vectorizing compilers.
C       On scalar systems the code can be simplified by
C   removing the branch to 400 and lines from label 400 to 490.
C    Inputs:
C     X(2,M)  R    Complex array to be transformed.
C     M       I    Length of X (must be a power of 2)
C     ISIG    I    Direction of the transform - same convention as FPS
C    Outputs:
C     X(2,M)  R    Resulting transformed array.
C     W(2,M)  R    Complex work array.
C   Modified for vector machines by W. D. Cotton, NRAO, Feb. 1986
C-----------------------------------------------------------------------
      INTEGER   M, ISIG, OLDM, N, I, J, L, M4, NM2, I1, K, MSTEP, ISEP,
     *   ITIMES, ISEPI1, JI, JI1, II, ISEP4
      INTEGER   IBITRV(4096)
      REAL      X(2,M), W(2,M), C(1024), S(1024), XT, ARG, AR, AI
      DOUBLE PRECISION DARG, TPI
      SAVE      OLDM, IBITRV, C, S
      DATA OLDM /0/
C-----------------------------------------------------------------------
      IF (M.EQ.OLDM) GO TO 200
C                                       Initialize tables
C                                       Find power of 2
      ARG = M
      N = (LOG (ARG) / LOG(2.0)) + 0.1
C                                       Fill sine & cosine tables
      M4 = M / 4
      NM2 = N - 2
      TPI = 8.0D0 * ATAN (1.0D0)
      TPI = TPI / M
      DO 10 I = 1,M4
         DARG = TPI * (I-1)
         C(I) = COS (DARG)
         S(I) = SIN (DARG)
 10      CONTINUE
C                                       Reverse bit order
      DO 50 I = 1,M4
         I1 = I - 1
         J = 0
         DO 20 K = 1,NM2
            L = I1 / 2
            J = I1 + 2 * (J-L)
            I1 = L
 20         CONTINUE
         J = J + 1
         IF (I.GE.J) GO TO 50
            XT = C(I)
            C(I) = C(J)
            C(J) = XT
            XT = S(I)
            S(I) = S(J)
            S(J) = XT
 50      CONTINUE
C                                       Bit reversal index array
      DO 100 I = 1,M
         I1 = I - 1
         J = 0
         DO 80 K = 1,N
            L = I1 / 2
            J = I1 + 2 * (J-L)
            I1 = L
 80         CONTINUE
         J = J + 1
         IBITRV(I) = J
 100     CONTINUE
C                                       Do transform
 200  OLDM = M
C                                       Copy to work vector (w/ conj.)
      XT = -ISIG
      DO 210 I = 1,M
         W(1,I) = X(1,I)
         W(2,I) = XT * X(2,I)
 210     CONTINUE
C                                       Butterfly loop
      DO 500 MSTEP = 1,N
         ISEP = 2 ** (N-MSTEP)
         ISEP4 = ISEP * 4
         ITIMES = 2 ** (MSTEP-1)
C                                       Move longer loop to inner
         IF (ITIMES.GT.ISEP) GO TO 400
         DO 390 I = 1,ITIMES
            ISEPI1 = 2 * (I-1) * ISEP
            II = (I+1) / 2
            JI1 = ISEPI1 + 1
            JI = JI1 + ISEP
            IF (I.EQ.(2*(I/2))) GO TO 260
C                                       Odd pass
      INCLUDE 'INCS:ZVND.INC'
               DO 250 J = 1,ISEP
                 AR = W(1,JI) * C(II) - W(2,JI) * S(II)
                 AI = W(1,JI) * S(II) + W(2,JI) * C(II)
                 W(1,JI) = W(1,JI1) - AR
                 W(2,JI) = W(2,JI1) - AI
                 W(1,JI1) = W(1,JI1) + AR
                 W(2,JI1) = W(2,JI1) + AI
                 JI1 = JI1 + 1
                 JI = JI + 1
 250             CONTINUE
              GO TO 390
C                                       Even pass
      INCLUDE 'INCS:ZVND.INC'
 260          DO 280 J = 1,ISEP
                 AR = W(1,JI) * S(II) + W(2,JI) * C(II)
                 AI = W(1,JI) * C(II) - W(2,JI) * S(II)
                 W(1,JI) = W(1,JI1) + AR
                 W(2,JI) = W(2,JI1) - AI
                 W(1,JI1) = W(1,JI1) - AR
                 W(2,JI1) = W(2,JI1) + AI
                 JI1 = JI1 + 1
                 JI = JI + 1
 280             CONTINUE
 390          CONTINUE
         GO TO 500
C                                       Switch order of loops
 400     DO 490 J = 1,ISEP
C                                       Odd passes
            II = 1
            JI1 = J
            JI = JI1 + ISEP
      INCLUDE 'INCS:ZVND.INC'
            DO 450 I = 1,ITIMES,2
               AR = W(1,JI) * C(II) - W(2,JI) * S(II)
               AI = W(1,JI) * S(II) + W(2,JI) * C(II)
               W(1,JI) = W(1,JI1) - AR
               W(2,JI) = W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) + AR
               W(2,JI1) = W(2,JI1) + AI
               II = II + 1
               JI1 = JI1 + ISEP4
               JI = JI + ISEP4
 450           CONTINUE
C                                       Even passes
            II = 1
            JI1 = J + 2 * ISEP
            JI = JI1 + ISEP
      INCLUDE 'INCS:ZVND.INC'
            DO 480 I = 2,ITIMES,2
               AR = W(1,JI) * S(II) + W(2,JI) * C(II)
               AI = W(1,JI) * C(II) - W(2,JI) * S(II)
               W(1,JI) = W(1,JI1) + AR
               W(2,JI) = W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) - AR
               W(2,JI1) = W(2,JI1) + AI
               II = II + 1
               JI1 = JI1 + ISEP4
               JI = JI + ISEP4
 480           CONTINUE
 490       CONTINUE
 500    CONTINUE
C                                       Bit reverse and conjugate back
C                                       to X
      XT = -ISIG
      DO 600 I = 1,M
         II = IBITRV(I)
         X(1,I) = W(1,II)
         X(2,I) = XT * W(2,II)
 600     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FOURG ( DATA, N, ISIGN, WORK)
C-----------------------------------------------------------------------
C     Cooley-Tukey fast fourier transform.
C     One-dimensional transform of complex data, arbitrary number of
C     points.  N points can be transformed in time proportional to
C     N*log(N) (for N non-prime), whereas other methods take N**2 time.
C     furthermore, because fewer arithmetic operations are performed,
C     less error is built up.
C     The transform done is--
C           Real*4 data(n),transform(n),work(n)
C           Complex data,transform,work
C       transform(k) = sum(data(j)*exp(isign*2*pi*i*(j-1)*(k-1)/n)),
C     summed from j = 1 to n for all k from 1 to N.  The transform
C     values are returned to data, replacing the input.  N may be any
C     positive number, but it should be non-prime for speed.
C     ISIGN = +1 or -1.
C     A -1 transform followed by a +1 one (or vice versa)
C     returns n times the original data.  work is a one-dimensional
C     complex array of length n used for working storage.
C     running time is proportional to n * (sum of the prime factors of
C     n).  for example, n = 1960, time is to * 1960 * (2+2+2+5+7+7).
C     naive methods directly implementing the summation run in time
C     proportional to n**2.  an upper bound for the rms relative error
C     is 3 * 2**(-b) * sum(f**1.5), where b is the number of bits in
C     the floating point fraction and the sum is over the prime
C     factors of n.
C     Written by Norman Brenner, Mit, August 1968.
C-----------------------------------------------------------------------
      REAL    DATA(*), WORK(*), TWOPI, THETA, SINTH, ROOTR,
     *   ROOTI, WSTPR, WSTPI, WMINR, WMINI, TEMPR, TEMPI,
     *   WR, WI, SUMR, SUMI, TEMP
      INTEGER   IFACT(32), N, ISIGN, IF, NPART, ID, IDIV, IQUOT,
     *   NFACT, IP0, IP3, IWORK, I3REV, I3, IP2, IP1,
     *   IFCUR, I1, J0, J1, IWMAX, I2MAX, I2
C-----------------------------------------------------------------------
      TWOPI = 6.2831853072 * REAL(ISIGN)
C                                          factor n into its prime
C                                          factors, nfact in number.
C                                          for example, for n = 1960,
C                                          nfact = 6 and ifact(if) = 2,
C                                          2, 2, 5, 7 and 7.
      IF = 0
      NPART = N
      DO 50 ID = 1, N, 2
      IDIV = ID
      IF (ID-1) 10,10,20
 10   IDIV = 2
 20   IQUOT = NPART / IDIV
      IF (NPART - IDIV * IQUOT) 40,30,40
 30   IF = IF + 1
      IFACT(IF) = IDIV
      NPART = IQUOT
      GO TO 20
 40   IF (IQUOT-IDIV) 60,60,50
 50   CONTINUE
 60   IF (NPART-1) 80,80,70
 70   IF = IF + 1
      IFACT(IF) = NPART
 80   NFACT = IF
C                                       shuffle the data array by
C                                       reversing the digits of the
C                                       index. Replace data(i) by
C                                       data(irev) for all i from 1
C                                       to n.  irev-1 is the integer
C                                       whose digit representation
C                                       in the multi-radix notation
C                                       of factors ifact(if) is the
C                                       reverse of the representatn
C                                       of i-1. For example, if all
C                                       ifact(if) = 2, then for i-1
C                                       = 11001, irev-1 = 10011. A
C                                       work array of length n is
C                                       needed.
      IP0 = 2
      IP3 = IP0 * N
      IWORK = 1
      I3REV = 1
      DO 110 I3 = 1, IP3, IP0
      WORK(IWORK) = DATA(I3REV)
      WORK(IWORK+1) = DATA(I3REV+1)
      IP2 = IP3
      DO 100 IF = 1, NFACT
      IP1 = IP2 / IFACT(IF)
      I3REV = I3REV + IP1
      IF (I3REV-IP2) 110,110,90
 90   I3REV = I3REV - IP2
 100  IP2 = IP1
 110  IWORK = IWORK + IP0
      IWORK = 1
      DO 120 I3 = 1, IP3, IP0
      DATA(I3) = WORK(IWORK)
      DATA(I3+1) = WORK(IWORK+1)
 120  IWORK = IWORK + IP0
C                                       phase-shifted fourier transform
C                                       of length ifact(if).
C                                       iprod=ip1/ip0
C                                       irem=n/(ifact(if)*iprod)
C                                       real data(iprod,ifact(if),irem),
C                                       work(ifact(if))
C                                       complex data,work
C                                       data(i1,j2,i3) =
C                                        sum(data(i1,i2,i3)
C                                        * w**(i2-1)), summed over
C                                       i2 = 1 to ifact(if) for all i1
C                                       from 1 to iprod, j2 from 1 to
C                                       ifact(if) and i3 from 1 to irem.
C                                       w = exp(isign*2*pi*i*
C                                        (i1-1+iprod*(j2-1))/
C                                        (iprod*ifact(if))).
      IF = 0
      IP1 = IP0
 130  IF (IP1-IP3) 140,240,240
 140  IF = IF + 1
      IFCUR = IFACT(IF)
      IP2 = IP1 * IFCUR
      THETA = TWOPI / REAL(IFCUR)
      SINTH = SIN(THETA/2.)
      ROOTR = -2. * SINTH * SINTH
      ROOTI = SIN(THETA)
      THETA = TWOPI / REAL(IP2/IP0)
      SINTH = SIN(THETA / 2.)
      WSTPR = -2. * SINTH * SINTH
      WSTPI = SIN(THETA)
      WMINR = 1.
      WMINI = 0.
      DO 230 I1 = 1, IP1, IP0
      IF (IFCUR-2) 150,150,170
 150  DO 160 I3 = I1, IP3, IP2
      J0 = I3
      J1 = I3 + IP1
      TEMPR = WMINR * DATA(J1) - WMINI * DATA(J1+1)
      TEMPI = WMINR * DATA(J1+1) + WMINI * DATA(J1)
      DATA(J1) = DATA(J0) - TEMPR
      DATA(J1+1) = DATA(J0+1) - TEMPI
      DATA(J0) = DATA(J0) + TEMPR
 160  DATA(J0+1) = DATA(J0+1) + TEMPI
      GO TO 220
 170  IWMAX = IP0 * IFCUR
      DO 210 I3 = I1, IP3, IP2
      I2MAX = I3 + IP2 - IP1
      WR = WMINR
      WI = WMINI
      DO 200 IWORK = 1, IWMAX, IP0
      I2 = I2MAX
      SUMR = DATA(I2)
      SUMI = DATA(I2+1)
 180  I2 = I2 - IP1
      TEMPR = SUMR
      SUMR = WR * SUMR - WI * SUMI + DATA(I2)
      SUMI = WR * SUMI + WI * TEMPR + DATA(I2+1)
      IF (I2-I3) 190, 190, 180
 190  WORK(IWORK) = SUMR
      WORK(IWORK+1) = SUMI
      TEMP = WR
      WR = WR * ROOTR - WI * ROOTI + WR
 200  WI = TEMP * ROOTI + WI * ROOTR + WI
      IWORK = 1
      DO 210 I2 = I3, I2MAX, IP1
      DATA(I2) = WORK(IWORK)
      DATA(I2+1) = WORK(IWORK+1)
 210  IWORK = IWORK + IP0
 220  TEMPR = WMINR
      WMINR = WMINR * WSTPR - WMINI * WSTPI + WMINR
 230  WMINI = TEMPR * WSTPI + WMINI * WSTPR + WMINI
      IP1 = IP2
      GO TO 130
 240  RETURN
      END
      SUBROUTINE ATHDR (RECORD, IHDR, ITYPE, IEXT, ITPVOL, ISUBF,
     *   LFILNM, LHDR, TLENGT, XTRA)
C---------------------------------------------------------------------
C    Decodes first 256 bytes of MK3 correlator A-tape TAR file headers.
C    Determines whether the header is an 'end of archive' record.
C    Anything else is treated as a file header record.
C    Extracts the file name, file type, file extent. Also returns a
C    description of the header suitable for direct output.
C    Gives the length of the current file within TAR.
C
C    Input parameters:
C       RECORD           I(*)    Header from tape (128 x 16-bit words).
C    Output parameters:
C       IHDR     I       Header type: 2=EOF record, 3=File hdr,
C                        -1 = deselected based on A-file
C       LHDR     C*80    Header description.
C       TLENGT   I       Length of file in Tape ARchive in blocks of
C                        256 (!) bytes. TAR is organized in blocks of
C                        512 bytes, but all the correllator data seems
C                        to be organized in blocks of 256 bytes and is
C                        read in this way.
C       XTRA     I       There is an empty block at and to fill up TAR file
C       Case IHDR of:
C
C       2:                 Additional information not neccessary
C
C       3: ITYPE    I      File type.
C          ITPVOL   I      SAVEM tape volume number.
C          IEXT     I      File extent number.
C          LFILNM   C*100  File name (UNIX convention)
C   Input from common
C      MKFILE   I          Current tape file no wrt tape start posn.
C   Input/output via common
C      NBITWD   I          # bits /word
C      TMPBUF   I(1024)    Work buffer.
C---------------------------------------------------------------------
      INTEGER RECORD(*), IHDR, ITYPE, IEXT, ITPVOL, NCONV,
     *        ISUBF, I, JHDR, K
      INTEGER TLENGT, XTRA
      CHARACTER OLENGT*12
      CHARACTER LFILNM*100
      CHARACTER LHDR*150
      CHARACTER DUMMY2*6
C      CHARACTER  LFILNM*6
C      CHARACTER  LHDR*80
C      CHARACTER  LINFO*60
      CHARACTER  LBLNK
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'AFILE.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LBLNK /' '/
C--------------------------------------------------------------------
C                                      Initialisation
      IHDR=0
      ITYPE=0
      IEXT=0
      ITPVOL=0
      ISUBF=0
      LFILNM=LBLNK
      LHDR=LBLNK
      TLENGT=0
      XTRA=0
C                                      Convert entire record to integer
C                                      Record length in TAR is 512 bytes,
C                                      but we still read blocks of 256 .
      NCONV=128
      CALL ZI16IL (NCONV, 1, RECORD, TMPBUF)
C                                      End of file record (in this case
C                                      end of archive) identified by
C                                      all words equal to 0.
      DO 200 K=1,128
         IF (TMPBUF(K).NE.0) GO TO 250
200   CONTINUE
      IHDR = 2
      WRITE (LHDR,1010) MKFILE
      GO TO 999
C                                      With UNIX TAR every header is a
C                                      file header
250   IHDR = 3
C                                      Find out length of file in TAR
C                                      First get string which describes
C                                      length of file in octal values.
      NCONV = 12
      CALL ZC8CL (NCONV, 125, RECORD, OLENGT)
C                                      Then convert string to no. of
C                                      blocks of 256 bytes.
      CALL CLENGT (OLENGT,TLENGT,XTRA)
C                                      File name
C                                      Unix filename on Tar is at least
C                                      100 chars long, maximum is 256.
C                                      We care only for the first 100
C                                      chars in the moment.
      NCONV = 100
      CALL ZC8CL (NCONV, 1, RECORD, LFILNM)
C                                      Here we have to find out from the
C                                      UNIX-filename what file type we
C                                      are dealing with! For stability
C                                      reasons we will keep the old
C                                      50-51-52 notation for the moment.
C                                      We also need the extent no. and
C                                      the root id code.
      CALL CUF2FT (LFILNM,ITYPE,IEXT,DUMMY2)
C                                      Catch the tape number (Info only)
      IF ((LFILNM(1:2).EQ.'#a').OR.(LFILNM(1:2).EQ.'#A')) THEN
         IF (LFILNM(3:6).NE.'0000') THEN
            ATAPEN = LFILNM(2:6)
            WRITE (MSGTXT,1040) ATAPEN
            CALL MSGWRT (6)
         END IF
      END IF
C                                      Is this root id and extent no.in
C                                      list of those to be accepted?
      IF (AFILEX .AND. (ITYPE.EQ.51)) THEN
         JHDR = -1
         DO 100 I = 1, ROOTNM
            IF ((DUMMY2.EQ.ROOTID(I)) .AND.
     *         (IEXT.EQ.EXTENT(I))) THEN
C                                             The tape number is
C                                             obsolete under UNIX, so we
C                                             do it the easy way.
               JHDR = IHDR
               GO TO 150
            END IF
100      CONTINUE
C                                      Build the header description
150      IF (JHDR.EQ.-1) IHDR = JHDR
            END IF
         IF (IHDR.EQ.-1) THEN
            WRITE (LHDR,1030) MKFILE, ITYPE, IEXT, LFILNM
         ELSE
            WRITE (LHDR,1020) MKFILE, ITYPE, IEXT, LFILNM
         END IF
C
999   RETURN
C---------------------------------------------------------------------
 1010 FORMAT (I4, 3X, 'END OF FILE RECORD')
 1020 FORMAT (I4,3X,'TYPE:',I3,' EXT:',I3,' FILE: ',A40)
 1030 FORMAT ('SKIP: ',I4,3X,'TYPE:',I3,' EXT:',I3,
     *   ' FILE: ',A40)
 1040 FORMAT ('Reading data from tape ',A5)
      END
      SUBROUTINE INDXEQ (ITEMNO, NBITEM, NBWORD, IP, IPS, NBUNIT)
C---------------------------------------------------------------------
C! Compute indices for equivalenced arrays
C# Utility
C    Routine to compute indices for equivalenced arrays. Consider
C    an input buffer of words of length NBWORD bits, equivalenced
C    to an array of data items each NBITEM bits long. The address
C    of item ITEMNO in the input buffer is computed as (IP, IPS),
C    where IP is the 1-relative word number in the input buffer,
C    and IPS is the sub-index relative to the start of the word
C    in units of NBUNIT bits (also 1-relative). These parameters
C    can then be used directly in calls to ZC8CL, ZI16IL, ZRHPRL etc.
C    For ZC8CL or ZI8IL NBUNIT=8, ZI16IL NBUNIT=16 etc.
C    Routine will abort if IPS in non-integral.
C    Input parameters:
C      ITEMNO       I       Item number in data array for which
C                           address in input buffer is required.
C      NBITEM       I       Length of each item (bits).
C      NBWORD       I       Length of each input buffer word (bits).
C      NBUNIT       I       Units of IPS (bits).
C    Output parameters:
C      IP           I       1-relative array index in input buffer.
C      IPS          I       1-relative sub-index wrt word IP in
C                           units of NBITEM bits.
C   Input/output via common
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      INTEGER ITEMNO, NBITEM, NBUNIT, NBWORD, IP, IPS
      INTEGER ITMBIT, IPSBIT, IPMOD
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
      ITMBIT = (ITEMNO - 1) * NBITEM
      IP = ITMBIT / NBWORD + 1
      IPSBIT = MOD (ITMBIT, NBWORD)
C                                      Check for non-integral sub-indx
      IPMOD = MOD (IPSBIT, NBUNIT)
      IF (IPMOD .EQ. 0) THEN
         IPS = IPSBIT / NBUNIT + 1
      ELSE
         WRITE (MSGTXT, 1000) NBWORD, NBITEM
         CALL MSGWRT (9)
         STOP
         END IF
C
      RETURN
C---------------------------------------------------------------------
1000  FORMAT ('INDXEQ REQUIRES DEVELOPMENT: NBWORD=',I4,' NBITEM=',
     *        I4,' NBUNIT=',I4)
      END
      SUBROUTINE IDCHK (NID, IDX, IBASL, IRET)
C---------------------------------------------------------------------
C    Routine to check that the ID recs within the type 50 root extent
C    are unique and in (baseline, ID) order. Requires that the first
C    record within the extent have ID=1000. Details of the last ID rec
C    are stored in /IDSEQ/. Routine also updates a flag table
C    indicating which ID records have been read within the current
C    scan-baseline (KFNDID in /IDSEQ/).
C    Input parameters:
C       NID       I       Absolute ID record no within the current
C                         type 50 root extent.
C       IDX       I       Record ID (eg 1000,2100 etc).
C       IBASL     I       Baseline no of current ID record.
C    Output parameters:
C       IRET      I       Return code (non-zero = error)
C   Input from common
C   Input/output via common
C      KFNDID   L(9)       Flag table for ID record types. For
C                          ID=nnxx, KFNDID index is (nn-19).
C      IBLST    I          Baseline no of last ID record.
C      IDXLST   I          Last record ID.
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      LOGICAL KERR
      INTEGER NID, IDX, IBASL, IRET, IDNLST, IDXMOD, IDN, J, IDIFF
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      IF (NID .EQ. 1) THEN
         IDXLST = 0
         IBLST = 0
         END IF
      IDN = IDX / 100
      IDNLST = IDXLST / 100
      IDXMOD = MOD (IDX, 100)
      KERR = .FALSE.
      IDIFF = IDX - IDXLST
C                                       Case IDX of:
      IF (IDX .NE. 1000) GO TO 100
C                                       IDX = 1000
         KERR = NID .NE. 1
         GO TO 500
C                                       IDX = 2000
100   IF (IDX .NE. 2000) GO TO 200
         IF (IDNLST .NE. 10) KERR = (IBASL .LE. IBLST) .OR. (KERR)
C                                       Initialise ID flag table
         DO 110 J=1,9
            KFNDID(J) = .FALSE.
110         CONTINUE
         GO TO 500
C                                       IDX = other
200   IF (IDN .EQ. IDNLST) THEN
         KERR = IDIFF .NE. 1
      ELSE
         KERR = (IDX .LE. IDXLST) .OR. (IDXMOD .NE. 0)
         END IF
      IF (IDN .NE. 10) KERR = (IBASL .NE. IBLST) .OR. (KERR)
C                                       Endcase: Update /IDSEQ/.
500   IDXLST = IDX
      IBLST = IBASL
C                                       Update ID flag table.
      IF (IDN .NE. 10) THEN
         J = IDN - 19
         IF ((J .GT. 0) .AND. (J .LE. 9)) THEN
            KFNDID(J) = .TRUE.
         ELSE
            KERR = .TRUE.
            END IF
         END IF
C                                       Report error.
      IF (KERR) THEN
         MSGTXT = 'ID RECORDS OUT OF SEQUENCE IN ROOT EXTENT: SKIP'
         CALL MSGWRT (6)
         IRET = -1
         END IF
C
999   RETURN
      END
      SUBROUTINE TBINIT
C--------------------------------------------------------------------
C   Initialise the MK3TAB.INC common blocks. These tables
C   contain the run-, source- and antenna-based parameters
C   used by M3TAR throughout the run. The AIPS IF tables
C   are also stored here. All parameters read from tape for
C   each scan are checked against these tables for consistency,
C   and discrepancies are reported. These checks are neccessary
C   as the MK3 correlator data format is baseline based.
C   Input from common
C      DBLANK   D          Magic value (= indeterminate).
C   Input/output via common
C      DTACLK   D(ant#)    Station clock rate (us/s) >0 = fast.
C      DTAEPO   D(ant#)    Station clock epoch (s since BOY).
C      DTALOC   D(3,ant#)  X,Y,Z coordinates for antenna ant# using
C                          left-handed axes.
C      DTASYN   D(ant#)    Station clock offset (us) >0 = fast.
C      ITNAFQ   I(ifid#)     No of frequencies in each set.
C      NTAFQT   I            No of entries in ITNAFQ.
C      DTINT    D          Correlator integration time for run (days)
C      DTSMRT   D          Sample rate for run (bits/s).
C      DTVLIG   D          Speed of light value for run (m/s).
C      NTJULD   I          No of entries in DTJULD.
C      LTCVER   C*8        COREL revision level.
C      DTEPOC   D          Last source coordinate epoch (eg 1950.0)
C      DTTIM1   D          Last scan mid time (Jul day no).
C-------------------------------------------------------------------
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER J, K
C--------------------------------------------------------------------
C                                       Run-based parameters (/RUNTB/).
      DTVLIG = 0.0D0
      DTSMRT = 0.0D0
      DTINT = 0.0D0
      NTJULD = 0
      LTCVER = 'INDE'
C                                       Source parameters (/SRCTB/).
      DTEPOC = DBLANK
      DTTIM1 = DBLANK
C                                       Antenna parameters (/ANTTB/).
      DO 150 J = 1,MXANT
         DO 100 K= 1,3
            DTALOC(K,J) = DBLANK
100      CONTINUE
         DTAEPO(J) = DBLANK
         DTASYN(J) = DBLANK
         DTACLK(J) = DBLANK
150   CONTINUE
C                                       AIPS IF tables.
      NTAFQT = 0
      DO 250 J = 1,MXFQID
         ITNAFQ(J) = 0
250   CONTINUE
C                                       Exit.
      RETURN
      END
      SUBROUTINE PROCFQ (IRET)
C---------------------------------------------------------------------
C   Process the freq data read from tape for the current scan-baseline
C   header.  The data are checked for consistency and the scan-baseline
C   is skipped if it is incomplete or has been de-selected.
C
C   Output:
C      IRET     I      Return code (0=OK, <> 0 = error)
C   Input from common
C      ANTNAM   C*8(ant#)  Antenna name.
C      KFNDID   L(9)       Flag table for ID record types. For
C                          ID=nnxx, KFNDID index is (nn-19).
C      MKLOST   L          If true then LO table exists (2700 rec).
C      MKBASL   I          Baseline no of current ID record.
C      MKSUTC   D          Scan start time (hhmmss in days).
C      MKFFQ    D(*)       List of RF frequencies found
C      MKNFFQ   I          Number of entries in MKFFQ
C   Input/output via common
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKFRQ    D(freq#)   RF frequency at baseband in video for each
C                          of up to 28 frequencies for the current
C                          scan-baseline data (Hz) (>0 USB; < 0 LSB).
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKNFRQ   I(base#)   No of freq in MKFRQ per baseline.
C      MKNPFQ   I(pcid#)   No of frequencies in each set.
C      MKPFQT   D(pcid#,   Set of phase cal freq in same order as
C               freq#)     MKFRQ. Each set is identified by a
C                          phase cal id pcid#.
C      MKPFRQ   D(freq#)   Phase calibration tone frequencies for the
C                          current scan baseline data (Hz). In same
C                          order as MKFRQ.
C      MKNFID   I          No of entries in MKNFQT.
C      MKNPID   I          No of entries in MKNPFQ.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBPFQ   I(base#)   Phase cal id #pcid for baseline base#.
C      MKSJD    D          Scan start time (Jul day no).
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      INTEGER IRET
C
      LOGICAL KOK
      CHARACTER  LTIME*20, LHM
      DOUBLE PRECISION DTDEG
      REAL SEC
      INTEGER J, HM(2), ISEC, IROUND, I1, I2, IDA
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MKOTH.INC'
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
C                                       Skip de-selected baselines.
      IF ((MKBL(1,MKBASL).LE.0) .OR. (MKBL(2,MKBASL).LE.0)) GO TO 999
C                                       Convert scan start time to
C                                       format 'IAT ddd/hh mm ss'.
      IDA = ATIME1
      DTDEG = (ATIME1 - IDA) * 360.0D0
      CALL COORDD (1, DTDEG, LHM, HM, SEC)
      ISEC = IROUND (SEC)
      WRITE (LTIME,1000) IDA, HM, ISEC
C                                       Is scan-baseline hdr complete ?
      KOK = .TRUE.
      DO 50 J = 1,7
         KOK = KOK .AND. KFNDID(J)
50       CONTINUE
      IF (MKLOST) KOK = KOK .AND. KFNDID(8)
      KOK = KOK .AND. (MKNFRQ(MKBASL) .GT. 0)
C                                       De-select if incomplete.
      IF (.NOT. KOK) THEN
         I1 = ABS (MKBL(1,MKBASL))
         I2 = ABS (MKBL(2,MKBASL))
         MKBL(1,MKBASL) = -I1
         MKBL(2,MKBASL) = -I2
         WRITE (MSGTXT,1010) ANTNAM(I1), ANTNAM(I2), LTIME
         CALL MSGWRT (6)
         END IF
C                                       Add any frequencies found but
C                                       uncorrelated to the MKFRQ list
      CALL UPFLIS (MKNFFQ, MKFFQ, MXM3FQ, MKNFRQ(MKBASL), MKFRQ)
C                                       Process the scan-baseline data.
      IF ((MKBL(1,MKBASL) .GT. 0) .AND. (MKBL(2,MKBASL) .GT. 0)
     *   .AND. (MKNFRQ(MKBASL) .GT. 0)) THEN
C                                       RF freq. tables.
         CALL FQSRCH (MKFRQ, MKNFRQ(MKBASL), MKFQT, MKNFQT, MKNFID,
     *      MKBFQ(MKBASL), IRET)
C                                       Phase-cal tables.
         CALL FQSRCH (MKPFRQ, MKNFRQ(MKBASL), MKPFQT, MKNPFQ,
     *      MKNPID, MKBPFQ(MKBASL), IRET)
         END IF
C                                       Exit
999   RETURN
C---------------------------------------------------------------------
1000  FORMAT ('IAT',I4,'/',3I3)
1010  FORMAT ('Baseline ',A8,'-',A8,' incomplete hdr on ',A20)
      END
      SUBROUTINE PROCSB (IRET)
C---------------------------------------------------------------------
C   Process the source, antenna and run parameters for the
C   data read from tape for the current scan-baseline.
C   Output:
C      IRET     I      Return code (0=OK, <> 0 = error)
C   Input from common
C      ANTNAM   C*8(ant#)  Antenna name.
C      MKSNAM   C*8(base#) Source name.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      ATIME1   R          Start time of current scan (IAT
C                          days wrt ref Jul day no).
C      ATIME2   R          Nominal stop time of current scan
C                          (IAT days wrt ref Jul day no).
C      MKPRT    D(base#)   Processing reference time (PRT) as used by
C                          MKIII correlator (ms since BOY). Should
C                          equal a priori epoch.
C      MKTIM0   D(base#)   Time since BOY (s) of a priori calc
C                          (Should be the same time as PRT).
C   Input/output via common
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER LTIME*20, LHM
      DOUBLE PRECISION DEGTIM, DELTAT
      REAL SEC
      INTEGER HMA(2), HMB(2), ISECA, ISECB, IROUND, IDA, IDB, IA1, IA2
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
C                                       Convert scan start, stop times
C                                       to format 'IAT ddd/hh mm ss'.
      IDA = ATIME1
      DEGTIM = (ATIME1 - IDA) * 360.0D0
      CALL COORDD (1, DEGTIM, LHM, HMA, SEC)
      ISECA = IROUND (SEC)
      WRITE (LTIME,1000) IDA, HMA, ISECA
      IDB = ATIME2
      DEGTIM = (ATIME2 - IDB) * 360.0D0
      CALL COORDD (1, DEGTIM, LHM, HMB, SEC)
      ISECB = IROUND (SEC)
C                                       Process the scan-baseline data.
      IA1 = MKBL(1,MKBASE)
      IA2 = MKBL(2,MKBASE)
      IF ((IA1 .GT. 0) .AND. (IA2 .GT. 0)) THEN
C                                       Print the scan-baseline header.
         WRITE (MSGTXT,1010) MKSNAM(MKBASE), ANTNAM(IA1), ANTNAM(IA2),
     *      IDA, HMA, ISECA, IDB, HMB, ISECB
         CALL MSGWRT (6)
C                                       Print a warning if the proc
C                                           ref time does not agree with
C                                       the a priori time.
         DELTAT = MKPRT(MKBASE) * 1.0D-3 - MKTIM0(MKBASE)
         IF (DELTAT .NE. 0) THEN
            WRITE (MSGTXT,1020) DELTAT
            CALL MSGWRT (6)
            END IF
C                                       Print a data quality summary.
         CALL DATQ
C                                       Run-based parameters.
         CALL RUNSB (LTIME, IRET)
C                                       Source parameters.
         IF (IRET .EQ. 0) CALL MK3SOU (LTIME, IRET)
C                                       Antenna parameters.
         IF (IRET .EQ. 0) CALL ANTSB (LTIME, IRET)
         END IF
C                                       Exit
      RETURN
C---------------------------------------------------------------------
1000  FORMAT ('IAT',I4,'/',3I3)
1010  FORMAT (A8,': ',A8,' - ',A8,' at ',I4,'/',I2,2I3,' - ',
     *   I3,'/',I2,2I3)
1020  FORMAT ('WARNING: PROC REF TIME - A PRIORI TIME = ',D10.4,' S')
      END
      SUBROUTINE RUNSB (LTIME, IRET)
C---------------------------------------------------------------------
C   Check the run-based parameters for consistency with previous
C   values.
C   Input parameters:
C     LTIME   C*20    Scan start time as 'IAT ddd/hh mm ss'.
C   Output parameters:
C     IRET    I       Return code (0=OK, <> 0 = error)
C   Input from common
C      MKCVER   C*8        COREL revision level.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKSIDT   D(base#)   GAST at a priori epoch (rad).
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C   Input/output via common
C      DBLANK   D          Magic value (= indeterminate).
C      MKINT    D(base#)   Correlator integration time (days).
C      MKSIDD   D(base#)   Derivative of GAST wrt UT (rad/s).
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C      MKUT1    D(base#)   UT1-UTC of date (s).
C      MKVLIG   D(base#)   Speed of light (m/s).
C      MKWOB    D(2,base#) X,Y pole position of date (arc-sec).
C      MSGTXT   C*80       AIPS message string.
C      DTGAST   D(day#)    GAST at 0 UT on day# (rad).
C      DTJULD   D(day#)    Julian day no for run based parameters.
C      DTSIDD   D(day#)    d(GAST)/d(UT) on day# (rad/s).
C      DTUT1    D(day#)    UT1-UTC (s) on day#.
C      DTWOB    D(2,day#)  X(=1),Y(=2) pole posn on day# (arcsec).
C      DTINT    D          Correlator integration time for run (days)
C      DTSMRT   D          Sample rate for run (bits/s).
C      DTVLIG   D          Speed of light value for run (m/s).
C      NTJULD   I          No of entries in DTJULD.
C      LTCVER   C*8        COREL revision level.
C---------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER LTIME*20, LDESC*20
      DOUBLE PRECISION DGST0, DPI, DE, D0, D2PI
      INTEGER J
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C--------------------------------------------------------------------
C                                       Initialisation.
      IRET = 0
      DPI = 4.0D0 * ATAN (1.0D0)
      D2PI = 2.0D0 * DPI
C                                       Calculate GAST at 0 UT.
      DGST0 = MKSIDT(MKBASE) - MKTIMC(2,MKBASE) * MKSIDD(MKBASE) *
     *   86400.0D0
      DGST0 = MOD (DGST0, D2PI)
      IF (DGST0 .LT. 0) DGST0 = DGST0 + D2PI
C                                       Compare current and prev values.
      DE = 1.0D-8
      D0 = 0.0D0
      LDESC = 'SPEED OF LIGHT (m/s)'
      CALL D3CHK (DTVLIG, MKVLIG(MKBASE), DE, D0, LTIME, LDESC)
      LDESC = 'SAMPLE RATE (bits/s)'
      CALL D3CHK (DTSMRT, MKSMRT(MKBASE), DE, D0, LTIME, LDESC)
      LDESC = 'INTEGRATION TIME (s)'
      CALL D3CHK (DTINT, MKINT(MKBASE), DE, D0, LTIME, LDESC)
      IF ((LTCVER .NE. 'INDE') .AND. (MKCVER .NE. LTCVER)) THEN
         WRITE (MSGTXT,1020) LTIME
         CALL MSGWRT (6)
         WRITE (MSGTXT,1030) LTCVER, MKCVER
         CALL MSGWRT (6)
        END IF
      LTCVER = MKCVER
C                                       Find place in ref day table.
      J = 1
100   IF (J .GT. NTJULD) GO TO 150
         IF (MKTIMC(1,MKBASE) .EQ. DTJULD(J)) GO TO 150
         J = J + 1
         GO TO 100
C                                       Compare ref day values.
150   IF (J .LE. NTJULD) THEN
         LDESC = 'UT1-UTC (s)'
         CALL D3CHK (DTUT1(J), MKUT1(MKBASE), DE, DBLANK, LTIME, LDESC)
         LDESC = 'GAST at 0 UT (rad)'
         CALL D3CHK (DTGAST(J), DGST0, DE, DBLANK, LTIME, LDESC)
         LDESC = 'dGAST/dUT (rad/s)'
         CALL D3CHK (DTSIDD(J), MKSIDD(MKBASE), DE, DBLANK,LTIME,LDESC)
         LDESC = 'X pol posn (arcsec)'
         CALL D3CHK (DTWOB(1,J), MKWOB(1,MKBASE), DE, DBLANK,
     *      LTIME, LDESC)
         LDESC = 'Y pol posn (arcsec)'
         CALL D3CHK (DTWOB(2,J), MKWOB(2,MKBASE), DE, DBLANK,
     *      LTIME, LDESC)
      ELSE
         IF (NTJULD .EQ. MXRDAY) THEN
            MSGTXT = 'M3TAR: PARAMETER MXRDAY NEEDS TO BE INCREASED'
            CALL MSGWRT (7)
            IRET = 1
         ELSE
            NTJULD = NTJULD + 1
            DTJULD(NTJULD) = MKTIMC(1,MKBASE)
            DTUT1(NTJULD) = MKUT1(MKBASE)
            DTGAST(NTJULD) = DGST0
            DTSIDD(NTJULD) = MKSIDD(MKBASE)
            DTWOB(1,NTJULD) = MKWOB(1,MKBASE)
            DTWOB(2,NTJULD) = MKWOB(2,MKBASE)
            END IF
         END IF
C                                       Exit
      RETURN
C---------------------------------------------------------------------
1020  FORMAT ('Change in COREL REVISION DATE on ',A17)
1030  FORMAT ('Old: ',A8,5X,' New: ',A8)
      END
      SUBROUTINE D3CHK (DOLD, DNEW, DEPS, DNULL, LTIME, LDESC)
C--------------------------------------------------------------------
C   Routine to check whether two double precision variables, DOLD and
C   DNEW, are equal within a fractional error DEPS. No test is
C   carried out if DOLD equals DNULL, the null value. If the values
C   do not agree an AIPS warning message is printed incorporating
C   the time LTIME and the variable name LDESC. DOLD is set to DNEW
C   on exit.
C   Input parameters:
C     DOLD, DNEW   D      Variables to be compared.
C     DEPS         D      Fractional error allowed in comparison.
C     DNULL        D      Value indicating variable is undefined.
C     LTIME        C*20   Time in string format 'IAT ddd/hh mm ss'.
C     LDESC        C*20   Description of variable.
C   Input/output via common
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      CHARACTER LTIME*20, LDESC*20
      DOUBLE PRECISION DOLD, DNEW, DEPS, DNULL, DLOW, DHIGH, DTEMP
C
      INCLUDE 'INCS:DMSG.INC'
C--------------------------------------------------------------------
C                                       Set limits for comparison.
      DTEMP = DOLD * DEPS
      DTEMP = ABS (DTEMP)
      DLOW = DOLD - DTEMP
      DHIGH = DOLD + DTEMP
C                                       Compare the two variables.
      IF (((DNEW .LT. DLOW) .OR. (DNEW .GT. DHIGH)) .AND.
     *   (DOLD .NE. DNULL)) THEN
         WRITE (MSGTXT,1000) LDESC, LTIME
         CALL MSGWRT (6)
         WRITE (MSGTXT,1010) DOLD, DNEW
         CALL MSGWRT (6)
         END IF
C                                       Set DOLD to DNEW
      DOLD = DNEW
      RETURN
C-------------------------------------------------------------------
1000  FORMAT ('Change in ',A20,' on ',A20)
1010  FORMAT ('Old: ',D20.12,' New: ',D20.12)
      END
      SUBROUTINE MK3SOU (LTIME, IRET)
C---------------------------------------------------------------------
C   Routine to write the source info to the SU table. If the source
C   already exists in the SU table, the coordinates are checked for
C   consistency. A buffer is maintained containing the details of the
C   last source processed by MK3SOU.
C   Input parameters:
C      LTIME    C*20    Scan start time as 'IAT ddd/hh mm ss'
C   Output parameters:
C     IRET     I       Return code (0 = OK, <> 0 = Error).
C   Input from common
C      DBLANK   D          Magic value (= indeterminate).
C      KINAX    I(7)       Number of pixels on each axis.
C      KRCIC    R(7)       Coordinate value increment along axis.
C      CATR     R(256)     Catalog header.
C      MKSNAM   C*8(base#) Source name.
C      MKAIF    I          Number of IF's in output file.
C      MKBASE   I          Baseline # of current scan-baseline data.
C      MKDEC    D(base#)   Declination of date (rad).
C      MKDECI   D(base#)   DEC of source (epoch MKEPOC) (rad).
C      MKRA     D(base#)   Right ascension of date (rad).
C      MKRAIN   D(base#)   RA of source (epoch MKEPOC) (rad).
C      MKTIMC   D(2,*)     Time for which a prioris are calculated.
C                          (1=Jul day no; 2= hhmmss in days).
C      JLOCF    I          Order in data of frequency.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C   Input/output via common
C      SULIST   C(100)*16  SU table index: source name
C      UVROT    R(2)       Cos, sin of rotation angle for uv axes
C                          to reference epoch (first order corr)
C      CURSOU   I          Current source number in SU table.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      CATBLK   I(256)     Catalog header.
C      MKEPOC   D(base#)   Coordinate epoch (yr: eg 1950.0, 2000.0).
C      DTDECI   D          Last source DEC at epoch DTEPOC (rad).
C      DTEPOC   D          Last source coordinate epoch (eg 1950.0)
C      DTRAIN   D          Last source RA at epoch DTEPOC (rad).
C      DTTIM1   D          Last scan mid time (Jul day no).
C      DTTIM2   D          Last scan mid time (hhmmss in days).
C      ITSOUR   I          Last source rec no in SU table.
C      LTSRCE   C*8        Last source name.
C      IDSOUR   I(100)     SU table index: source number.
C      SUBUFF   I(1024)    SU table buffer.
C      SUKOLS   I(50)      SU table column pointer array.
C      SUNUMV   I(50)      SU table element count in each column.
C      ISURNO   I          Next scan number in SU table.
C      NSOUR    I          No of sources in source table index.
C---------------------------------------------------------------------
      CHARACTER LTIME*20
      INTEGER IRET
      INTEGER DIR
C
      LOGICAL WNEW, WTRUE
      CHARACTER LVETYP*8, LVEDEF*8, LSUNAM*16, LCALCD*4, LDESC*20
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKSRC.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MK3TAB.INC'
C
      DOUBLE PRECISION DFREQ0(MXM3FQ), DBANDW, DRAEPO, DECEPO,
     *   DEPOCH, DRAAPP, DECAPP, DLSRV(MXM3FQ), DRSTFQ(MXM3FQ),
     *   DPMRA, DPMDC, DR2D, DE, DNUL, DRGEO, DPGEO, DLST, DSRA,
     *   DSDEC, DELDAT, DSJD, DRAP1, DECAP1, DIFFRA, DIFFDC, DRAIN,
     *   DECIN, OBSPOS(3), DRAOBS, DDECOB
      REAL FLUX(4,MXM3FQ), ROTP, POLAR(2)
      INTEGER IDCURR, J, K, M, IVER, ILUN, INOGRP, IDSOU, IQUAL
      INTEGER IRECNO, LSUFQD
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WTRUE/.TRUE./
      DATA OBSPOS, POLAR /3 * 0.0D0, 2 * 0.0/
C--------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      WNEW = .FALSE.
C                                       Same as last source read ?
      IF ((MKSNAM(MKBASE) .EQ. LTSRCE) .AND.
     *    (MKRAIN(MKBASE) .EQ. DTRAIN) .AND.
     *    (MKDECI(MKBASE) .EQ. DTDECI) .AND.
     *    (MKEPOC(MKBASE) .EQ. DTEPOC)) THEN
C                                       Set the source pointer.
         IDCURR = ITSOUR
         GO TO 900
         END IF
C                                       Else search the SU table index.
      J = 1
100   IF (J .GT. NSOUR) GO TO 150
      IF (MKSNAM(MKBASE) .EQ. SULIST(J)) GO TO 150
      J = J + 1
      GO TO 100
C                                       Open the SU table.
150   IDCURR = J
      DR2D = 180.0D0 / (4.0D0 * ATAN (1.0D0))
      IVER = 1
      ILUN = 28
      INOGRP = MKAIF
      CALL SOUINI ('WRIT', SUBUFF, DISKO, CNOOUT, IVER, CATBLK,
     *   ILUN, INOGRP, LVETYP, LVEDEF, LSUFQD, ISURNO, SUKOLS, SUNUMV,
     *   IRET)
      IF (IRET .NE. 0) GO TO 999
C                                       If source already entered in
C                                       SU table, check consistency.
      IF (IDCURR .LE. NSOUR) THEN
         IRECNO = IDCURR
         CALL TABSOU ('READ', SUBUFF, IRECNO, SUKOLS, SUNUMV,
     *      IDSOU, LSUNAM, IQUAL, LCALCD, FLUX, DFREQ0, DBANDW,
     *      DRAEPO, DECEPO, DEPOCH, DRAAPP, DECAPP, DRAOBS, DDECOB,
     *      DLSRV, DRSTFQ, DPMRA, DPMDC, IRET)
         IF (IRET .NE. 0) GO TO 999
C                                       Compare EPOCH, RA0, DEC0
         DE = 0.0D0
         DNUL = DBLANK
         LDESC = 'COORD EPOCH:' // LSUNAM(1:8)
         CALL D3CHK (DEPOCH, MKEPOC(MKBASE), DE, DNUL, LTIME, LDESC)
         LDESC = 'RA0: ' // LSUNAM(1:8)
         DRAIN = MKRAIN(MKBASE) * DR2D
         CALL D3CHK (DRAEPO, DRAIN, DE, DNUL, LTIME, LDESC)
         LDESC = 'DEC0: ' // LSUNAM(1:8)
         DECIN = MKDECI(MKBASE) * DR2D
         CALL D3CHK (DECEPO, DECIN, DE, DNUL, LTIME, LDESC)
C                                       Set flag to update SU table.
         WNEW = (DEPOCH .NE. MKEPOC(MKBASE)) .OR.
     *      (DRAEPO .NE. DRAIN) .OR. (DECEPO .NE. DECIN)
      ELSE
C                                       Create new SU table entry.
         IDSOU = IDCURR
         IDSOUR(IDSOU) = IDCURR
         LSUNAM = MKSNAM(MKBASE) // '        '
         SULIST(IDSOU) = LSUNAM
         NSOUR = NSOUR + 1
         IQUAL = 0
         LCALCD = '    '
         DO 250 K = 1, INOGRP
            DO 200 M = 1, 4
               FLUX(M,K) = 0.0
200            CONTINUE
            DFREQ0(K) = 0.0D0
            DLSRV(K) = 0.0D0
            DRSTFQ(K) = 0.0D0
250         CONTINUE
C                                       Coordinates, bandwith.
         DRAEPO = MKRAIN(MKBASE) * DR2D
         DECEPO = MKDECI(MKBASE) * DR2D
         DEPOCH = MKEPOC(MKBASE)
         DRAAPP = MKRA(MKBASE) * DR2D
         DECAPP = MKDEC(MKBASE) * DR2D
C                                       Bandwidth per channel
         DBANDW = CATR(KRCIC + JLOCF)
         DPMRA = 0.0D0
         DPMDC = 0.0D0
         WNEW = .TRUE.
         END IF
C                                       Write new SU table rec if reqd.
      IF (WNEW) THEN
         IRECNO = IDCURR
         CALL TABSOU ('WRIT', SUBUFF, IRECNO, SUKOLS, SUNUMV,
     *      IDSOU, LSUNAM, IQUAL, LCALCD, FLUX, DFREQ0, DBANDW,
     *      DRAEPO, DECEPO, DEPOCH, DRAAPP, DECAPP, DRAOBS, DDECOB,
     *      DLSRV, DRSTFQ, DPMRA, DPMDC, IRET)
         IF (IRET .NE. 0) GO TO 999
         END IF
C                                       Close SU table.
      CALL TABIO ('CLOS', 0, ISURNO, SUBUFF, SUBUFF, IRET)
      IF (IRET .NE. 0) GO TO 999
C                                       Calculate u,v rotation if necc.
900   CONTINUE
      IF (WNEW .OR. (MKTIMC(1,MKBASE) .NE. DTTIM1)
     *   .OR. (MKTIMC(2,MKBASE) .NE. DTTIM2)) THEN
C                                       Get rotation to MKEPOC by
C                                       precessing 2 close positions
C                                       to measure rotation.
         DRGEO = 0.0D0
         DPGEO = 0.0D0
         DLST = 0.0D0
         DSRA = MKRAIN(MKBASE)
         DSDEC = MKDECI(MKBASE)
         DELDAT = 0.1D0
         DIR = 1
C                                       The old (B1950) routines are
C                                       good enough for this.
C                                       Stil changed by the modified
C                                       JPRECS DEC2004 LK.
         DSJD = MKTIMC(1,MKBASE) + MKTIMC(2,MKBASE)
         CALL JPRECS (DSJD, MKEPOC(MKBASE), DELDAT, DIR, WTRUE,
     *      OBSPOS, POLAR, DSRA, DSDEC, DRAAPP, DECAPP)
C                                       Use 10s north
         DSRA = MKRAIN(MKBASE)
         DSDEC = MKDECI(MKBASE) + (10.0D0 / 3600.0D0) / DR2D
         CALL JPRECS (DSJD, MKEPOC(MKBASE), DELDAT, DIR, WTRUE,
     *      OBSPOS, POLAR, DSRA, DSDEC, DRAAPP, DECAPP)
         DIFFRA = (DRAP1 - DRAAPP) * COS (MKDECI(MKBASE))
         DIFFDC = DECAP1 - DECAPP
         ROTP = -ATAN2 (DIFFRA, DIFFDC)
         UVROT(1) = COS (ROTP)
         UVROT(2) = SIN (ROTP)
         END IF
C                                       Update last source buffer.
      LTSRCE = MKSNAM(MKBASE)
      DTRAIN = MKRAIN(MKBASE)
      DTDECI = MKDECI(MKBASE)
      DTEPOC = MKEPOC(MKBASE)
      DTTIM1 = MKTIMC(1,MKBASE)
      DTTIM2 = MKTIMC(2,MKBASE)
      ITSOUR = IDCURR
C                                       Set the current source pointer.
      CURSOU = IDCURR
C                                       Exit
999   RETURN
      END
      SUBROUTINE ANTSB (LTIME, IRET)
C--------------------------------------------------------------------
C   Routine to process antenna information from the current
C   scan-baseline. The data are checked for consistency.
C   Input parameters:
C     LTIME     C*20    Scan start time as 'IAT ddd/hh mm ss'
C   Output parameters:
C     IRET      I       Return code (<> 0 = error).
C   Input from common
C      ANTNAM   C*8(ant#)  Antenna name.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C   Input/output via common
C      DTACLK   D(ant#)    Station clock rate (us/s) >0 = fast.
C      DTAEPO   D(ant#)    Station clock epoch (s since BOY).
C      DTALOC   D(3,ant#)  X,Y,Z coordinates for antenna ant# using
C                          left-handed axes.
C      DTASYN   D(ant#)    Station clock offset (us) >0 = fast.
C      DBLANK   D          Magic value (= indeterminate).
C      ANTCLK   D(2,base#) Offset clock rate (ref,rem stn) (s/s)
C                          > 0 = fast.
C      ANTEPO   D(2,base#) Antenna clock ref epoch (s since BOY).
C      ANTLOC   D(stn#,3,  XYZ coordinates for stn#1=ref,2=rem
C               base#)     for baseline base# (left-handed).
C      ANTSYN   D(2,base#) Clock sync (ref,rem stn) (us) >0 = fast.
C-------------------------------------------------------------------
      CHARACTER LTIME*20
      INTEGER IRET
C
      DOUBLE PRECISION DEPS
      CHARACTER LSTANM*8, LAXIS(3), LDESC*20
      INTEGER IAA, IANT, J
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LAXIS /'X', 'Y', 'Z'/
C--------------------------------------------------------------------
C                                       Initialisation.
      IRET = 0
C                                       Process baseline stn A, stn B.
      DO 200 IAA = 1,2
C                                       Get antenna no, station name.
         IANT = MKBL(IAA,MKBASE)
         LSTANM = ANTNAM(IANT)
C                                       Check antenna coordinates.
         DEPS = 0.0D0
         DO 100 J = 1,3
            LDESC = LAXIS(J) // ' COORD: ' // LSTANM
            CALL D3CHK (DTALOC(J,IANT), ANTLOC(IAA,J,MKBASE), DEPS,
     *         DBLANK, LTIME, LDESC)
100         CONTINUE
C                                       Check station clock parameters.
         LDESC = 'CLK EPOCH: ' // LSTANM
         CALL D3CHK (DTAEPO(IANT), ANTEPO(IAA,MKBASE), DEPS,
     *      DBLANK, LTIME, LDESC)
         LDESC = 'CLK OFFST: ' // LSTANM
         CALL D3CHK (DTASYN(IANT), ANTSYN(IAA,MKBASE), DEPS,
     *      DBLANK, LTIME, LDESC)
         LDESC = 'CLK RATE: ' // LSTANM
         CALL D3CHK (DTACLK(IANT), ANTCLK(IAA,MKBASE), DEPS,
     *      DBLANK, LTIME, LDESC)
200      CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE FQSRCH (DFQCUR, NFQCUR, DFQTAB, NFQTAB, NFQID, IFQID,
     *   IRET)
C---------------------------------------------------------------------
C   Insert the current frequency table read from tape into the existing
C   set of frequency tables. This routine is used for the RF freq,
C   LO offset and phase-cal tables read from tape, also for the AIPS
C   IF table.
C   Input parameters:
C      DFQCUR     D(MXM3FQ)         Current frequency table.
C      NFQCUR     I                 No of frequencies in DFQCUR.
C      DFQTAB     D(MXFQID,MXM3FQ)  Tabulated set of freq. tables
C                                   (id,frq).
C      NFQTAB     I(MXFQID)         No of freqs for each table id.
C      NFQID      I                 No of freq tables.
C    Output parameters:
C      IFQID      I                 Freq table id of current table.
C      IRET       I                 Return code (<>0 = error).
C   Input/output via common
C      MSGTXT   C*80       AIPS message string.
C---------------------------------------------------------------------
      INCLUDE 'PTAPE.INC'
      DOUBLE PRECISION DFQCUR(MXM3FQ), DFQTAB(MXFQID,MXM3FQ)
      INTEGER NFQCUR, NFQTAB(MXFQID), NFQID, IFQID, IRET
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WEQUAL
      INTEGER IFQ, J
C--------------------------------------------------------------------
C                                       Initialisation.
      IRET = 0
      IFQ = 1
100   IF (IFQ .GT. NFQID) GO TO 150
      WEQUAL = NFQCUR .EQ. NFQTAB(IFQ)
      DO 120 J = 1,NFQTAB(IFQ)
         WEQUAL = WEQUAL .AND. (DFQCUR(J) .EQ. DFQTAB(IFQ,J))
120      CONTINUE
      IF (WEQUAL) GO TO 150
      IFQ = IFQ + 1
      GO TO 100
C                                       Create new entry if not found.
150   CONTINUE
      IF (IFQ .GT. NFQID) THEN
         IF (IFQ .GT. MXFQID) THEN
            WRITE (MSGTXT,1000)
            CALL MSGWRT (7)
            IRET = 1
         ELSE
            NFQID = NFQID + 1
            NFQTAB(NFQID) = NFQCUR
            DO 200 J = 1,NFQCUR
               DFQTAB(NFQID,J) = DFQCUR(J)
200            CONTINUE
            END IF
         END IF
C                                       Exit
      IFQID = IFQ
      RETURN
C---------------------------------------------------------------------
1000  FORMAT ('M3TAR: Too many observing bands - increase MXFQID')
      END
      SUBROUTINE ATORDR (IBASE, IEXT, IRET)
C--------------------------------------------------------------------
C   Process the 20XX cross-reference table for this baseline to
C   produce a correlator module index for this extent. This
C   routine is called before reading the first data record in the
C   extent.
C   Input parameter:
C      IBASE      I       Baseline number.
C      IEXT       I       Extent number.
C   Output parameter:
C      IRET       I       Return code (<> 0 = error).
C   Input from common
C      MAXPOL   I          Max polzn code for current scan baseline.
C      MINPOL   I          Min polzn code for current scan
C                          baseline (1=RR,2=LL,3=RL,4=LR).
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBL     I(2,base#) Antenna numbers for ref, remote antennas
C                          in baseline base#.
C      MKBLNX   I(2,base#) 1=Start, 2=stop pointers for baseline
C                          base# in cross-ref index table MKXREF.
C      MKSMRT   D(base#)   Formatter sample rate (bits/s).
C   Input/output via common
C      DTAFQT   D(ifid#,if#) Set of AIPS IF frequencies for each
C                            IF if#. Each set of frequencies is
C                            identified by the IF id ifid# (Hz)
C      ITNAFQ   I(ifid#)     No of frequencies in each set.
C      NTAFQT   I            No of entries in ITNAFQ.
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCMOD   I(corr#)   No of lags per module for this correlator
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKMNLG   I(corr#)   Min module delay offset for this corr.
C      MKMXLG   I(corr#)   Max module delay offset for this corr.
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKAFRQ   I          No of freq channels per spectrum in
C                          output file.
C      MKAIF    I          Number of IF's in output file.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKNPOL   I          No of polarizations in output file.
C      MKPOL1   I          Min polzn code for output file
C                          (1=RR,2=LL,3=RL,4=LR).
C      MKPOL2   I          Max polzn code for output file.
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      MKAFNO   I(fqid#,   AIPS IF no for freq# of frequency
C               freq#)     id fqid#.
C      MKAFQT   D(if#)     AIPS IF frequencies for the current
C                          scan baseline (Hz).
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKBAFQ   I(base#)   AIPS IF id for baseline base#.
C      MKXREF   I(parm#,   Cross reference table for correlator
C               indx#)     module type 51 data records.
C                          indx# = corr module index wrt start
C                          index for curr baseline (MKBLNX(1,base#)
C                          parm#: 1= Type 51 module index (unused);
C                          2= Ref stn track no (unused); 3= Remote
C                          stn track no (unused); 4= Freq index in
C                          MKFRQ.; 5= Partner track on ref. stn tape
C                          (track on opposite side of same VC)
C                          (unused); 6= Module delay offset (bits);
C                          7= Pulsar gate-on (milli-periods)(unused);
C                          8= Pulsar gate-off (milli-periods) (unus-
C                          ed).; 9= Module termination code (from
C                          COREL at end of scan (2-11 ok); 10= Extent
C                          no; 11= Polarization code (1=RR,2=LL,
C                          3=RL,4=LR); 12= Correlator module serial
C                          no (<0:MK3A, >0: MK3).
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER IBASE, IEXT, IRET
C
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKVAR.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WUPPER, WLOWER, KFIRST, KALL, KDONE(MXCOR)
      DOUBLE PRECISION DFRQJ, DFRQK, DFQDIF
      INTEGER  IFQID, NFREQ, ISORT(2*MXCOR), J, K,
     *   NFRQ1, K1, ITEMP, ICORRL, JNDX, IFREQ, ISTOKE, NLAGS,
     *   NAIF1, JNDX0, NMOD, JJ, KK, MAXLG, IDELAY, INCRD
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set the Stokes range if not
C                                       specified on input.
      IF (MKNPOL .EQ. 0) THEN
         MKNPOL = MAXPOL - MINPOL + 1
         MKPOL1 = MINPOL
         MKPOL2 = MAXPOL
         END IF
      IFQID = MKBFQ(IBASE)
      NFREQ = MKNFQT(IFQID)
C                                       Sort the RF freq table in
C                                       ascending order of magnitude
      DO 100 J = 1,NFREQ
         ISORT(J) = J
100      CONTINUE
C                                       Perform a bubble ptr sort.
      NFRQ1 = NFREQ - 1
      DO 200 J = 1,NFRQ1
         K1 = J + 1
         DO 150 K = K1,NFREQ
            DFRQJ = ABS (MKFQT(IFQID,ISORT(J)))
            DFRQK = ABS (MKFQT(IFQID,ISORT(K)))
            IF (DFRQJ.GT.DFRQK) THEN
               ITEMP = ISORT(J)
               ISORT(J) = ISORT(K)
               ISORT(K) = ITEMP
               END IF
150         CONTINUE
200      CONTINUE
C                                       Create an AIPS FQ table entry
C                                       (freqs must differ by 1kHz).
      WUPPER = .FALSE.
      WLOWER = .FALSE.
      J = 0
      DO 300 K = 1,NFREQ
         MKAFQT(K) = 0.0D0
         DFRQK = MKFQT(IFQID,ISORT(K))
         WUPPER = WUPPER .OR. (DFRQK .GT. 0)
         WLOWER = WLOWER .OR. (DFRQK .LT. 0)
         DFRQK = ABS (DFRQK)
         DFQDIF = 1.0D5
         IF (J .NE. 0) DFQDIF = ABS (DFRQK - MKAFQT(J))
         IF (DFQDIF .GT. 1.0D3) THEN
            J = J + 1
            MKAFQT(J) = DFRQK
            END IF
         MKAFNO(IFQID,ISORT(K)) = J
300      CONTINUE
      IF (MKAIF .EQ. 0) MKAIF = J
C                                       Check sidebands present
      IF (MKSIDB .EQ. -999) THEN
         IF (WLOWER .AND. (.NOT. WUPPER)) MKSIDB = -1
         IF (WUPPER .AND. (.NOT. WLOWER)) MKSIDB = 1
         IF (WUPPER .AND. WLOWER) MKSIDB = 0
         END IF
C                                       Add to the set of AIPS
C                                       FQ tables. Append sample
C                                       rate to the (n+1)-th element.
      NAIF1 = MKAIF + 1
      MKAFQT(NAIF1) = MKSMRT(IBASE)
      CALL FQSRCH (MKAFQT, NAIF1, DTAFQT, ITNAFQ, NTAFQT,
     *   MKBAFQ(IBASE), IRET)
      IF (IRET .NE. 0) GO TO 999
C                                       Sort cross-ref table on delay.
      JNDX0 = MKBLNX(1,IBASE)
      NMOD = MKBLNX(2,IBASE) - MKBLNX(1,IBASE) + 1
      DO 350 J = 1,NMOD
         ISORT(J) = J
         KDONE(J) = .FALSE.
350      CONTINUE
C                                       Bubble ptr sort.
      DO 400 J = 1,NMOD-1
         DO 380 K = J+1,NMOD
            JJ = JNDX0 + ISORT(J) - 1
            KK = JNDX0 + ISORT(K) - 1
            IF (MKXREF(6,JJ) .GT. MKXREF(6,KK)) THEN
               ITEMP = ISORT(J)
               ISORT(J) = ISORT(K)
               ISORT(K) = ITEMP
               END IF
380         CONTINUE
400      CONTINUE
C                                       Generate the correlator table.
      ICORRL = 0
      MAXLG = 0
C                                       Extract a set of correlators
C                                       from the cross-ref table.
C                                        Search for sets of modules
C                                       with contiguous delay offsets
C                                       at intervals of 8 or 16 lags.
C                                       Repeat (search) until (all
C                                       modules processed)
450   KFIRST = .TRUE.
      KALL = .TRUE.
C                                       Search through the module
C                                       cross-reference table.
      DO 550 J = 1,NMOD
         JNDX = JNDX0 + ISORT(J) - 1
C                                       Skip data with zero index
C                                       in cross-ref table.
         IF (MKXREF(1,JNDX) .LE. 0) GO TO 550
C                                       Use data only for this extent.
         IF (MKXREF(10,JNDX) .NE. IEXT) GO TO 550
C                                       Accept only valid Stokes pairs.
         IFREQ = ABS (MKXREF(4,JNDX))
         ISTOKE = MKXREF(11,JNDX)
         IF ((ISTOKE .LT. MKPOL1) .OR. (ISTOKE .GT. MKPOL2)) GO TO 550
C                                       Skip module if already processed
         IF (KDONE(J)) GO TO 550
         KALL = .FALSE.
C                                       First module of the current
C                                       correlator ?
         IF (.NOT. KFIRST) GO TO 520
C                                       Too many correlators ?
            IF (ICORRL .EQ. MXCOR) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (6)
               IRET = -1
               GO TO 999
               END IF
C                                       Create new correlator entry.
            ICORRL = ICORRL + 1
            MKCFRQ(ICORRL) = IFREQ
            MKCSTK(ICORRL) = ISTOKE
            MKCMOD(ICORRL) = 0
            MKCTYP(ICORRL) = 999
            IDELAY = MKXREF(6,JNDX)
            MKMNLG(ICORRL) = IDELAY
            MKMXLG(ICORRL) = IDELAY
            KFIRST = .FALSE.
            KDONE(J) = .TRUE.
         GO TO 550
C                                       Else can this module be added to
C                                       the current correlator ?
520      INCRD = MKXREF(6,JNDX) - IDELAY
         IF (INCRD .EQ. 0) GO TO 550
         IF ((IFREQ .EQ. MKCFRQ(ICORRL)) .AND. (MKCSTK(ICORRL) .EQ.
     *      ISTOKE) .AND. ((INCRD .EQ. MKCMOD(ICORRL)) .OR.
     *      ((MKCMOD(ICORRL) .EQ. 0) .AND. ((INCRD .EQ. 8) .OR.
     *       (INCRD .EQ. 16))))) THEN
C                                       Add to the current correlator.
            IDELAY = MKXREF(6,JNDX)
            MKMXLG(ICORRL) = IDELAY
            MKCMOD(ICORRL) = INCRD
            KDONE(J) = .TRUE.
            END IF
550      CONTINUE
C                                       Compute lag range.
      IF (MKCMOD(ICORRL) .EQ. 0) MKCMOD(ICORRL) = 8
      NLAGS = MKMXLG(ICORRL) - MKMNLG(ICORRL) + MKCMOD(ICORRL)
      MAXLG = MAX (MAXLG, NLAGS)
C                                       All correlators found ?
      IF (.NOT. KALL) GO TO 450
      MKNCOR = ICORRL
C                                       Set no. of freq. channels
C                                       if default lag range
C                                       specified on input.
      IF (MKNLAG .GT. 0) MKAFRQ = MKNLAG / 2
C                                       Exit
999   RETURN
C-------------------------------------------------------------------
1000  FORMAT ('ATORDR: PARAMETER MXCOR NEEDS TO BE INCREASED')
      END
      SUBROUTINE MK3FQ (IRET)
C--------------------------------------------------------------------
C   Write the FQ table.
C   Output parameters:
C     IRET    I     Return code (<> 0 = error).
C   Input from common
C      DTAFQT   D(ifid#,if#) Set of AIPS IF frequencies for each
C                            IF if#. Each set of frequencies is
C                            identified by the IF id ifid# (Hz)
C      NTAFQT   I            No of entries in ITNAFQ.
C      REFREQ   D          Reference frequency (Hz) in catalog hdr.
C      MKAIF    I          Number of IF's in output file.
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKSIDB   I          0= Double sideband concatenation in
C                          output file.
C      DOSIDB   L          If true separate sidebands by IF
C   Input/output via common
C      BUFFER   R(UVBFSS)  Work buffer.
C      CNOOUT   I          Output file sequence number.
C      DISKO    I          Output disk number.
C      CATBLK   I(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C      FQKOLS   I(10)      FQ table column pointer array.
C      FQNUMV   I(10)      FQ table element count in each column.
C      IFQRNO   I          Next row number in FQ table.
C--------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'MK3IN.INC'
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION DIFFRQ(MXM3FQ)
      REAL      CHWIF(MXM3FQ), TBWIF(MXM3FQ), BANDWD, CHW
      INTEGER   ILUN, IVER, NUMIF, IFQAID, IFNO, IFSIDE(MXM3FQ)
      CHARACTER BNDCOD(MXM3FQ)*8
      DATA ILUN /27/
C-----------------------------------------------------------------------
C                                       Skip if no FQ entries.
      IF (NTAFQT .LE. 0) GO TO 999
C                                       Open FQ table for write.
      IVER = 1
      NUMIF = MKAIF
      CALL FQINI ('WRIT', BUFFER, DISKO, CNOOUT, IVER, CATBLK, ILUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET .NE. 0) GO TO 990
      IFQRNO = 1
C                                       Loop over the AIPS freq. tables.
      DO 200 IFQAID = 1,NTAFQT
         BANDWD = DTAFQT(IFQAID,NUMIF+1) * 0.5
         CHW = 2.0 * BANDWD / MKNLAG
C                                       Double sideband?
         IF ((MKSIDB.EQ.0) .AND. (.NOT.DOSIDB)) BANDWD = 2.0 * BANDWD
C                                       Fill the FQ table record.
         DO 100 IFNO = 1,NUMIF
            DIFFRQ(IFNO) = DTAFQT(IFQAID,IFNO) - REFREQ
            CHWIF(IFNO) = CHW
            TBWIF(IFNO) = BANDWD
            IFSIDE(IFNO) = 1
            BNDCOD(IFNO) = ' '
100         CONTINUE
C                                       Write the FQ table entry.
         CALL TABFQ ('WRIT', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      IFQAID, DIFFRQ, CHWIF, TBWIF, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) GO TO 990
C
200   CONTINUE
C                                       Close the FQ table.
      CALL TABFQ ('CLOS', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *   IFQAID, DIFFRQ, CHWIF, TBWIF, IFSIDE, BNDCOD, IRET)
      IF (IRET .NE. 0) GO TO 990
      GO TO 999
C                                       Error handling.
990   WRITE (MSGTXT,1020) IRET
      CALL MSGWRT (8)
C                                       Exit.
999   RETURN
C--------------------------------------------------------------------
1020  FORMAT ('MK3FQ: Error ',I3,' writing FQ table')
      END
      SUBROUTINE ATXACF (NLAGS, ISB, MINL, MAXL, DATA, TWORK)
C---------------------------------------------------------------------
C   Transform MK3 AC functions to AC spectra. Corrects for bias
C   and clipping. Uses routine FOURG. Expects an AC function in
C   the form {0,..N/2}, {0,..(N/2-1)}, {-N/2,..0} or {(-N/2+1),..0}.
C   Input parameters:
C      NLAGS    I       No of lags N in each XC function (2*AC fn)
C      ISB      I       Sideband (-1 = LSB; +1 = USB).
C      MINL     I       Min lag known for the AC function.
C      MAXL     I       Max lag known for the AC function.
C      DATA     R(2,*)  Array containing (Re,Im=0) AC function
C                       in increasing lag order over NLAGS/2 lags.
C      TWORK    R(2,*)  Work array (same dimension as DATA)
C---------------------------------------------------------------------
      REAL DATA(2,*), TWORK(2,*)
      INTEGER NLAGS, ISB, MINL, MAXL
C
      LOGICAL WREVR
      REAL TEMP, BIAS, BNORM
      INTEGER NLAGS2, NLAGS4, J, K, ISTART, ISTOP, NBIAS, ISIGN
      INTEGER NPNTS, NPNTS2
C---------------------------------------------------------------------
      NLAGS2 = NLAGS / 2
      NLAGS4 = NLAGS / 4
      NPNTS = MAXL - MINL + 1
      NPNTS2 = NPNTS / 2
C                                       Reverse first half of AC fn ?
      WREVR = MINL .NE. 0

      IF (WREVR) THEN
         DO 10 J = 1,NPNTS2
            K = NPNTS - J + 1
            TEMP = DATA(1,J)
            DATA(1,J) = DATA(1,K)
            DATA(1,K) = TEMP
10          CONTINUE
         END IF
C                                       Determine AC bias using last
C                                       third of AC function.
      BIAS = 0.0
      ISTART = (NPNTS * 2) / 3
      ISTOP = NPNTS
      DO 30 J = ISTART,ISTOP
         BIAS = BIAS + DATA(1,J)
30       CONTINUE
      NBIAS = ISTOP - ISTART + 1
      IF (NBIAS .NE. 0) BIAS = BIAS / NBIAS
      BNORM = 1.0 / (1.0 - BIAS)
C                                       Apply bias correction and
C                                       the Van Vleck correction.
C                                       Also normalise and mirror
C                                       the AC fn.
C                                       (Davis,AA Supl, 15, p381)
      DATA(1,1) = 1.0
      DATA(2,1) = 0.0
      DO 50 J = 2,NLAGS2
         DATA(1,J) = (SIN (DATA(1,J)) - BIAS) * BNORM
         DATA(2,J) = 0.0
         K = NLAGS - J + 2
         DATA(1,K) = DATA(1,J)
         DATA(2,K) = 0.0
50       CONTINUE
C                                       Pad lag (+- N/2) if necc.
      IF (MAX (ABS(MAXL), ABS(MINL)) .LT. NLAGS2) THEN
         DATA(1,NLAGS2+1) = 0.0
         DATA(2,NLAGS2+1) = 0.0
         END IF
      ISIGN = 1
      IF (ISB .LT. 0) ISIGN = -1
C                                       Phase ramp to shift frequency
C                                       sampling.
      CALL ATFSHT (DATA, NLAGS, ISIGN)
C                                       Fourier transform to AC spect.
      CALL FOURG (DATA, NLAGS, ISIGN, TWORK)
C                                       Exit
      RETURN
      END
      SUBROUTINE PRTPAR
C-----------------------------------------------------------------
C   Print the correlator model parameters at the end of the run
C   if required.
C   Input from common
C      DTACLK   D(ant#)    Station clock rate (us/s) >0 = fast.
C      DTAEPO   D(ant#)    Station clock epoch (s since BOY).
C      DTALOC   D(3,ant#)  X,Y,Z coordinates for antenna ant# using
C                          left-handed axes.
C      DTASYN   D(ant#)    Station clock offset (us) >0 = fast.
C      DBLANK   D          Magic value (= indeterminate).
C      ANTNAM   C*8(ant#)  Antenna name.
C      MKPRNT   I          Print level (0..4).
C      NANT     I          No of antennas in antenna list.
C      ATJDRF   D          Reference Jul day no for output file.
C      DTGAST   D(day#)    GAST at 0 UT on day# (rad).
C      DTJULD   D(day#)    Julian day no for run based parameters.
C      DTSIDD   D(day#)    d(GAST)/d(UT) on day# (rad/s).
C      DTUT1    D(day#)    UT1-UTC (s) on day#.
C      DTWOB    D(2,day#)  X(=1),Y(=2) pole posn on day# (arcsec).
C      DTVLIG   D          Speed of light value for run (m/s).
C      NTJULD   I          No of entries in DTJULD.
C      LTCVER   C*8        COREL revision level.
C   Input/output via common
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKTIM.INC'
      INCLUDE 'MKSTA.INC'
      INCLUDE 'MKCHR.INC'
      INCLUDE 'MK3TAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      INTEGER I
C----------------------------------------------------------------------
C                                       Check print level.
      IF (MKPRNT .LE. 0) GO TO 999
C                                       Print header
      WRITE (MSGTXT,1000)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1010) LTCVER
      CALL MSGWRT (6)
      WRITE (MSGTXT,1020) DTVLIG
      CALL MSGWRT (6)
      WRITE (MSGTXT,1025) ATJDRF
      CALL MSGWRT (6)
C                                       Run parameters.
      WRITE (MSGTXT,1030)
      CALL MSGWRT (6)
      DO 20 I = 1,NTJULD
         WRITE (MSGTXT,1040) DTJULD(I), DTUT1(I), DTGAST(I), DTSIDD(I),
     *      DTWOB(1,I), DTWOB(2,I)
         CALL MSGWRT (6)
20       CONTINUE
C                                       Antenna clocks.
      WRITE (MSGTXT,1045)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1050)
      CALL MSGWRT (6)
      DO 50 I = 1,NANT
         IF (ANTNAM(I) .EQ. 'ANY') GO TO 50
         IF (DTALOC(1,I) .EQ. DBLANK) GO TO 50
         WRITE (MSGTXT,1060) ANTNAM(I), DTAEPO(I), DTASYN(I), DTACLK(I)
         CALL MSGWRT (6)
50       CONTINUE
C                                       Exit
999   RETURN
C--------------------------------------------------------------------
1000  FORMAT ('CORRELATOR MODEL PARAMETERS')
1010  FORMAT ('COREL Revision level: ',A8)
1020  FORMAT ('Speed of light (m/s): ', D17.10)
1025  FORMAT ('Reference Julian day: ',F10.1)
1030  FORMAT ('Jul day',4X,'UT1-UTC',2X,'GAST (rad)',2X,
     *   'GAST dot rad/s',2X,'X,Y pole (arc s)')
1040  FORMAT (F9.1,2X,F7.4,2X,F10.7,2X,E14.7,2X,F7.4,2X,F7.4)
1045  FORMAT ('Station Clocks')
1050  FORMAT ('Antenna',4X,'Epoch (s BOY)',6X,'Offset (us)',
     *   7X,'Rate (us/s)')
1060  FORMAT (A8,3X,D15.8,4X,D15.8,3X,D15.8)
      END
      SUBROUTINE DATQ
C--------------------------------------------------------------------
C   Print a data quality summary for each lag spectrum.
C   The print options (MKPRNT) are:
C     0 = Source header for each scan.
C     1 = Scan error summary included (also correlator model summary
C         at end of run.
C     2 = List each correlator.
C     3 = Print COREL file names.
C     4 = Error table included for each correlator module.
C   Input from common
C      MKCFRQ   I(corr#)   Freq# for this correlator.
C      MKCSTK   I(corr#)   Polzn. no for this correlator.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C      MKSERL   I(mod#)    Serial no of correlator module mod#.
C      MKNCOR   I          No of correlators for current scan
C                          baseline data.
C      MKNSER   I          No of entries in MKSERL.
C      MKPRNT   I          Print level (0..4).
C      MKFQT    D(fqid#,   Table of RF frequencies in same order
C               freq#)     as MKFRQ.  Each set of frequencies is
C                          identified by the freq id fqid# (Hz).
C      MKNFQT   I(fqid#)   No of frequencies in each set.
C      MKBFQ    I(base#)   Frequency id #fqid for baseline base#.
C      MKBASE   I          Baseline # of current scan-baseline data.
C   Input/output via common
C      MKCTOT   I(corr#)   Total no of correlation functions for this
C                          correlator over the current scan baseline.
C      MKER51   I(mod#,    No of type 51 records rejected with error
C               error#)    number error# for module mod#.
C      MKGOOD   I(corr#)   No of good correlation functions for
C                          this correlator over the current scan
C                          baseline.
C      MKNR51   I(mod#)    Total no of type 51 data records read
C                          for module mod#.
C      MSGTXT   C*80       AIPS message string.
C--------------------------------------------------------------------
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKSCN.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'MKRFQ.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WPRNT
      CHARACTER LSCODE(4)*3, LSTYP(2)*3, LSIDEB(2)*4, LEXTRA(8)*3
      REAL FREQLO, FREQHI, PERCNT, FRQMHZ, REC51
      INTEGER I, J, IPOLCD(4), ISDB(2), NGOOD, NTOTL, IFREQ,
     *   ISTOKE, IFQID, NFREQ, ICORR, ISIDEB, ICTYP(2), ICTYPE
      DATA LSCODE /' RR',' LL',' RL',' LR'/
      DATA LSIDEB /' USB',' LSB'/
      DATA LSTYP /' XC',' AC'/
C-----------------------------------------------------------------------
C                                       Initialisation.
      NGOOD = 0
      NTOTL = 0
      DO 20 I = 1,4
         IPOLCD(I) = 0
20       CONTINUE
      DO 25 I = 1,2
         ISDB(I) = 0
         ICTYP(I) = 0
25       CONTINUE
      DO 30 I = 1,8
         LEXTRA(I) = '   '
30       CONTINUE
      FREQLO = 999E15
      FREQHI = -999E15
      IFQID = MKBFQ(MKBASE)
      NFREQ = MKNFQT(IFQID)
      WPRNT = .FALSE.
C                                       Loop over the lag spectra.
      DO 200 ICORR = 1,MKNCOR
C                                       Valid correlator ?
         IF (MKCTYP(ICORR) .LT. 0) GO TO 200
C                                       Freq, Stokes
         IFREQ = MKCFRQ(ICORR)
         ISTOKE = MKCSTK(ICORR)
C                                       Determine freq (MHz), sideband.
         FRQMHZ = MKFQT(IFQID,IFREQ) / 1.0D6
         ISIDEB = 1
         IF (FRQMHZ .LT. 0) ISIDEB = 2
         FRQMHZ = ABS (FRQMHZ)
C                                       Determine the corr. type
         ICTYPE = 1
         IF ((MKCTYP(ICORR).EQ.1).OR.(MKCTYP(ICORR).EQ.2)) ICTYPE = 2
C                                       Check print level.
         IF (MKPRNT .GE. 2) THEN
            PERCNT = 0.0
            IF (MKCTOT(ICORR) .GT. 0) PERCNT = REAL (MKGOOD(ICORR))
     *         / REAL (MKCTOT(ICORR)) * 100.0
            WRITE (MSGTXT,1000) LSTYP(ICTYPE), FRQMHZ, LSIDEB(ISIDEB),
     *         LSCODE(ISTOKE), PERCNT, MKGOOD(ICORR)
            CALL MSGWRT (6)
         ELSE
            NGOOD = NGOOD + MKGOOD(ICORR)
            NTOTL = NTOTL + MKCTOT(ICORR)
            IPOLCD(ISTOKE) = 1
            ISDB(ISIDEB) = 1
            ICTYP(ICTYPE) = 1
            FREQLO = MIN (FREQLO, FRQMHZ)
            FREQHI = MAX (FREQHI, FRQMHZ)
            END IF
         WPRNT = .TRUE.
200      CONTINUE
C                                       Scan-baseline summary.
      IF (MKPRNT .LT. 2) THEN
         IF (FREQLO .EQ. 999E15) FREQLO = 0.0
         IF (FREQHI .EQ. -999E15) FREQHI = 0.0
         J = 0
         DO 250 I = 1,2
            IF (ISDB(I) .GT. 0) THEN
               J = J + 1
               LEXTRA(J) = LSIDEB(I)(:3)
               END IF
250         CONTINUE
         DO 270 I = 1,4
            IF (IPOLCD(I) .GT. 0) THEN
               J = J + 1
               LEXTRA(J) = LSCODE(I)
               END IF
270         CONTINUE
         DO 290 I = 1,2
            IF (ICTYP(I) .GT. 0) THEN
               J = J + 1
               LEXTRA(J) = LSTYP(I)
               END IF
290         CONTINUE
         IF (WPRNT) THEN
            WRITE (MSGTXT,1010) NGOOD, FREQLO, FREQHI, LEXTRA
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Print msge if no data selected.
      IF (.NOT. WPRNT) THEN
         WRITE (MSGTXT,1012)
         CALL MSGWRT (6)
         END IF
C                                       Type 51 error summary
      IF (MKPRNT .LE. 0) GO TO 999
      DO 350 J = 1,MKNSER
         REC51 = REAL (MKNR51(J))
         DO 300 I = 1,16
            PERCNT = 0.0
            IF (MKNR51(J).GT.0) PERCNT = MKER51(J,I) / REC51 * 100.0
            MKER51(J,I) = PERCNT
            IF (MKER51(J,I) .EQ. 100) MKER51(J,I) = 99
300         CONTINUE
350      CONTINUE
C                                       Error summary header.
      IF (MKNSER .NE. 0) THEN
         WRITE (MSGTXT,1015)
         CALL MSGWRT (6)
         END IF
      DO 400 J = 1,MKNSER
         WRITE (MSGTXT,1020) MKSERL(J), (MKER51(J,I),I=1,16), MKNR51(J)
         CALL MSGWRT (6)
400      CONTINUE
C                                       Exit
999   RETURN
C---------------------------------------------------------------------
1000  FORMAT (A3,' Freq = ',F9.3,' MHz',A4,A3,' Good = ',F5.1,'%',
     *    ' NVis = ',I6)
1010  FORMAT ('Nvis=',I6,' Freq= ',F9.3,' - ',F9.3,8A3)
1012  FORMAT ('No data selected for this scan')
1015  FORMAT ('  MOD CS TS AP PP YS XS CL TM CRCC  XP YP XD YD MT',
     *   ' 3A  TOT')
1020  FORMAT (I5,16I3,I6)
      END
      SUBROUTINE ATLAG (WAUTO, ICORR, IRET)
C--------------------------------------------------------------------
C   Determine the max symmetric lag range for a correlator.
C   M3TAR expects correlation functions that cover lag ranges
C   symmetric about zero lag.
C   Inputs:
C      WAUTO    L    Autocorrelator ?
C      ICORR    I    Correlator number in correlator table.
C   Output:
C      IRET     I    Termination status <>0 => error
C   Input from common
C      MKMNLG   I(corr#)   Min module delay offset for this correlator
C      MKMXLG   I(corr#)   Max module delay offset for this correlator
C      MKCMOD   I(corr#)   No of lags per module for this correlator
C   Input/output via common
C      MKNLAG   I          No of lags in each correlation function
C                          in output file.
C      MKAFRQ   I          No of frequency channels per spectrum
C                          in output file.
C      MKCMIN   I(corr#)   Min usable lag for this correlator.
C      MKCMAX   I(corr#)   Max usable lag for this correlator.
C      MKCTYP   I(corr#)   Correlator type (0=xc; 1=AC ant 1;
C                          2=AC ant 2; -ve = correlator deselected).
C--------------------------------------------------------------------
      LOGICAL WAUTO
      INTEGER ICORR, IRET
C
      INCLUDE 'PTAPE.INC'
      INCLUDE 'MKINP.INC'
      INCLUDE 'MKCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WVALID
      CHARACTER LXCAC*2
      INTEGER JMINLG, JMAXLG, JABS1, JABS2, JABSMN, JABSMX, NLAGS
      REAL XPROD
C--------------------------------------------------------------------
C                                            Initialisation
      IRET = 0
      LXCAC = 'XC'
      IF (WAUTO) LXCAC = 'AC'
      NLAGS = 0
C                                       Compute actual lag range for
C                                       this correlator.
      JMINLG = MKMNLG(ICORR) - MKCMOD(ICORR) / 2 + 1
      JMAXLG = MKMXLG(ICORR) + MKCMOD(ICORR) / 2
      XPROD = JMINLG * JMAXLG
      JABS1 = ABS (JMINLG)
      JABS2 = ABS (JMAXLG)
      JABSMN = MIN (JABS1, JABS2)
      JABSMX = MAX (JABS1, JABS2)
C                                       Is this lag range valid ?
      IF (WAUTO) THEN
C                                       AC: (-ve,0),  (0,+ve),
C                                           (-ve,-1), (1, +ve)
         WVALID = ((JABSMN .LE. 1) .AND. (JABSMX .GT. JABSMN) .AND.
     *      (XPROD .GE. 0.0))
      ELSE
C                                       XC: (-ve,+ve) or (+ve,-ve)
         WVALID = (XPROD .LT. 0.0)
         END IF
C                                       Invalid default lag range ?
      IF ((.NOT.WVALID).AND.(MKNLAG.LE.0)) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT(8)
         IRET = 3
         END IF
      IF ((IRET.NE.0).OR.(.NOT.WVALID)) GO TO 900
C                                       Compute max symmetric lag
C                                       range for this correlator.
      IF (.NOT. WAUTO) THEN
C                                       XC:
C                                       (-N/2+1),...,0,...(N/2)
         IF (JABS1 .GT. JABS2) THEN
            MKCMIN(ICORR) = -(JABS2 - 1)
            MKCMAX(ICORR) = JMAXLG
            END IF
         IF (JABS1 .LT. JABS2) THEN
            MKCMIN(ICORR) = JMINLG
            MKCMAX(ICORR) = JABS1 + 1
            END IF
         IF (JABS1 .EQ. JABS2) THEN
            MKCMIN(ICORR) = -(JABS2 - 1)
            MKCMAX(ICORR) = JMAXLG
            END IF
         NLAGS = MKCMAX(ICORR) - MKCMIN(ICORR) + 1
      ELSE
C                                       AC: Either
C                                       {(-N/2+1),..0} or {0,..(N/2-1)}
         IF (JABSMN.EQ.0) THEN
            IF (JABS1 .LE. JABS2) THEN
               MKCMIN(ICORR) = 0
               MKCMAX(ICORR) = JMAXLG
            ELSE
               MKCMIN(ICORR) = JMINLG
               MKCMAX(ICORR) = 0
               END IF
         ELSE
C                                       AC: Either
C                                       {(-N/2,..-1} or {1,..(N/2)}
            IF (JABS1 .LE. JABS2) THEN
               MKCMIN(ICORR) = 0
               MKCMAX(ICORR) = JMAXLG - 1
            ELSE
               MKCMIN(ICORR) = JMINLG + 1
               MKCMAX(ICORR) = 0
               END IF
            END IF
         NLAGS = 2 * (MKCMAX(ICORR) - MKCMIN(ICORR) + 1)
         END IF
C                                       Set default lag range if
C                                       not specified on input.
      IF (MKNLAG .EQ. 0) THEN
         MKNLAG = NLAGS
         MKAFRQ = MKNLAG / 2
         END IF
C                                       Check lag range
      IF ((NLAGS .EQ. MKNLAG) .OR.
     *   (WAUTO .AND. (NLAGS .EQ. (MKNLAG+2)))) GO TO 999
C                                       Invalid lag range.
 900     WRITE (MSGTXT,1010) LXCAC, JMINLG, JMAXLG
         CALL MSGWRT (6)
         IF (NLAGS .NE. 0) THEN
            WRITE (MSGTXT,1020) NLAGS
            CALL MSGWRT (6)
            END IF
         MKCTYP(ICORR) = -1
C                                       Exit
 999  RETURN
C--------------------------------------------------------------------
 1000 FORMAT ('ATLAG: Set default lag range (NO_LAGS) in exp file')
 1010 FORMAT ('ATLAG: Skipping ',A2,' function with lag range',
     *   I5,' to',I5)
 1020 FORMAT ('ATLAG: Use NO_LAGS=',I5,' for this correlation function')
      END
      SUBROUTINE ATFSHT (DATA, N, SIDEBN)
C-----------------------------------------------------------------------
C   Routine to apply phase ramp to a complex vector such that when it is
C   Fourier transformed the sampled frequencies are shifted by one half
C   cell.  Uses the Fourier shift theorem.
C      Expects data with the center value in the first element following
C   the usual FFT conventions.
C   Inputs:
C      N         I  The number of complex elements in DATA
C      SIDEBN    I  Sideband, 1=upper, -1 = lower
C   Input/Output:
C      DATA(2,N) R  Complex vector
C-----------------------------------------------------------------------
      INTEGER   N, SIDEBN
      REAL      DATA(2,N)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   LOOP, NRAMP, LOC
      REAL      TRE, TIM, RAMP(2,2*MAXCHA), ARG, PI
      SAVE NRAMP, RAMP
      DATA  PI /3.141592654/
C-----------------------------------------------------------------------
C                                       Need to compute ramp?
      IF (NRAMP.NE.N) THEN
C                                       Check that big enough
         IF (N.GT.2*MAXCHA) THEN
            MSGTXT = 'ATFSHT:INTERNAL ARRAY TOO SMALL'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 50 LOOP = 1,N/2
            LOC = LOOP - 1
            ARG = LOC * PI / N
            RAMP(1,LOOP) = COS (ARG)
            RAMP(2,LOOP) = SIN (ARG)
 50         CONTINUE
         DO 60 LOOP = N/2+1,N
            LOC = LOOP - N - 1
            ARG = LOC * PI / N
            RAMP(1,LOOP) = COS (ARG)
            RAMP(2,LOOP) = SIN (ARG)
 60         CONTINUE
         NRAMP = N
         END IF
C                                       Apply by sideband
      IF (SIDEBN.GT.0) THEN
C                                       Upper sideband
         DO 100 LOOP = 1,N
            TRE = DATA(1,LOOP)
            TIM = DATA(2,LOOP)
            DATA(1,LOOP) = TRE*RAMP(1,LOOP) - TIM*RAMP(2,LOOP)
            DATA(2,LOOP) = TIM*RAMP(1,LOOP) + TRE*RAMP(2,LOOP)
 100        CONTINUE
      ELSE
C                                       Lower sideband
         DO 200 LOOP = 1,N
            TRE = DATA(1,LOOP)
            TIM = DATA(2,LOOP)
            DATA(1,LOOP) = TRE*RAMP(1,LOOP) + TIM*RAMP(2,LOOP)
            DATA(2,LOOP) = TIM*RAMP(1,LOOP) - TRE*RAMP(2,LOOP)
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE UPFLIS (NIN, FIN, MAXOUT, NOUT, FOUT)
C-----------------------------------------------------------------------
C   Routine to add any missing frequencies to the end of a list of
C   frequencies.  Any frequencies within 1 kHz are considered the same.
C   Inputs:
C      NIN   I     Number of input frequencies
C      FIN   D(*)  List of potential freq. to append.
C   Input/Output:
C     MAXOUT I     Dimension of FOUT
C     NOUT   I     Number of frequencies in existing list.
C     FOUT   D(*)  List to be updated.
C-----------------------------------------------------------------------
      INTEGER   NIN, NOUT, MAXOUT
      DOUBLE PRECISION FIN(*), FOUT(MAXOUT)
C
      INTEGER   LOOP, LOOP2
      LOGICAL   MATCH
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,NIN
         MATCH = .FALSE.
         DO 50 LOOP2 = 1,NOUT
            MATCH = MATCH .OR. ABS (FIN(LOOP)-FOUT(LOOP2)).LE.1.0D3
 50         CONTINUE
         IF ((.NOT.MATCH) .AND. (NOUT.LT.MAXOUT)) THEN
C                                       Add to list
            NOUT = NOUT + 1
            FOUT(NOUT) = FIN(LOOP)
            END IF
 100     CONTINUE
 999  RETURN
      END
      SUBROUTINE DSBFIX (LSBOFF, MAXOUT, NOUT, FOUT)
C-----------------------------------------------------------------------
C   Routine to add any missing sidebands
C   frequencies.  Any frequencies within 1 kHz are considered the same.
C   Inputs:
C      LSBOFF D     Correction for LSB freq.
C      MAXOUT I     Dimension of FOUT
C   Input/Output:
C      NOUT   I     Number of frequencies in existing list.
C      FOUT   D(*)  List to be updated.  Neg. freq = LSB.
C-----------------------------------------------------------------------
      INTEGER   NOUT, MAXOUT
      DOUBLE PRECISION LSBOFF, FOUT(MAXOUT), TF
C
      INTEGER   LOOP, LOOP2, N
      LOGICAL   LSB, USB, MATCH
C-----------------------------------------------------------------------
C                                       Is this dsb data?
      LSB = .FALSE.
      USB = .FALSE.
      DO 10 LOOP = 1,NOUT
         LSB = LSB .OR. FOUT(LOOP).LT.0.0D0
         USB = USB .OR. FOUT(LOOP).GT.0.0D0
 10      CONTINUE
C                                       In not both - quit.
      IF (.NOT.(LSB.AND.USB)) GO TO 999
C                                        Check each for other SB
      N = NOUT
      DO 100 LOOP = 1,N
         TF = ABS (FOUT(LOOP))
         IF (FOUT(LOOP).LT.0.0D0) TF = TF + LSBOFF
         MATCH = .FALSE.
         DO 50 LOOP2 = 1,N
            MATCH = MATCH .OR. ((ABS (TF-FOUT(LOOP2)).LE.1.0D3)
     *         .AND. (LOOP.NE.LOOP2))
 50         CONTINUE
         IF ((.NOT.MATCH) .AND. (NOUT.LT.MAXOUT)) THEN
C                                       Add to list
            NOUT = NOUT + 1
            IF (FOUT(LOOP).LT.0.0D0) THEN
C                                        Add USB
               FOUT(NOUT) = TF
            ELSE
C                                        Add LSB
               FOUT(NOUT) = -(FOUT(LOOP) - LSBOFF)
               END IF
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETSRC (XSRC)
C-----------------------------------------------------------------------
C   Set source selection criteria from the SOURCES adverb.
C
C   Inputs:
C      XSRC    H(4, 30)    SOURCES adverb
C
C   Outputs (in common):
C      SRCLST  C(30)*16    Source names from SOURCES adverb with any
C                           leading '-'s removed.
C      NUMSRC  I           Number of entries in SRCLST
C      DESEL   L           Deselection flag.  If true sources in
C                           SRCLST will not be selected
C-----------------------------------------------------------------------
      HOLLERITH XSRC(4, 30)
C
      CHARACTER SRCNAM*16
      INTEGER   I
C
      INCLUDE 'SRCSEL.INC'
C-----------------------------------------------------------------------
      NUMSRC = 0
      DESEL = .FALSE.
      DO 10 I = 1, 30
C                                       Decode source name
         CALL H2CHR (16, 1, XSRC(1, I), SRCNAM)
         IF (SRCNAM.NE.' ') THEN
C                                       Add source to source list and
C                                       update deselection flag.
            NUMSRC = NUMSRC + 1
            IF (SRCNAM(1:1).EQ.'-') THEN
               DESEL = .TRUE.
               SRCLST(NUMSRC) = SRCNAM(2:16)
            ELSE
               SRCLST(NUMSRC) = SRCNAM
               END IF
            END IF
   10    CONTINUE
C                                       If there are no sources in the
C                                       list then the sources in the
C                                       list are deselected (trivially)
      IF (NUMSRC.EQ.0) DESEL = .TRUE.
      END
      LOGICAL FUNCTION WNTSRC (SRCNAM)
C-----------------------------------------------------------------------
C   Return true if source SRCNAM is selected, false if it is not.
C
C   Inputs:
C      SRCNAM     C*(*)       Source name to be tested
C-----------------------------------------------------------------------
      CHARACTER SRCNAM*(*)
C
      INTEGER   I
      LOGICAL   FOUND
C
      INCLUDE 'SRCSEL.INC'
C-----------------------------------------------------------------------
      FOUND = .FALSE.
      DO 10 I = 1, NUMSRC
         IF (SRCNAM.EQ.SRCLST(I)) FOUND = .TRUE.
         IF (FOUND) GO TO 20
   10    CONTINUE
C
   20 CONTINUE
      IF (DESEL) THEN
         WNTSRC = .NOT.FOUND
      ELSE
         WNTSRC = FOUND
         END IF
C
      END
      SUBROUTINE CAF2UX (LINE,UNAME,PEXT,UROOTI,IRET)
C-----------------------------------------------------------------------
C   Converts a single Type 2 line of a UNIX-style A-file
C   to find out the unix filename (which is in the TAR header).
C   It returns other information as parent extent no and root id code
C   (and this is what we really need).
C   We care only for version 4 in the moment.
C   Input:
C      LINE     C*512    A single line from UNIX-style A-file
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C      UNAME    C*100    Complete UNIX-style filename
C      PEXT     I        Parent corel extent no
C      UROOTI   C*6      Root ID code
C-----------------------------------------------------------------------
      CHARACTER UEXTNO*2, USERNO*4, UTTAG*10
      CHARACTER UBSLN*3, UFREQ*4, UROOTI*6
      CHARACTER LINE*512, UNAME*100, LBLANK*50
C
      INTEGER   LEXTNO, LSERNO, LTTAG
      INTEGER   IRET,PEXT
      INTEGER   MAXI, PI(100), LI(100)
C
C-----------------------------------------------------------------------
      IRET = 0
      LBLANK = '                                '
C                                        Find pointers to data fields
      CALL UINDEX (LINE,MAXI,PI,LI)
C                                        Field #2 Root ID code
      READ (LINE(PI(2):),1000) UROOTI
C                                        Field #4 Extent no.
      READ (LINE(PI(4):),1010) UEXTNO
      LEXTNO = LI(4)
C                                        Field #8 Exp. serial no.
      READ (LINE(PI(8):),1020) USERNO
      LSERNO = LI(8)
C                                        Field #11 Time tag
      READ (LINE(PI(11):),1030) UTTAG
      LTTAG = LI(11)
C                                        Field #14 Baseline + QF
      READ (LINE(PI(14):),1040) UBSLN
C                                        Field #15 Frequency code etc
      READ (LINE(PI(15):),1050) UFREQ
C                                        Field #41 Parent corel extent
      READ (LINE(PI(41):),1060) PEXT
C                                        There can be more than one
C                                        parent, but we don't care
C                                        in the moment.
C
C                                Build up a valid UNIX-style filename
      UNAME=USERNO(1:LSERNO)//'/'//UTTAG(1:LTTAG)//'/'//
     *  UBSLN(1:2)//'.'//UFREQ(1:1)//'.'//
     *  UEXTNO(1:LEXTNO)//'.'//UROOTI//LBLANK//LBLANK
C-----------------------------------------------------------------------
1000  FORMAT (A6)
1010  FORMAT (A2)
1020  FORMAT (A4)
1030  FORMAT (A10)
1040  FORMAT (A3)
1050  FORMAT (A4)
1060  FORMAT (I2)
      RETURN
      END
      SUBROUTINE CUF2FT (FILNM, TYPE, EXTNO, IDROOT)
C-----------------------------------------------------------------------
C   Utility to find out what kind of file is described in
C   the TAR header, ordinary text files (Type 3 I assume)
C   or Type 50,51 or 52 files.
C   Supply additional information as the extent no and root id code.
C
C   The different file names in the header look like that:
C
C   Single short filename without dots or slashes     ---> ordinary file
C
C   Exp_No/   or   Exp_No/TimeTag/                    ---> directory path
C
C   Exp_No/TimeTag/Sourcename.RootId                  ---> Type 50
C
C   Exp_No/TimeTag/Baseline.ExtentNo.RootId           ---> Type 51
C
C   Exp_No/TimeTag/Baseline.FreqCode.ExtentNo.RootId  ---> Type 52
C
C   Input:   FILNM     Unix style filename
C   Output:  TYPE      Correllator file type
C            EXTNO     Extent no.
C            IDROOT    Root id code
C
C-----------------------------------------------------------------------
      CHARACTER FILNM*100, CTEMP*12, IDROOT*6
      INTEGER   TYPE, I, FOUND, EXTNO
      INTEGER   ISTART, IEND, ITEMP
C-----------------------------------------------------------------------
      I = 1
      FOUND = 0
      EXTNO = 0
      IDROOT = ' '
C                                First find out whether there is a `/`
C                                anywhere in the string. If not, it
C                                should be an ordinary file.
      DO 100 I=1,100
         IF (FILNM(I:I).EQ.'/') THEN
            FOUND = FOUND + 1
         END IF
  100 CONTINUE
C
      IF (FOUND.NE.0) GO TO 200
      TYPE = 3
      EXTNO = 0
      GO TO 900
C
  200 FOUND = 0
      DO 300 I=1,100
         IF (FILNM(I:I).EQ.'.') THEN
            FOUND = FOUND + 1
         END IF
  300 CONTINUE
C
      TYPE = FOUND + 49
C                         If there are only slashes and no dots
C                         we have a plain directory path
      IF (TYPE.EQ.49) THEN
         TYPE = 2
         EXTNO = 0
      END IF
C
      IF (TYPE.EQ.50) THEN
         EXTNO = 0
      END IF
C                          Find out extent no. and root id for Type 51
C                          file
      IF (TYPE.EQ.51) THEN
         DO 400 I=1,100
            IF (FILNM(I:I).EQ.'.') THEN
               ISTART = I
               GO TO 410
            END IF
400      CONTINUE
410      DO 420 I=ISTART+1,100
            IF (FILNM(I:I).EQ.'.') THEN
               IEND = I
               GO TO 430
            END IF
420      CONTINUE
430      CTEMP = FILNM((ISTART+1):(IEND-1))
         READ(CTEMP,440) EXTNO
440      FORMAT (I2)
         IDROOT = FILNM((IEND+1):(IEND+7))
      END IF
C                          Find out extent no. and root id for Type 52
C                          file
      IF (TYPE.EQ.52) THEN
         DO 500 I=1,100
            IF (FILNM(I:I).EQ.'.') THEN
               ITEMP = I
               GO TO 510
            END IF
500      CONTINUE
510      DO 520 I=ITEMP+1,100
            IF (FILNM(I:I).EQ.'.') THEN
               ISTART = I
               GO TO 530
            END IF
520      CONTINUE
530      DO 540 I=ISTART+1,100
            IF (FILNM(I:I).EQ.'.') THEN
               IEND = I
               GOTO 550
            END IF
540      CONTINUE
550      CTEMP = FILNM((ISTART+1):(IEND-1))
         READ (CTEMP,560) EXTNO
560      FORMAT (I2)
         IDROOT = FILNM((IEND+1):(IEND+7))
      END IF
C
  900 RETURN
      END
      SUBROUTINE CLENGT (OLENGT, TLENGT, XTRA)
C-----------------------------------------------------------------------
C     Utility to convert string describing an octal value (which is the
C     length of a file) to the number of data blocks ( 256 bytes each)
C     on tape. TAR writes only complete data blocks of 512 bytes on
C     tape, if the file is shorter, it is padded with blanks.
C     Unfortunately the correllator data is organized in blocks of 256
C     bytes and the input from tape is handled this way. So we could
C     round upwards from the file size to an even number of blocks.
C     We have a problem when the file length results in an odd number
C     of blocks of 256 bytes and the correllator cuts the file off
C     there. TAR then fills up the file with zeros.
C     In case of a type 51 file this leads to trouble, because
C     the last 'empty' block will be read and interpreted.
C     So we have to take care of this and give some sort
C     of signal to the calling procedure.
C     The best thing would be to compute the number of blocks,
C     odd or even, and a set a variable if there is an empty block
C     to fill up the TAR file.
C
C     Input :   OLENGT   CHAR   String of octal numbers
C     Output:   TLENGT   INT    number of blocks of 256 bytes
C               XTRA     INT    there is an empty block
C-----------------------------------------------------------------------
      INTEGER   TLENGT, ILENGT, IERR, XTRA
      REAL      REMAIN
      CHARACTER OLENGT*12
C-----------------------------------------------------------------------
      TLENGT = 0
      XTRA = 0
C                                Convert octal to integer
      CALL OCT2I (OLENGT,ILENGT,IERR)
C                                Integral part of filesize / 256
      TLENGT = ILENGT / 256
C                                If there is a remainder from division
      REMAIN = MOD (ILENGT , 256)
C                                we have one more full record
      IF (REMAIN.NE.0.) THEN
         TLENGT = TLENGT +1
      END IF
C                                In case TLENGT is odd, we have to add
C                                an empty block just for TAR
      REMAIN = MOD (TLENGT, 2)
      IF (REMAIN.NE.0.) THEN
         XTRA = 1
      ELSE
         XTRA = 0
      END IF
C                                If the size is zero - means directory
C                                path there are no records at all
      IF (ILENGT.EQ.0) THEN
         TLENGT = 0
         XTRA = 0
      END IF
C
      RETURN
      END
      SUBROUTINE TTSKIP (TLENGT, XTRA, TERR)
C-----------------------------------------------------------------------
C     Routine to skip a single file within an Tape ARchive. Since
C     TAR is one big file containing a huge number of small files
C     only separated by the headers (not any EOF's) we have to read
C     the header, find out the length of the file. Then we read the
C     data blocks. If we need the file, we process data, if the file
C     is skipped, we just throw the data away. But we have to read
C     all data anyway.
C     When skipping records we have to use the same buffer as ATREC
C     does to keep track of the file.
C
C     Input:   TLENGT    Size of file in blocks of 256 bytes
C              XTRA      There is an empty block to fill up TAR file
C     Output:  TERR      Tape error from TAPIO
C-----------------------------------------------------------------------
      INCLUDE 'MKOTH.INC'
C
      INTEGER TLENGT, TERR, RECO, XTRA
C-----------------------------------------------------------------------
C                                     A directory path is just a header
C                                     without any data blocks
      IF (TLENGT.EQ.0) GO TO 900
C                                     otherwise read all the blocks
C                                     and just forget them
      DO 100 RECO = 1,TLENGT,1
C
         CALL TAPIO('READ',FDVEC,TBUFF,TBIND,TERR)
C
         IF (TERR.NE.0) GO TO 900
100   CONTINUE
C                                     skip empty block at end
      IF (XTRA.EQ.1) THEN
         CALL TAPIO('READ',FDVEC,TBUFF,TBIND,TERR)
      END IF
C                                     update file counter
900   MKFILE = MKFILE + 1
      RETURN
      END
      SUBROUTINE CU2DBL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C     Replacement for ZDHPRL
C     On Unix TAR's of correlator data the HP 64-bit floating format is
C     no longer used.  Now values on tape are 64-bit IEEE, which has to
C     be copied with translation from the input to output buffers
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      DOUBLE PRECISION INB(*),OUTB(*)
C-----------------------------------------------------------------------
      CALL ZR64RL (NVAL, NP, INB, OUTB)
C
 999  RETURN
      END
      SUBROUTINE UINDEX (LINE,MAXI,PI,LI)
C-----------------------------------------------------------------------
C     Utility to parse a line of text containing data fields.
C     Returns the total number of data items, pointers to the
C     single items, and the length of the data field (which is
C     handled as a string here).
C     We assume that the first character in line is non-blank !
C
C     Input:  CHARACTER   LINE*512   A single line of text
C     Output: INTEGER     MAXI       Total number of data items
C             INTEGER     PI(*)      Pointer to data field
C             INTEGER     LI(*)      Length of according data field
C-----------------------------------------------------------------------
      CHARACTER LINE*512, CTEMP
      INTEGER   MAXI, PI(*), LI(*)
      INTEGER   MAXL, I, PTR1, PTR2
C-----------------------------------------------------------------------
C                                      Initialize
      MAXL = 512
      PTR1 = 1
      I    = 0
C                                      Next data item
 50   I = I + 1
      PTR2 = 0
      PI(I) = PTR1
C                                      Read a single char from line
100   CTEMP = LINE (PTR1:)
      PTR1 = PTR1 + 1
      PTR2 = PTR2 + 1
C                                      Blank means end of string
      IF (CTEMP.NE.' ') GO TO 100
      LI(I) = PTR2 - 1
C                                      End of line ?
200   IF (PTR1.GE.MAXL) GO TO 300
C                                      There could be contiguos blanks
C                                      between data fields
      CTEMP = LINE (PTR1:)
      PTR1 = PTR1 + 1
C                                      Non-blank means next data item
      IF (CTEMP.EQ.' ') GO TO 200
C                                      Adjust pointer to next data
      PTR1 = PTR1 - 1
      GO TO 50
C                                      Found last data item
300   MAXI = I
C
      RETURN
      END
      SUBROUTINE OCT2I (HVAL, IVAL, IERR)
C-----------------------------------------------------------------------
C! Compute the decimal equivalent of an octal (base 8) number
C-----------------------------------------------------------------------
C   Input:
C      HVAL   C*(*)   String containing octal value.  May contain
C                     leading or trailing blanks or tabs.
C   Outputs:
C      IVAL   I       Decimal equivalent of HVAL.
C      IERR   I       Error status, 0: success
C                        1: illegal octal digit
C-----------------------------------------------------------------------
      INTEGER   IERR, IVAL
      CHARACTER HVAL*(*)
C
      INTEGER   J, K, J1, J2
      CHARACTER OC*1, OCTC*8
C
      INTEGER   JTRIM
      EXTERNAL  JTRIM
      DATA OCTC /'01234567'/
C-----------------------------------------------------------------------
C                                       Initialize
      IERR = 0
      CALL TXTLEN (HVAL(1:JTRIM (HVAL)), J1, J2)
C                                       Process the octal digits
C                                       from left to right
      IVAL = 0
      DO 20 J = J1,J2
         IVAL = 8 * IVAL
         OC = HVAL(J:J)
         K = INDEX (OCTC, OC)
C                                       Illegal character
         IF (K.LT.1) THEN
            IVAL = -1
            IERR = 1
            GO TO 999
         ELSE
            IVAL = IVAL + (K-1)
            END IF
 20      CONTINUE
C
 999  RETURN
      END
