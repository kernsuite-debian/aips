LOCAL INCLUDE 'FILLM.INC'
C                                       Local include for FILLM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MXSOU, MXALL, MXFQE, MXSTRM, MXANTS
C                                       maximum antenna number
      PARAMETER (MXANTS=29)
C                                       MXSOU=max. no. source/qual/bandw
      PARAMETER (MXSOU=1000)
C                                       MXALL=max. allow output files.
      PARAMETER (MXALL=250)
C                                       MXSTRM=max. simultaneously open
C                                       streams
      PARAMETER (MXSTRM=8)
C                                       MXFQE=max no FQ rows
      PARAMETER (MXFQE=250)
C
      INTEGER   ITAPE, NFILES, JQUAL, DISKO(MXSTRM), CNOOUT(MXSTRM),
     *   SEQOUT(MXSTRM), CATOUT(256,MXSTRM), LRECO(MXSTRM),
     *   FILSIZ(MXSTRM), NUMHIS, JBUFSZ, BIF, EIF, KLOCWT,
     *   CURSOU(MXSTRM), NSOUR(MXSTRM), SOURID(MXSOU,MXSTRM),
     *   NXBUFF(512), SUBUFF(512), CLBUFF(1024), TYBUFF(512),
     *   NXKOLS(MAXNXC), NXNUMV(MAXNXC), INOGRP, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), CLKOLS(MAXCLC), CLNUMV(MAXCLC), TYKOLS(MAXTYC),
     *   TYNUMV(MAXTYC), KINCS(MXSTRM), KINCF(MXSTRM), KINCIF(MXSTRM),
     *   SUQUAL(MXSOU,MXSTRM), ALLNO, ALLDSK(MXALL), ALLCNO(MXALL),
     *   ALLNCH(MXALL), ALLNIF(MXALL), ALLNST(MXALL), ALLSTK(MXALL),
     *   ALLCHS(MXALL), ALLOBC(MXALL), ALLTYP(MXALL), OTBUFF(512),
     *   BUF256(256), OLDSOU(MXSTRM), OLDFQI(MXSTRM), NWRITN,
     *   BADNOM(4,MXANTS), BADNMS, ALLZMD(MXALL), ALLCCO(MXALL), HINCPB
      LOGICAL   DOUVCM, SOLAR, ISSUN, ALLMOD(MXALL), ONLINE, DOQUIT,
     *   DOOTT, DOMANY, DOBREK, DOSTOP, GOTCHA, INORDR, SUMCH0, DOCORD,
     *   SUMOVE(MXSOU,MXSTRM), IGQUAL, ISCORC, RICKS
      INTEGER   INXRNO, ISURNO, ICLRNO, ITYRNO, IFQRNO
      HOLLERITH XBAND, XINFIL(12), XNAMOU(3), XVLAOB(2), XVLAMO,
     *   XREFDA(2), XCALCO, XIN2FL(12)
      REAL      XTAPE, XNFILE, QUAL, XTR(8), XBCH, XECH, XSOUT, XDISO,
     *   XDOUVC, XDOCON, XNCUNT, XDOWT, XDOACR, CPARM(10), DPARM(10),
     *   BPARM(10)
      REAL      PRTIME, BANDW, TSTART, TEND, TIMCLI, TIMNCL, TIMTYI,
     *   TIMNTY, TIMWXI, TIMNWX, TIMPOI, TIMNPO, TIMOFI, TIMNOF,
     *   BUFFER(UVBFSS,MXSTRM), ALLBW(MXALL), ALLRFC(MXALL)
      DOUBLE PRECISION SUFREQ(2,MXSOU,MXSTRM), DPDAY, ALLFRQ(MXALL),
     *   JDTELL, JDTINC, SUPOS(2,MXSOU,MXSTRM), ALLXYZ(3,MXANTS,MXALL)
      CHARACTER BAND*4, NAMOUT*12, VLAOBS*6, VLAMOD*2, REFDAY*8,
     *   CLAOUT(MXSTRM)*6, OBSR*8, OBSDAT*8, VELTYP*8, VELDEF*8,
     *   CALKOD*4, HISCRD(10)*72, SULIST(MXSOU,MXSTRM)*16, JBAND*2,
     *   SFILE(MXSTRM)*48, SELCOD*4, ALLBAN(MXALL)*1, ALLCM(MXALL)*4,
     *   ALLIFS(MXALL)*4, INFILE*48, IN2FIL*48, ALLANN(MXANTS,MXALL)*8,
     *   HICORM*4, BNDCOD(MXFQE,2,MXSTRM)*8
      INTEGER   CURFQI(MXSTRM), FQBUFF(512), FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC)
      REAL      CHBW(MXFQE,2,MXSTRM), TOTBW(MXFQE,2,MXSTRM)
      DOUBLE PRECISION CATFRQ(MXSTRM), IFFREQ(MXFQE,2,MXSTRM)
      COMMON /BUFRS/ BUFFER, JBUFSZ, BUF256
      COMMON /INPARM/ XTAPE, XINFIL, XNFILE, XBAND, QUAL, XCALCO,XVLAOB,
     *   XVLAMO, XREFDA, XTR, XBCH, XECH, XNAMOU, XSOUT, XDISO, XDOUVC,
     *   XDOCON, XNCUNT, XDOWT, XDOACR, CPARM, DPARM, BPARM, XIN2FL
      COMMON /EXTRA/ BANDW, ITAPE, NFILES, JQUAL, SEQOUT, DISKO, CNOOUT,
     *   BIF, EIF, LRECO, FILSIZ, NUMHIS, DOUVCM, SOLAR, ISSUN, TSTART,
     *   TEND, TIMCLI, TIMNCL, TIMTYI, TIMNTY, TIMWXI, TIMNWX, TIMPOI,
     *   TIMNPO, TIMOFI, TIMNOF, ONLINE, DOQUIT, DOOTT, DOMANY, DOBREK,
     *   DOSTOP, GOTCHA, INORDR, PRTIME, SUMCH0, DOCORD, SUMOVE, IGQUAL,
     *   ISCORC, RICKS, HINCPB
      COMMON /SNFINF/ CATOUT, SUFREQ, SUPOS, DPDAY, INXRNO, ISURNO,
     *   ICLRNO, ITYRNO, IFQRNO, CURSOU, NSOUR, SOURID, NXBUFF, SUBUFF,
     *   CLBUFF, TYBUFF, NXKOLS, NXNUMV, INOGRP, SUKOLS, SUNUMV, CLKOLS,
     *   CLNUMV, TYKOLS, TYNUMV, KLOCWT, KINCS, KINCF, KINCIF, SUQUAL,
     *   OTBUFF, OLDSOU, OLDFQI, NWRITN, BADNOM, BADNMS
      COMMON /ALLCOM/ ALLFRQ, ALLXYZ, ALLBW, ALLRFC, ALLNO, ALLDSK,
     *   ALLCNO, ALLNCH, ALLNIF, ALLNST, ALLSTK, ALLCHS, ALLOBC, ALLMOD,
     *   ALLTYP, ALLZMD, ALLCCO
      COMMON /CHRCOM/ BAND, NAMOUT, VLAOBS, VLAMOD, REFDAY, CLAOUT,
     *   OBSR, OBSDAT, VELTYP, VELDEF, CALKOD, HISCRD, SULIST, JBAND,
     *   SFILE, ALLBAN, ALLCM, ALLIFS, SELCOD, INFILE, IN2FIL, ALLANN,
     *   HICORM, BNDCOD
      COMMON /FQINF/ JDTELL, JDTINC, CATFRQ, IFFREQ, CHBW, TOTBW,
     *   CURFQI, FQBUFF, FQKOLS, FQNUMV
C                                                          End FILLM
LOCAL END
LOCAL INCLUDE 'MC2.INC'
C                                                          Include MC2.
C                                       Local include for MODCOMP VLA
      INTEGER   TBSIZE, PELIM, MXSTRE, NCOUNT, MXANT, NOBAND
C                                       maximum antenna number
      PARAMETER (MXANT=29)
C                                       TBSIZE = tape buffer size
      PARAMETER (TBSIZE = 28000)
C                                       PELIM Limit for Tape read errors
C                                       MXSTRE=max. simultaneously open
C                                       streams.  This MUST be the same
C                                       as MXSTRM in FILLM.INC
      PARAMETER (MXSTRE=8)
C                                       number recognized bands
      PARAMETER (NOBAND=10)
C
      INTEGER   MCLNF1, NOFILE, IFSLIM
      INTEGER   PECNT, MCWLEF, WTSYS
      LOGICAL   PASFLG, DEFTY, ALLREF, ISEOF, NEWFIL, STRTAB, MODCHN,
     *   SUBMCH, NOWARN, DEFSOL, VLECK, TVLK, INVRT, DOPTNG, DOTIP,
     *   ISEOT, DOACOR, DODISK, BAIL, EVLA(MXANT)
C                                       Control info, tape buffers
      INTEGER   TAPBSZ, TAPIND, FDVEC(50), TAPBUF(TBSIZE)
      INTEGER   NSTREM, LENBAS, STLEN(MXSTRE), STPNT(MXSTRE),
     *   STMCI1(MXSTRE), STMCI2(MXSTRE), STNOIF(MXSTRE), STNOCH(MXSTRE),
     *   STNOPL(MXSTRE), STBCH(MXSTRE), STECH(MXSTRE), STOFFS(MXSTRE),
     *   STOFFF(MXSTRE), STOFIF(MXSTRE), STCDA(MXSTRE),
     *   STCOFF(MXSTRE,2), STTYPE(MXSTRE), TMCBCH, TMCECH, OSTREM,
     *   MCMODE, ANTBAS(500,2), OSTNIF(MXSTRE), OSTNPL(MXSTRE),
     *   STZMOD(MXSTRE), OLDOFF, GNTYP, OPACIT, BADEXP(MXANT), BADEXS
      LOGICAL   DOAC(MXSTRE), DOCH0(4), MCINIT, GOTHED, IFFLAG(4,MXANT),
     *   MCANTS(MXANT), MCHOLO, MCPTNG, MCTIP, NEEDGN, ONSHAD, WANTOF,
     *   PASPTG, PASOTH, ZSPEC
      REAL      SHADOW, SELECT(11), MCDATA(602112), TBUFF(2048), TCORR,
     *   CGN(8,2,MXANT,MXANT), ZOPAC, WTOPAC, AGAINS(NOBAND,4,MXANT+1)
      CHARACTER SELBAN*1, SELBA2*1, SELPGM*6, SELMOD*2,
     *   STIFNA(MXSTRE)*4
      DOUBLE PRECISION STFREQ(MXSTRE), STIAT, FRESEL(2,4), AVRAAP,
     *   AVDEAP
C                                       Header information
      INTEGER   MCBLK, MCNBLK, MCFMT, MCFREV, MCLADA, MCNANT,
     *   MCLCDA(2,4), MCNBPR, MCSAID, MCQUAL, MCUSID, MCNCPB,
     *   MCINTG, MCBCOD, MCFEFC, MCRCC, MCEPOC, MCCHOF(4), MCHISP(4),
     *   MCANPP(4,MXANT), MCAORD(MXANT), DAYOFF, MCLPR(1100), TSDATE,
     *   OLDORD(MXANT), MCIFCB(4,MXANT)
      INTEGER   MCLRL, MCDATE, MCIATC, MCSDA, MCADA, MCCDA(4), MCACB,
     *   MCANCB(MXANT), MCCHAN(4), AVANCB(MXANT), MCSTAT(MXANT),
     *   AVSTAT(MXANT)
      REAL      MCLSTE, MCLSTB, MCSR, MCZAPP, MCSCOS(6), MCZSF,
     *   MCUVLM(2), MCWEAT(5), MCANNS(4,MXANT), MCANTF(4,MXANT),
     *   MCANTB(4,MXANT), MCANPD(4,MXANT), MCAUVW(3,MXANT), MCFEFW(4),
     *   MCANBA(MXANT), AVGUVW(3,MXANT), SUMINT, MCBANW(4), MCHSEP(4),
     *   AVTSYS(2,2,MXANT), AVTANT(2,2,MXANT), AVWEAT(5), AVSCOS(2)
      CHARACTER MCCPID*8, MCSNAM*16, MCARRC*2, MCPGID*6, MCOMOD*2,
     *   MCALCD*2, MCVRF*8, MCCORM*4, MCAPOP*4, BANDAC*2, BANDBD*2,
     *   MCANTN(MXANT)*8, MCRCV*2
      DOUBLE PRECISION MCRAEP, MCDCEP, MCRAAP, MCDCAP, MCSSLO(4),
     *   MCSKYF(4), MCIATI, MCLSTI, MCIATG, MCRVEL(4), MCREST(4),
     *   MCANTD(MXANT), MCAXYZ(3,MXANT), REFMJD, AVGIAT, EOSTIM,
     *   TSIAT0, STRXYZ(3,MXANT), TOLERB, AVANTD(MXANT)
      COMMON /MODCOM/ PECNT, PELIM, MCWLEF, PASFLG, DEFTY, ALLREF,
     *   ISEOF, NEWFIL, STRTAB, MCLNF1, NOFILE, IFSLIM, MODCHN, SUBMCH,
     *   NOWARN, WTSYS, DEFSOL, VLECK, TVLK, INVRT, DOPTNG, DOTIP,
     *   ISEOT, DOACOR, DODISK, BADEXP, BADEXS, BAIL
      COMMON /MC2COM/ STFREQ, STIAT, FRESEL, CGN, TBUFF, SHADOW, TCORR,
     *   SELECT, MCDATA, NSTREM, LENBAS, STLEN, STPNT, STMCI1, STMCI2,
     *   STNOIF, STNOCH, STNOPL, STBCH, STECH, STOFFS, STOFFF, STOFIF,
     *   STCDA, STCOFF, STTYPE, TMCBCH, TMCECH, MCMODE, ANTBAS,
     *   OSTREM, OSTNIF, OSTNPL, OLDOFF, DOAC, DOCH0, MCINIT, GOTHED,
     *   IFFLAG, ONSHAD, WANTOF, PASPTG, PASOTH, TAPBSZ, TAPIND, FDVEC,
     *   TAPBUF, ZOPAC, WTOPAC, AGAINS, GNTYP, OPACIT, NEEDGN, NCOUNT,
     *   ZSPEC, STZMOD
      COMMON /MC2CHR/ STIFNA
C                                       Tape header info. Grouped by
C                                       type to allow easy access
C                                       through pointers in MCH1
C
C                                       double precision variables
      COMMON /MCHDR/ MCRAEP, MCDCEP, MCRAAP, MCDCAP, MCSSLO, MCSKYF,
     *   MCIATI, MCLSTI, MCIATG, MCRVEL, MCREST, MCANTD, MCAXYZ, REFMJD,
     *   AVGIAT, TSIAT0, STRXYZ, TOLERB,
C                                       REAL*4 variables
     *   MCLSTE, MCLSTB, MCSR, MCZAPP, MCSCOS, MCZSF, MCUVLM, MCWEAT,
     *   MCANNS, MCANTF, MCANTB, MCANPD, MCAUVW, MCANBA, AVGUVW, SUMINT,
     *   MCBANW, MCHSEP, MCLRL, MCFEFW,
C                                       long integer variables
     *   MCDATE, MCIATC, MCSDA, MCADA, MCCDA, MCACB, MCANCB, MCBLK,
     *   MCNBLK,
C                                       short integer variables
     *   MCFMT, MCFREV, MCLADA, MCNANT, MCLCDA, MCNBPR, MCSAID, MCQUAL,
     *   MCUSID, MCNCPB, MCINTG, MCBCOD, MCFEFC, MCRCC, MCEPOC,
     *   MCCHOF, MCHISP, MCANPP, MCIFCB,
     *   MCAORD, MCANTS, MCCHAN, DAYOFF, MCHOLO, MCPTNG, MCTIP, MCLPR,
     *   OLDORD, TSDATE
      COMMON /MODCHR/ MCCPID, MCSNAM, MCARRC, MCPGID, MCOMOD, MCALCD,
     *   MCVRF, MCCORM, MCAPOP, BANDAC, BANDBD, SELBAN, SELBA2, SELPGM,
     *   SELMOD, MCANTN, MCRCV
      COMMON /SCNTIM/ EOSTIM
      COMMON /MOREHD/ AVANTD, AVRAAP, AVDEAP, AVTANT, AVTSYS, AVWEAT,
     *   AVANCB, AVSCOS, MCSTAT, AVSTAT, EVLA
C                                                          End MC2.
LOCAL END
LOCAL INCLUDE 'MCB.INC'
      CHARACTER MCRCVX*2
      COMMON /MCBCOM/ MCRCVX
LOCAL END
      PROGRAM FILLM
C-----------------------------------------------------------------------
C! Reads VLA Modcomp archive tapes
C# Tape UV VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2010, 2012-2013, 2015-2016
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
C   FILLM copies selected data from a VLA Modcomp archive tape to an
C   AIPS uv database.
C   Version for data written after January 1988
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INTAPE         ITAPE         Tape drive number.
C      NFILES         NFILES        Number of files to skip.
C      BAND           JBAND         Frequency band code (L,C,U,K)
C      QUAL           JQUAL         Source qualifier, -1=>all
C      CALCODE        SELCOD        Calibrator select code ' ' => all
C      VLAOBS         VLAOBS        VLA Observing prgm name. eg (AJ99)
C                                   6 characters
C      VLAMODE        VLAMOD        VLA Observing mode '  '=>normal
C      TIMERANG       SELECT(9&10)  Timerange desired.
C      BCHAN          MCBCH         Start channel
C      ECHAN          MCECH         Highest channel
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is reference date.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      NCOUNT                       Number of files to read
C      CPARM(10)      CPARM         User specified array.
C                                   1 => Avg. time (seconds)
C                                   2 >0 => ignore Modcomp flags
C                                   4 => shadow flagging limit
C                                   5 => # channels requested.
C                                   6 => Subarray number 0=>any.
C                                   7 => FQ entry tolerance.
C                                   8 => CL table incr. (def=5)
C                                   9 => TY table increment
C                                  10 => Cal. Avg. time (sec)
C      DPARM(10)      DPARM         Frequency selection array
C-----------------------------------------------------------------------
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C
      CHARACTER PRGM*6
      INTEGER   IRET, I, J
C
      DATA PRGM /'FILLM '/
C-----------------------------------------------------------------------
C                                       Limit for Tape read errors
      PELIM = 50
      CALL FILL (MXANT, 0, BADEXP)
      BADEXS = 0
C                                       Get input parameters.
      CALL FLMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read tape.
      CALL FLMUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       summary bad nominals
      IF (BADNMS.GT.0) THEN
         DO 20 J = 1,MXANTS
            DO 10 I = 1,4
               IF (BADNOM(I,J).GT.0) THEN
                  WRITE (MSGTXT,1000) J, I, BADNOM(I,J)
                  CALL MSGWRT (6)
                  END IF
 10            CONTINUE
 20         CONTINUE
         END IF
C                                       summary bad nominals
      IF (BADEXS.GT.0) THEN
         DO 30 J = 1,MXANTS
            IF (BADEXP(J).GT.0) THEN
               WRITE (MSGTXT,1020) J, BADEXP(J)
               CALL MSGWRT (6)
               END IF
 30         CONTINUE
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUF256)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Number bad nominal sensitivities: ant',I3,' IF',I2,' =',
     *   I10)
 1020 FORMAT ('Number bad exponents for antenna',I3,' was',I10)
      END
      SUBROUTINE FLMIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   FLMIN gets input parameters for FILLM and other setup.
C   Inputs:
C      PRGN     C*6      Program name
C   Output:
C      JERR     I        Error code: 0 => ok
C                                    4 => error creating output file
C                                    8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ output file catalog header
C   See prologue comments in FILLM for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*(*)
      INTEGER   JERR
C
      INTEGER   IERR, NPARM, IROUND, IL, ID(6), I, J, K
      CHARACTER LNAME*6, XLATED*128
      LOGICAL   ACTION(12)
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'MCB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Buffer size
      JBUFSZ = UVBFSS * 2
      NWRITN = 0
      BADNMS = 0
      I = 4 * MXANTS
      CALL FILL (I, 0, BADNOM)
      NUMHIS = 0
      PECNT = 0
      INORDR = .TRUE.
      PRTIME = -1.E10
      HICORM = ' '
      HINCPB = -1000000
      MCRCVX = ' '
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      CALL FILL (MXSTRM, 0, NSOUR)
C                                       Get input parameters.
      NPARM = 84
      CALL GTPARM (PRGN, NPARM, RQUICK, XTAPE, BUF256, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUF256, IERR)
      IF (JERR.NE.0) GO TO 999
      MSGTXT = '******************************************************'
      CALL MSGWRT (4)
      MSGTXT = '**   Times written by FILLM are now centered in     **'
      CALL MSGWRT (4)
      MSGTXT = '**   the integration, before they were at the end   **'
      CALL MSGWRT (4)
      MSGTXT = '******************************************************'
      CALL MSGWRT (4)
C                                       Crunch input parameters.
      ITAPE = IROUND (XTAPE)
      ITAPE = MAX (1, ITAPE)
      NFILES = IROUND (XNFILE)
      JQUAL = IROUND (QUAL)
      NCOUNT = IROUND (XNCUNT)
      IF (NCOUNT.LE.0) NCOUNT = 999
C                                       Convert characters
      CALL H2CHR (48, 1, XINFIL, INFILE)
      DODISK = INFILE.NE.' '
      CALL H2CHR (48, 1, XIN2FL, IN2FIL)
      CALL H2CHR (4, 1, XBAND, BAND)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (4, 1, XCALCO, SELCOD)
      CALL H2CHR (6, 1, XVLAOB, VLAOBS)
      CALL H2CHR (2, 1, XVLAMO, VLAMOD)
      CALL H2CHR (8, 1, XREFDA, REFDAY)
      DOACOR = XDOACR.GT.0.0
C                                       Check for valid REFDAY
      IF (REFDAY.NE.' ') THEN
         CALL JULDAY (REFDAY, REFMJD)
         IF (REFMJD.LE.0.) THEN
            WRITE (MSGTXT,1010) REFDAY
            CALL MSGWRT (6)
            REFDAY = ' '
            REFMJD = -999D0
            END IF
         CALL GREG (REFMJD, REFDAY)
         REFMJD = REFMJD - 2400000.5D0
      ELSE
         REFMJD = -999D0
         END IF
C
      DOUVCM = XDOUVC.GT.0.0
      SOLAR = VLAMOD(1:1).EQ.'S'
C                                       Give message if compressed UV
      IF (DOUVCM) THEN
         MSGTXT = 'UV data will be written in compressed format'
         CALL MSGWRT (4)
         END IF
C                                       Set CPARM defaults
C                                       First see if OTT wanted.
      DOOTT = (CPARM(1).GE.1.0E5)
      IF (DOOTT) CPARM(1) = CPARM(1) - 1.0E5
      IF (CPARM(1).LE.1.0E-4) CPARM(1) = 1.0E-4
C                                       Van Vleck correction
C                                       if CPARM(6) >= 8
C                                       inverse if  <= -8
      VLECK = .FALSE.
      TVLK  = .FALSE.
      INVRT = CPARM(6).LT.0.0
      CPARM(6) = ABS (CPARM(6))
      I = INT (CPARM(6))
      K = INT (I/8)
      IF (K.GE.1) THEN
         CPARM(6) = CPARM(6) - FLOAT (K * 8)
         VLECK = .TRUE.
         MSGTXT = 'will apply Van Vleck correction on continuum data'
         CALL MSGWRT (4)
         TVLK = K.GE.2
         END IF
C                                       CL table increment
      IF (CPARM(8).LE.1.0E-10) CPARM(8) = 5.0
C                                       Cal integration time = source
      IF (CPARM(10).LE.1.0E-10) CPARM(10) = CPARM(1)
C                                       Decode CPARM(2)-five LSB's
      I = INT (CPARM(2))
      DO 20 J = 1,12
         K = INT (I/2)
         ACTION(J) = K*2.NE.I
         I = K
 20      CONTINUE
      PASFLG = ACTION(1)
      DEFTY  = .NOT.ACTION(2)
      ALLREF = ACTION(3)
      WTSYS = 0
      IF ((ACTION(4)) .OR. (XDOWT.GT.0.0)) WTSYS = 1
      IF (ABS(XDOWT-10.0).LE.0.1) WTSYS = 2
      DOCORD = .NOT.ACTION(5)
      DOPTNG = ACTION(6)
      DOTIP = ACTION(11)
      SUMCH0 = .NOT.ACTION(7)
      RICKS = ACTION(12)
C                                       Undocumented on purpose: add 128
C                                       to CPARM(2) to force non-solar
C                                       treatment
      DEFSOL = .NOT.ACTION(8)
      IGQUAL = ACTION(9)
C                                       Load bad pointing data?
      PASPTG = .NOT.ACTION(10)
C                                       Load seriously flagged data?
C                                       Disabled.  should be covered
C                                       by the IF status flags.  bjb.
C     PASOTH = ACTION(10)
      PASOTH = .TRUE.
C                                       Set shadowing limit in meters
C                                       and tell user what happened
      SHADOW = CPARM(4)
      IF (SHADOW.LE.-1.0E-5) THEN
         SHADOW = 0.0
         ONSHAD = .FALSE.
      ELSE IF (SHADOW.LT.1.0E-5) THEN
         SHADOW = 25.0
         ONSHAD = .TRUE.
      ELSE
         ONSHAD = .FALSE.
         END IF
      WRITE (MSGTXT,1020) SHADOW
      CALL MSGWRT (4)
      I = IROUND (CPARM(3))
      IF ((I.GE.0) .AND. (I.LE.3)) I = 3
      IF ((I.LT.0) .AND. (I.GE.-3)) I = -3
      CPARM(3) = I
C     WANTOF = (SHADOW.LT.24.99) .OR. (ACTION(1)) .OR. (ACTION(13)) .OR.
C    *   (ABS(CPARM(3)).GT.3)
      WANTOF = .TRUE.
C                                       Convert SHADOW to nsec
      IF (SHADOW.GT.0.0) SHADOW = 1.0D9 * SHADOW / VELITE
C
C                                       Set DPARM defaults
C                                       C Freq. => A
      IF (ABS (DPARM(5)+DPARM(6)).LE.1.0E-10) THEN
         DPARM(5) = DPARM(1)
         DPARM(6) = DPARM(2)
         END IF
C                                       D Freq. => B
      IF (ABS (DPARM(7)+DPARM(8)).LE.1.0E-10) THEN
         DPARM(7) = DPARM(3)
         DPARM(8) = DPARM(4)
         END IF
C                                       Be careful with tolerances
      IF (ABS(DPARM(1)+DPARM(2)).LE.1.0E-10) DPARM(9) = 1.0E25
      IF (ABS(DPARM(3)+DPARM(4)).LE.1.0E-10) DPARM(10) = 1.0E25
C                                       Tolerance = all
      IF (DPARM(9).LE.1.0E-10) DPARM(9) = 1.0E25
      IF (DPARM(10).LE.1.0E-10) DPARM(10) = 1.0E25
C                                       Opacity & gain curve.
      IF (BPARM(1).EQ.0.0) BPARM(1) = 20.
      IF (BPARM(10).EQ.0.0) BPARM(10) = 0.5
      BPARM(10) = MAX (0.0, MIN (1.0, BPARM(10)))
      IF (BPARM(1).LT.0.0) THEN
         OPACIT = 0
         MSGTXT = 'No opacity correction in CL table.'
      ELSE IF (BPARM(1).LE.10.0) THEN
         OPACIT = 1
         ZOPAC = BPARM(1)
         WRITE (MSGTXT,1025) ZOPAC
      ELSE
         OPACIT = 2
         WTOPAC = MIN (1.0, MAX (0.0, BPARM(10)))
         MSGTXT = 'Opacity correction in CL table weighted average of'//
     *            ' weather and'
         CALL MSGWRT (3)
         WRITE (MSGTXT,1026) WTOPAC
         CALL MSGWRT (3)
         IF (BPARM(1).GE.100.0) THEN
            OPACIT =  OPACIT + 2
            MSGTXT = '**** Old opacity model is being used ***'
         ELSE
            MSGTXT = '**** New opacity model is being used ***'
            END IF
         END IF
      CALL MSGWRT (3)
      NEEDGN = .FALSE.
      I = NOBAND * 4 * (MXANT+1)
      CALL RFILL (I, 0.0, AGAINS)
      IF (BPARM(2).LT.0.0) THEN
         GNTYP = 0
         MSGTXT = 'No gain curve correction in CL table.'
      ELSE IF (BPARM(2).GE.3.0) THEN
         GNTYP = 3
         DO 30 I = 1,NOBAND
            DO 30 J = 1,4
               AGAINS(I,J,MXANT+1) = BPARM(J+2)
 30            CONTINUE
         MSGTXT = 'Gain curve correction in CL table based on ' //
     *            'user specified: '
         CALL MSGWRT (3)
         WRITE (MSGTXT,1030) (BPARM(J),J=3,6)
      ELSE IF (BPARM(2).GE.2.0) THEN
         GNTYP = 2
         NEEDGN = .TRUE.
         MSGTXT = 'Gain curve correction in CL table read from file,'
         CALL MSGWRT (3)
         MSGTXT = '   with variation as function of band only.'
      ELSE
         GNTYP = 1
         NEEDGN = .TRUE.
         MSGTXT = 'Gain curve correction in CL table read from file,'
         CALL MSGWRT (3)
         MSGTXT = '   with variation as function of antenna and band.'
         END IF
      CALL MSGWRT (3)
C                                       Watch number for # streams
C                                       and other things changing
      OSTREM = -1
      CALL FILL (MXSTRE, -1, OSTNIF)
      CALL FILL (MXSTRE, -1, OSTNPL)
C                                       Default antenna array
      CALL FILL (MXANT, 0, OLDORD)
C                                       Default bands
      BANDAC = ' '
      BANDBD = ' '
C                                       Are we On-Line version?
      IF (DODISK) THEN
         ONLINE = .FALSE.
      ELSE
         LNAME = 'AMT0x:'
         CALL ZEHEX (ITAPE, 1, LNAME(5:5))
         CALL ZTRLOG (6, LNAME, 128, XLATED, IL, IERR)
         IF ((IERR.NE.0) .OR. (IL.LT.7)) THEN
            ONLINE = .FALSE.
         ELSE
            CALL CHLTOU (7, XLATED)
            ONLINE = XLATED(1:7).EQ.'ON-LINE'
            END IF
         END IF
C                                       Allow higher TELL frequency
C                                       (JDTINC) when ONLINE.
      IF (ONLINE) THEN
         MSGTXT = 'Welcome to real-time FILLM!'
         CALL MSGWRT (3)
         XDOCON = -1.0
         XSOUT = 0.0
         PELIM = PELIM / 10
         JDTINC = 1.0D0 / (24.0D0 * 60.0D0) / 6.0
      ELSE
         JDTINC = 1.0D0 / (24.0D0 * 60.0D0)
         END IF
C                                       Set defaults.
      DOMANY = .FALSE.
      DOBREK = .FALSE.
      DOSTOP = .FALSE.
      DOQUIT = .FALSE.
C                                       Gotcha becomes true in MCWANT
C                                       whenever data are received with
C                                       the specified VLAOBS. This
C                                       enables FILLM to stop when the
C                                       code changes.
      GOTCHA = .FALSE.
C                                       No warning about subarrays has
C                                       yet been given.  (see MCWANT)
      NOWARN = .TRUE.
      CALL ZDATE (ID(1))
      CALL ZTIME (ID(4))
      CALL DAT2JD (ID, JDTELL)
C                                       Fill allowed output file table.
      CALL FLMCAT (JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMIN: Error ',I3,' obtaining input parameters')
 1010 FORMAT ('FLMIN: Invalid REFDATE "',A,'".  FILLM will use " ".')
 1020 FORMAT ('Shadow flag limit = ',1PE10.3,' meters.')
 1025 FORMAT ('Opacity correction in CL table based on user ',
     *   'specified: ',F5.3)
 1026 FORMAT ('   season.  Weight for weather = ',F4.2)
 1030 FORMAT (4(3X,F5.3))
      END
      SUBROUTINE FLMGN (IRET)
C-----------------------------------------------------------------------
C   Reads in gain curves from a file.
C   Input through common:
C      OBSDAT   C*8      Reference date
C   Output through common:
C      AGAINS   R(*)     Gain curve parameters
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DOUBLE PRECISION ODATE, VDATE(2)
      REAL      GPARMS(4)
      INTEGER   IBAND, IANT, II, LUNGN, INDGN, JT, JTRIM
      CHARACTER INLINE*80
C-----------------------------------------------------------------------
      IRET = 0
C
      CALL JULDAY (OBSDAT, ODATE)
      LUNGN = 10
      MSGTXT = 'Trying to access VLA antenna gains file...'
      CALL MSGWRT (3)
C                                       Try user file
      IF (IN2FIL.NE.' ') THEN
         CALL ZTXOPN ('READ', LUNGN, INDGN, IN2FIL, .FALSE., IRET)
         IF (IRET.EQ.0) THEN
            CALL ZTXIO ('READ', LUNGN, INDGN, INLINE, IRET)
            IF ((IRET.EQ.0) .AND. (INLINE(:20).EQ.
     *         '; ANTENNA-GAIN-TABLE')) GO TO 20
            CALL ZTXCLS (LUNGN, INDGN, IRET)
            END IF
         MSGTXT = 'IN2FIL not a valid gains file, trying standard one.'
         CALL MSGWRT (6)
         END IF
      IN2FIL = 'AIPSIONS:VLA.GAINS'
      CALL ZTXOPN ('READ', LUNGN, INDGN, IN2FIL, .FALSE., IRET)
      IF (IRET.EQ.0) THEN
         CALL ZTXIO ('READ', LUNGN, INDGN, INLINE, IRET)
         IF ((IRET.EQ.0) .AND. (INLINE(:20).EQ.'; ANTENNA-GAIN-TABLE'))
     *      GO TO 20
         CALL ZTXCLS (LUNGN, INDGN, IRET)
         END IF
      MSGTXT = 'Standard gains file not valid or failed!'
      CALL MSGWRT (6)
      IRET = 1
      GO TO 999
C                                       Continue reading
 20   CALL ZTXIO ('READ', LUNGN, INDGN, INLINE, IRET)
      IF (IRET.EQ.0) THEN
         JT = JTRIM (INLINE)
         IF (INLINE(:1).NE.';') THEN
            CALL PARSEL (INLINE, IBAND, IANT, GPARMS, VDATE)
            IF ((ODATE.GE.VDATE(1)) .AND. (ODATE.LE.VDATE(2))) THEN
               IF (IANT.EQ.0) IANT = MXANT + 1
               DO 25 II = 1,4
                  AGAINS(IBAND,II,IANT) = GPARMS(II)
 25               CONTINUE
               END IF
            END IF
         GO TO 20
         END IF
      CALL ZTXCLS (LUNGN, INDGN, II)
      IF (IRET.EQ.2) IRET = 0
C
 999  RETURN
      END
      SUBROUTINE PARSEL (INLINE, IBAND, IANT, GPARMS, VDATE)
C-----------------------------------------------------------------------
C   Parses a line from a gain curve file
C   Input:
C      INLINE   C*       Input line
C   Output:
C      IBAND    I        The "band number".
C      IANT     I        Antenna number
C      GPARMS   R(4)     Gain curve polynomial parameters
C      VDATE    D(2)     Julian date range over which these gain curve
C                        coefficients are valid
C-----------------------------------------------------------------------
      CHARACTER INLINE*80
      INTEGER   IBAND, IANT
      REAL      GPARMS(4)
      DOUBLE PRECISION VDATE(2)
C
      INTEGER   II, JJ, NOBAND
      PARAMETER (NOBAND=10)
      CHARACTER CBAND(NOBAND)*1
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
C
      DATA CBAND / '4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q'/
C-----------------------------------------------------------------------
C                                       get band
      II = 1
      IBAND = 0
      IANT = -1
      DO 10 II = 1,80
         IF (INLINE(II:II).NE.' ') GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Match band
 20   DO 25 JJ = 1,NOBAND
         IF (INLINE(II:II).EQ.CBAND(JJ)) IBAND = JJ
 25      CONTINUE
C                                       get antenna
      II = II + 2
      CALL GETNUM (INLINE, 80, II, X)
      IF (X.EQ.DBLANK) GO TO 999
      IANT = X + 0.01D0
C                                       get dates
      II = II + 1
      JJ = II
      DO 30 II = JJ,80
         IF (INLINE(II:II).NE.' ') GO TO 35
 30      CONTINUE
      VDATE(1) = 1.D10
      GO TO 999
 35   CALL JULDAY (INLINE(II:II+7), VDATE(1))
      II = II + 9
      JJ = II
      DO 40 II = JJ,80
         IF (INLINE(II:II).NE.' ') GO TO 45
 40      CONTINUE
      VDATE(2) = -1.D10
      GO TO 999
 45   CALL JULDAY (INLINE(II:II+7), VDATE(2))
      II = II + 9
C                                       get parms
      DO 50 JJ = 1,4
         CALL GETNUM (INLINE, 80, II, X)
         IF (X.NE.DBLANK) GPARMS(JJ) = X
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FLMCAT (IRET)
C-----------------------------------------------------------------------
C   Fills in the list of allowable output files.
C   Input from common:
C      XDOCON   R        If > 0.0 then allow appending to old file
C      XDOUVC   R        If > 0.0 then compressed format
C      XDISO    R        Requested output disk (0=> any)
C      XSOUT    R        Requested output seq. (0=> any)
C      NAMOUT   C*12     Requested output name
C      BAND     C*4      Requested band code
C      DPARM    R        Frequency selection array.
C   Output in common:
C      ALLNO    I        Number of allowed output files
C      ALLDSK   I(*)     Output file disk numbers
C      ALLCNO   I(*)     Output file cat. slot numbers
C      ALLBAN   C(*)*1   Output file band code
C      ALLCM    C(*)*4   Output file correlator mode
C      ALLFRQ   D(*)     Output file reference frequency (Hz)
C      ALLBW    R(*)     Output file reference Bandwidth (ch. sep) (Hz)
C      ALLRFC   R(*)     Output file reference channel
C      ALLNCH   I(*)     Output file no. channels
C      ALLCHS   I(*)     Output channels in line data (for ch0 files)
C      ALLNIF   I(*)     Output file no. IFs
C      ALLNST   I(*)     Output file no. Stokes'
C      ALLSTK   I(*)     Output file first stokes type
C      ALLMOD   L(*)     Output file modified flag
C      ALLIFS   C(*)*4   Output file IF names
C      ALLOBC   I(*)     Output file - no. channels in original data
C      ALLTYP   I(*)     Output file data type:  0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      ALLZMD   I(*)     Output file in Z mode (ch 0 part of spectrum)
C      ALLCCO   I(*)     Output file in corr coefs (1), deciJy (-1)
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      CHARACTER CLASS*6, TYPE*2, BBAND*1, KEYWRD(5)*8, DCLASS*6
      INTEGER   ICNO, LIMD1, LIMD2, IDISK, LSEQ, LUN, IND, IERR, IVOL,
     *   VALUE(7), NUMKEY, KEYLOC(5), KEYTYP(5), DTYPE, ITBUFF(512),
     *   LUNAN, NANT, I, VER
      HOLLERITH HVALU(7)
      DOUBLE PRECISION FQSEL(2,4)
      LOGICAL   T, F, EOF, DOCOM, WANT
      EQUIVALENCE (VALUE, HVALU), (ITBUFF, TBUFF)
C
      DATA LUN, LUNAN /16, 27/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      ALLNO = 0
C                                       Previous files allowed?
      IF (XDOCON.LE.0.0) GO TO 999
C                                       Frequency selection
      FQSEL(1,1) = DPARM(1)
      FQSEL(2,1) = DPARM(9)
      FQSEL(1,2) = DPARM(3)
      FQSEL(2,2) = DPARM(10)
      FQSEL(1,3) = DPARM(5)
      FQSEL(2,3) = DPARM(9)
      FQSEL(1,4) = DPARM(7)
      FQSEL(2,4) = DPARM(10)
      FQSEL(1,1) = FQSEL(1,1) + DPARM(2)
      FQSEL(1,2) = FQSEL(1,2) + DPARM(4)
      FQSEL(1,3) = FQSEL(1,3) + DPARM(6)
      FQSEL(1,4) = FQSEL(1,4) + DPARM(8)
C                                       Catalog search
      ICNO = 0
      CLASS = '      '
C                                       Which disks
      IF (XDISO.GT.0.0) THEN
         LIMD1 = XDISO + 0.5
         LIMD2 = XDISO + 0.5
      ELSE
         LIMD1 = 1
         LIMD2 = NVOL
         END IF
      TYPE = 'UV'
      DOCOM = XDOUVC.GE.0.0
      DO 100 IDISK = LIMD1,LIMD2
 10         ICNO = ICNO + 1
            LSEQ = XSOUT + 0.5
            IVOL = IDISK
C                                       Find next data base.
            CALL NXTMAP ('READ', IVOL, NAMOUT, CLASS, LSEQ, TYPE,
     *         NLUSER, LUN, IND, ICNO, CATBLK, BUF256, EOF, IERR)
            IF ((IERR.EQ.1) .OR. EOF) GO TO 90
            IF (IERR.NE.0) THEN
               IRET = IERR
               WRITE (MSGTXT,1000) IERR
               GO TO 990
               END IF
C                                       Want this one?
            WANT = F
            MSGSUP = 32000
            CALL UVPGET (IERR)
            MSGSUP = 0
            IF (IERR.NE.0) GO TO 80
C                                       Multisource?
            IF (ILOCSU.LE.0) GO TO 80
C                                       Data compression
            IF ((DOCOM.AND.(CATBLK(KINAX).GT.1)) .OR.
     *         ((.NOT.DOCOM).AND.(CATBLK(KINAX).EQ.1))) GO TO 80
C                                       Frequency
            IF ((DABS (FREQ-FQSEL(1,1)).GT.FQSEL(2,1)) .AND.
     *         (DABS (FREQ-FQSEL(1,2)).GT.FQSEL(2,2)) .AND.
     *         (DABS (FREQ-FQSEL(1,3)).GT.FQSEL(2,3)) .AND.
     *         (DABS (FREQ-FQSEL(1,4)).GT.FQSEL(2,4))) GO TO 80
C                                       Find band
            CALL FLMBC (FREQ, BBAND)
C                                       Check Band
            IF (BAND(1:2).NE.'  ') THEN
               IF ((BBAND.NE.BAND(1:1)) .AND.
     *            (BBAND.NE.BAND(2:2))) GO TO 80
               END IF
C                                       Check Class
            CALL H2CHR(6, KHIMCO, CATH(KHIMC), DCLASS)
            IF (DCLASS.EQ.'LINE  ') THEN
               DTYPE = 2
            ELSE IF (DCLASS.EQ.'CH 0  ') THEN
               DTYPE = 1
            ELSE IF (DCLASS(2:).EQ.' BAND') THEN
               DTYPE = 0
            ELSE
               GO TO 80
               END IF
C                                       Save info
            IF (ALLNO.GE.MXALL) GO TO 80
            ALLNO = ALLNO + 1
            ALLDSK(ALLNO) = IVOL
            ALLCNO(ALLNO) = ICNO
            ALLBAN(ALLNO) = BBAND
            ALLFRQ(ALLNO) = CATD(KDCRV+JLOCF)
            ALLBW(ALLNO) = CATR(KRCIC+JLOCF)
            ALLRFC(ALLNO) = CATR(KRCRP+JLOCF)
            ALLNCH(ALLNO) = CATBLK(KINAX+JLOCF)
            ALLNIF(ALLNO) = CATBLK(KINAX+JLOCIF)
            ALLNST(ALLNO) = CATBLK(KINAX+JLOCS)
            ALLSTK(ALLNO) = CATD(KDCRV+JLOCS) - 0.5D0
            ALLMOD(ALLNO) = F
            ALLCHS(ALLNO) = 0
            ALLCM(ALLNO) = ' '
            ALLIFS(ALLNO) = ' '
            ALLOBC(ALLNO) = 0
            ALLTYP(ALLNO) = DTYPE
C                                       Z Mode stuff
            ALLZMD(ALLNO) = 0
            IF ((DTYPE.EQ.2) .AND. (MOD(ALLNCH(ALLNO), 2).EQ.0))
     *         ALLZMD(ALLNO) = 1
C                                       get station names
            VER = 1
            CALL ANTINI ('READ', TBUFF, IVOL, ICNO, VER, CATBLK, LUNAN,
     *         IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *         RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *         TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
            IF (IERR.NE.0) GO TO 80
            NANT = ITBUFF(5)
            DO 70 I = 1,NANT
               CALL TABAN ('READ', TBUFF, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IERR)
               IF (IERR.NE.0) GO TO 75
               ALLANN(NOSTA,ALLNO) = ANNAME
               ALLXYZ(1,NOSTA,ALLNO) = STAXYZ(1) / (1.D-9 * VELITE)
               ALLXYZ(2,NOSTA,ALLNO) = STAXYZ(2) / (1.D-9 * VELITE)
               ALLXYZ(3,NOSTA,ALLNO) = STAXYZ(3) / (1.D-9 * VELITE)
 70            CONTINUE
            WANT = T
 75         CALL TABIO ('CLOS', 0, IANRNO, ITBUFF, ITBUFF, IERR)
C                                       Close file.
 80         CALL MAPCLS ('READ', IVOL, ICNO, LUN, IND, CATBLK, F,
     *         BUF256, IERR)
            IF (IERR.NE.0) THEN
               IRET = IERR
               WRITE (MSGTXT,1080) IERR
               GO TO 990
               END IF
            IF (.NOT.WANT) GO TO 90
C                                       Read keyword/value pairs
C                                       associated with header.
            NUMKEY = 5
            KEYTYP(1) = 4
            KEYWRD(1) = 'OBSCHANS'
            KEYTYP(2) = 3
            KEYWRD(2) = 'CORRMODE'
            KEYTYP(3) = 4
            KEYWRD(3) = 'SELCHANS'
            KEYTYP(4) = 3
            KEYWRD(4) = 'VLAIFS'
            KEYTYP(5) = 4
            KEYWRD(5) = 'CORRCOEF'
            CALL CATKEY ('REED', IVOL, ICNO, KEYWRD, NUMKEY, KEYLOC,
     *         VALUE, KEYTYP, BUF256, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1090) IERR
C                                       In case 'READ' is used instead
C                                       of 'REED' in CATKEY call
               IF (IERR.LT.20) THEN
                  IRET = IERR
                  GO TO 990
                  END IF
               END IF
C                                       No. observed line channels
            IF (KEYLOC(1).NE.-1) ALLOBC(ALLNO) = VALUE(KEYLOC(1))
C                                       Correlator mode
            IF (KEYLOC(2).NE.-1)
     *         CALL H2CHR (4, 1, HVALU(KEYLOC(2)), ALLCM(ALLNO))
C                                       No. selected line channels
            IF (KEYLOC(3).NE.-1) ALLCHS(ALLNO) = VALUE(KEYLOC(3))
C                                       IF names
            IF (KEYLOC(4).NE.-1)
     *         CALL H2CHR (4, 1, HVALU(KEYLOC(4)), ALLIFS(ALLNO))
C                                       correleation coefficients
            IF (KEYLOC(5).NE.-1) ALLCCO(ALLNO) = VALUE(KEYLOC(5))
C                                       Next
 90         IF (.NOT.EOF) GO TO 10
 100     CONTINUE
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMCAT: NXTMAP ERROR',I3,' SEARCHING CATALOG')
 1080 FORMAT ('FLMCAT: ERROR',I3,' CLOSING FILE')
 1090 FORMAT ('FLMCAT: ERROR',I3,' READING KEYWORD/VALUE PAIR')
      END
      SUBROUTINE FLMUV (IRET)
C-----------------------------------------------------------------------
C   Reads data from tape and writes it to disk.  The output
C   file(s) is(are) created after the first valid record is found.
C   iNdeX, CaLibration, and SoUrce files are written on the fly.
C   Output:
C      IRET     I        Return code, 0 => OK, otherwise abort
C   Used in common:
C      STRTAB   L        If true allow MCHEAD to write AIPS tables
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      CHARACTER SUBMOD*1, NULL*1
      INTEGER   IPTRO(MXSTRM), LUNO(MXSTRM), IS, NIOUT(MXSTRM),
     *          NUMLIM(MXSTRM), BLNDX, INDO(MXSTRM), KBIND(MXSTRM),
     *          DUM2, IERR(MXSTRM), NUMVIS, XCOUNT(MXSTRM),
     *          YCOUNT(MXSTRM), FSTVIS(MXSTRM), LSTVIS(MXSTRM), MAXCNT,
     *          NVS, IPT, LOOP
      REAL      LSTIME, TIMEC, CURTIM, DTIME, OLDTIM, LSTBEG, INTTIM,
     *          RECTIM
      LOGICAL   T, F, START, TIMDIF, NEWINT, DOCL, DOTY, DOWX, DOPO,
     *          DOOF
C
      DATA LUNO /16,17,18,19,20,21,22,23/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      ICLRNO = -1
      START = T
      BLNDX = 0
      NULL = CHAR (0)
      CALL FILL (MXSTRM, 0, CURSOU)
      CALL FILL (MXSTRM, 0, CURFQI)
      CALL FILL (MXSTRM, 0, NIOUT)
      NUMVIS = 0
      CALL FILL (MXSTRM, 1, IPTRO)
      CALL FILL (MXSTRM, 0, YCOUNT)
      TIMCLI = CPARM(8) / 1440.0
      TIMTYI = CPARM(9) / 1440.0
      TIMWXI = CPARM(9) / 1440.0
      TIMPOI = CPARM(9) / 1440.0
      TIMOFI = CPARM(9) / 1440.0
      CALL LFILL (MXSOU*MXSTRM, F, SUMOVE)
C                                       Disable MCHEAD from writing AIPS
C                                       tables.
      STRTAB = F
C                                       Loop
 100     NUMVIS = NUMVIS + 1
C                                       Open or get next vis. record.
         CALL FLMDAT (NUMVIS, IPTRO, BLNDX, IERR)
C                                       Branch on his return
C                                       Error (fatal)
         IF (IERR(1).GT.0) THEN
            IRET = IERR(1)
            WRITE (MSGTXT,1130) IRET
            CALL MSGWRT (8)
            GO TO 200
            END IF
         IF (IERR(1).EQ.-3) GO TO 200
C                                       IERR(1)=-1 => Initialize
C                                       IERR(n)=-2 => Ignore stream n
C                                       IERR(1)=-3 => end of data.
C                                       IERR(1)=-4 => new output file.
C                                       Close files.
         IF (IERR(1).EQ.-4) THEN
            CALL FLMCLS (LUNO, INDO, KBIND, NIOUT, FSTVIS, XCOUNT,
     *         LSTIME, OLDTIM, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Reinit
            CALL FILL (MXSTRM, 0, NSOUR)
            END IF
C                                       Find output file, init etc
C                                       if not done before.
         IF ((IERR(1).EQ.-1) .OR. (IERR(1).EQ.-4)) THEN
            CALL FLMOPN (LUNO, INDO, KBIND, FSTVIS, IRET)
            IF (IRET.NE.0) GO TO 999
            START = F
            CALL COPY (MXSTRM, KBIND, IPTRO)
            CALL FILL (MXSTRM, 1, NUMLIM)
            CALL FILL (MXSTRM, 0, NIOUT)
            DO 120 LOOP = 1,MXSTRM
               XCOUNT(LOOP) = FSTVIS(LOOP) - 1
 120           CONTINUE
C                                       Setup for INDEX file
            LSTIME = AVGIAT
            CURTIM = LSTIME
            OLDTIM = LSTIME
            CALL COPY (MXSTRM, CURSOU, OLDSOU)
            CALL COPY (MXSTRM, CURFQI, OLDFQI)
            LSTBEG = MCLSTB
            SUBMOD = MCALCD(2:2)
C                                       Setup for table entries
            INTTIM = SUMINT / 86400.0
            RECTIM = MCINTG / 19.2 / 86400.0
            RECTIM = 0.8 * RECTIM
            TIMCLI = CPARM(8) / 1440.0
            TIMCLI = MAX (TIMCLI, 0.8*INTTIM)
            TIMTYI = CPARM(9) / 1440.0
            TIMTYI = MAX (TIMTYI, RECTIM)
            TIMWXI = TIMTYI
            TIMPOI = TIMTYI
            TIMOFI = TIMTYI
            TIMNCL = -1.0E20
            TIMNTY = -1.0E20
            TIMNWX = -1.0E20
            TIMNPO = -1.0E20
            TIMNOF = -1.0E20
C                                       Note: first call to FLMDAT is
C                                       a dummy to get initialization
C                                       info from tape.
            GO TO 100
            END IF
C                                       Got datum
C                                       time stamp current integration
         CURTIM = BUFFER(IPTRO(1)+ILOCT,1)
         IF (CURTIM-PRTIME.LT.-0.01/(3600.0*24.0)) THEN
            MSGTXT = 'WARNING: DATA OUT OF TIME ORDER, USE UVSRT'
            IF (INORDR) CALL MSGWRT (7)
            INORDR = .FALSE.
            END IF
C                                       Is this a new time stamp?
         NEWINT = ABS(CURTIM-LSTIME).GT.0.5/3600.0/24.
C                                       Trap NEWINT for debug purposes
C        IF (NEWINT) THEN
C           MSGTXT = 'new integration'
C           CALL MSGWRT (8)
C           END IF
C                                       Do we have a change in submode
C                                       to non-blank (except when in
C                                       holography mode)?
         MODCHN = SUBMOD.NE.MCALCD(2:2).AND.MCALCD(2:2).NE.NULL.AND.
     *            .NOT.MCHOLO
C                                       New scan for 1) LST change, 2)
C                                       submode change, 3) in pointing
C                                       mode for any new time stamp
         IF (LSTBEG.NE.MCLSTB.OR.MODCHN.OR.(NEWINT.AND.MCPTNG)) THEN
C           IF (MODCHN) THEN
C              MSGTXT = 'submode change'
C              CALL MSGWRT (8)
C              END IF
            CALL COPY (MXSTRM, XCOUNT, LSTVIS)
            TIMEC = (LSTIME + OLDTIM) * 0.5
            DTIME = LSTIME - OLDTIM
C                                       Start FQ, SU, CL entries. Don't
C                                       write CL entry when it already
C                                       was written in call to FLMTAB
C                                       in MCHEAD as *last* entry of
C                                       the scan
            TIMDIF = ABS (MCIATI - EOSTIM) .GT. 0.0
            DOOF = WANTOF
            CALL FLMTAB (T, TIMDIF, TIMDIF, TIMDIF, TIMDIF, DOOF, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       INDEX table
            CALL FLMNX (TIMEC, DTIME, FSTVIS, LSTVIS, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL COPY (MXSTRM, CURSOU, OLDSOU)
            CALL COPY (MXSTRM, CURFQI, OLDFQI)
            OLDTIM = CURTIM
            LSTBEG = MCLSTB
            SUBMOD = MCALCD(2:2)
C                                       Set correct source, FQ ids.
            DO 150 IS = 1,NSTREM
               IPT = IPTRO(IS)
               BUFFER(IPT+ILOCSU,IS) = CURSOU(IS)
               BUFFER(IPT+ILOCFQ,IS) = CURFQI(IS)
 150           CONTINUE
            END IF
C                                       Check if time for table entries
         DOCL = CURTIM.GE.TIMNCL
         DOTY = CURTIM.GE.TIMNTY
         DOWX = CURTIM.GE.TIMNWX
         DOPO = CURTIM.GE.TIMNPO
         DOOF = (CURTIM.GE.TIMNOF) .AND. (WANTOF)
         IF (DOCL .OR. DOTY .OR. DOWX .OR. DOPO .OR. DOOF) THEN
            CALL FLMTAB (F, DOCL, DOTY, DOWX, DOPO, DOOF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Reset last time
         LSTIME = CURTIM
         DO 160 IS = 1,NSTREM
C                                       See if good
            IF (IERR(IS).EQ.0) THEN
               IPTRO(IS) = IPTRO(IS) + LRECO(IS)
               XCOUNT(IS) = XCOUNT(IS) + 1
               YCOUNT(IS) = YCOUNT(IS) + 1
               NIOUT(IS) = NIOUT(IS) + 1
               END IF
C                                       Write vis record(s)
            IF (NIOUT(IS).GE.NUMLIM(IS)) THEN
C                                       Expand file?
               IF ((XCOUNT(IS)+NUMLIM(IS)).GT.FILSIZ(IS)) THEN
C                                       Real-time Add only 2,000
                  IF (DOMANY) THEN
                     NVS = XCOUNT(IS) + 2000
C                                       Off-line Add 10,000 vis.
                  ELSE
                     NVS = XCOUNT(IS) + 10000
                     END IF
                  CALL FLMXPN (LUNO(IS), IS, NVS, IRET)
C                                       Expansion failed - save what
C                                       you've got.
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FILE EXPANSION FAILED - QUITTING'
                     CALL MSGWRT (8)
                     XCOUNT(IS) = XCOUNT(IS) - NIOUT(IS)
                     NIOUT(IS) = 0
                     IRET = 0
                     GO TO 200
                     END IF
                  END IF
               CALL UVDISK ('WRIT', LUNO(IS), INDO(IS), BUFFER(1,IS),
     *            NIOUT(IS), KBIND(IS), IRET)
               NWRITN = NWRITN + 1
               NUMLIM(IS) = NIOUT(IS)
C                                       Check for end.
               IF (NUMLIM(IS).LE.0) GO TO 200
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  CALL MSGWRT (8)
                  GO TO 200
                  END IF
               IPTRO(IS) = KBIND(IS)
               NIOUT(IS) = 0
               END IF
 160        CONTINUE
C                                       Next record
         GO TO 100
C                                       Finished reading data
C                                       Final call to FLMDAT.
 200  NUMVIS = -1
      CALL FLMDAT (NUMVIS, IPTRO, DUM2, IERR)
      IRET = IERR(1)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1130) IRET
         GO TO 990
         END IF
C                                       Close down
      CALL FLMCLS (LUNO, INDO, KBIND, NIOUT, FSTVIS, XCOUNT, LSTIME,
     *   OLDTIM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Give message about data read.
      MAXCNT = -99
      DO 210 LOOP=1,MXSTRM
         MAXCNT = MAX (MAXCNT, YCOUNT(LOOP))
 210     CONTINUE
      WRITE (MSGTXT,1230) MAXCNT, NOFILE
      CALL MSGWRT (5)
C                                       Parity error count.
      IF (PECNT.GT.0) THEN
         WRITE (MSGTXT,1232) PECNT
         CALL MSGWRT (6)
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('FLMUV: FLMDAT error',I3)
 1150 FORMAT ('FLMUV: Error',I3,' writing visibility file')
 1230 FORMAT ('Read ',I9,' visibilities from',I3,' files')
 1232 FORMAT ('Encountered ',I7,' parity errors on tape')
      END
      SUBROUTINE FLMOPN (LUNO, INDO, KBIND, FSTVIS, IRET)
C-----------------------------------------------------------------------
C   Find/create/open/initialize output files(s)
C   Input:
C      LUNO     I(*)     LUN for I/O
C   Input from DMC2.INC common:
C      NSTREM   I        Number of output streams (files to create etc.)
C      MCVRF    C*8      Velocity rest frame  2 char /IF
C      DOCORD   L        Planet/Sun indicator
C   Input/Output in common: (if appending to an old file)
C      OBSDAT   C*8      Reference date
C      AVGIAT   D        Average IAT of current integration
C      REFMJD   D        Ref. MJAD
C      SUMOVE   L(*,*)   Source motion indicator
C   Output in common (if SU table already exists)
C      NSOUR    I(*)     Number of sources in source list
C      SULIST   C(*,*)*16 Packed names of sources in list
C      IDSOUR   I(*,4)   Ids of sources on list
C      SUFREQ   D(2,*,*) Source frequency offset (Hz)
C      SUQUAL   I(*,*)   Source qualifier
C   Output in common:
C      CATFRQ   D(*)     Catalogue reference frequency for each stream
C      KINCS    I(*)     Visibility increment in Stokes' for each stream
C      KINCF    I(*)     Visibility increment in Freq for each stream
C      KINCIF   I(*)     Visibility increment in IF for each stream
C      FILSIZ   I(*)     Size of output file in vis. rec.
C      STRTAB    L       If true allow MCHEAD to write AIPS tables
C   Output:
C      INDO      I(*)    FTAB pointer for I/O
C      KBIND     I(*)    Buffer pointer
C      FSTVIS    I(*)    First visibility number in each stream
C      IRET      I       Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   LUNO(*), INDO(*), KBIND(*), FSTVIS(*), IRET
C
      CHARACTER CALCOD*4, VELRF(4)*2, VELDF(3)*2, CHVEL(6)*8, JVELRF*2,
     *   JVELTY*2, NOUT*12, COUT*6
      INTEGER   LENBU, LUNTAB, VER, IERR, IPNT, IR1, IS, IVELTY, IVELDF,
     *   BO, N4SOU, LOOP, I4, MVIS, NVS, JERR, SUFQID
      REAL      FLUX(4,3)
      DOUBLE PRECISION RESTFQ(2), EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *   LSRVEL(2), PMRA, PMDEC, BANW, OLDMJD
      LOGICAL   T, ISOLD
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DATA BO, LENBU /1, 1/
      DATA VELRF, VELDF /'G ','T ','B ','L ', 'V ','Z ','O '/
      DATA CHVEL /'GEOCENTR', 'TOPOCENT', 'BARYCENT',
     *   'LSR     ', 'RADIO   ', 'OPTICAL '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      ICLRNO = 0
      INXRNO = 0
      IFQRNO = 0
C                                       Loop over output streams
      DO 500 IS = 1,NSTREM
         FSTVIS(IS) = 0
         CALL FLMFND (IS, IERR)
         ISOLD = IERR.EQ.0
C                                       Create?
         IF (IERR.NE.0) THEN
            CALL FLMCRE (IS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET
               GO TO 990
               END IF
            END IF
C                                       Message about frequencies
         WRITE (MSGTXT,1001) OBSDAT, MCSKYF(1), MCSKYF(2)
         CALL MSGWRT (4)
C                                       Old file
         IF (ISOLD) THEN
C                                       Get CATBLK
            CALL CATIO ('READ', DISKO(IS), CNOOUT(IS), CATBLK, 'WRIT',
     *         BUF256, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1002) IRET
               GO TO 990
               END IF
C                                       Tell user what it'll append to
            CALL H2CHR (12, 1, CATH(KHIMN), NOUT)
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), COUT)
            WRITE (MSGTXT,1010) NOUT,COUT,CATBLK(KIIMS),DISKO(IS)
            CALL MSGWRT (4)
C
            CALL COPY (256, CATBLK, CATOUT(1,IS))
C                                       Number of vis
            FSTVIS(IS) = CATBLK(KIGCN)
C                                       Old reference day
            CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
            CALL JULDAY (OBSDAT, OLDMJD)
            OLDMJD = OLDMJD - 2400000.5D0
C                                       Correct IAT for new ref. date
            AVGIAT = AVGIAT + NINT(REFMJD - OLDMJD)
            REFMJD = OLDMJD
C                                       Mark in /CFILES/
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKO(IS)
            FCNO(NCFILE) = CNOOUT(IS)
            FRW(NCFILE) = 1
C                                       Set up for FQ files
            CALL FQOLD (CATOUT(1,IS), IS, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Next statement was added to
C                                       reset JLOCF in the case data
C                                       are appended. When creating
C                                       NEW files, this is done in
C                                       FLMHED.       GvM, 10/16/92.
C           JLOCF = STOFFF(IS)
            JLOCF = 2
            END IF
         ISOLD = ISOLD .OR. (FSTVIS(IS).GT.0)
C                                       Set reference frequency
C
         CATFRQ(IS) = CATD(KDCRV+JLOCF) * 1.0D-9
C                                       Pointers etc.
         CALL UVPGET (JERR)
C                                       Save increments
         CALL MCKINC (IS)
C        KINCS(IS) = INCS
C        KINCF(IS) = INCF
C        KINCIF(IS) = INCIF
         KLOCWT = 0
         IF (CATBLK(KINAX).LE.1) THEN
C           KINCS(IS) = KINCS(IS) * 3
C           KINCF(IS) = KINCF(IS) * 3
C           KINCIF(IS) = KINCIF(IS) * 3
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *         KLOCWT, IERR)
            END IF
C                                       Open vis file for write
         CALL ZPHFIL ('UV', DISKO(IS), CNOOUT(IS), 1, SFILE(IS), IRET)
         CALL ZOPEN (LUNO(IS), INDO(IS), DISKO(IS), SFILE(IS), T, T, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       LREC = length of output rec.
         LRECO(IS) = LREC
         FILSIZ(IS) = FSTVIS(IS)
C                                       Expand old files
         IF (ISOLD) THEN
C                                       Real-time add only 2,000 vis.
            IF (DOMANY) THEN
               NVS = FSTVIS(IS) + 2000
            ELSE
               NVS = FSTVIS(IS) + 10000
               END IF
            CALL FLMXPN (LUNO(IS), IS, NVS, IRET)
            IF (IRET.NE.0) THEN
C                                       Expansion failed - save what
C                                       you've got.
               MSGTXT = 'File expansion failed - quitting!'
               GO TO 990
               END IF
            END IF
C                                       Get file size
         CALL ZEXIST (DISKO(IS), SFILE(IS), FILSIZ(IS), IERR)
         FILSIZ(IS) = (FILSIZ(IS) * 256.0D0) / LRECO(IS)
C                                       Init vis file for write
C                                       Force single buffering to avoid
C                                       problems with file expansion.
         LENBU = (JBUFSZ - 2*NBPS) / (2 * LRECO(IS))
C                                       Could be lots of data
         MVIS = 100000000
         CALL UVINIT ('WRIT', LUNO(IS), INDO(IS), MVIS, FSTVIS(IS),
     *      LRECO(IS), LENBU, JBUFSZ, BUFFER(1,IS), BO, KBIND(IS), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
         FSTVIS(IS) = FSTVIS(IS) + 1
C                                       Create INDEX tables
         LUNTAB = 29
         VER = 1
         CALL NDXINI ('WRIT', NXBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATBLK, LUNTAB, INXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       If more than one stream close
C                                       table.
         CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, IRET)
C                                       SOURCE table
         LUNTAB = 28
         INOGRP = STNOIF(IS)
C                                       Decode velocity reference
C                                       frame, definition
         IR1 = STMCI1(IS)
         IPNT = (IR1-1) * 2 + 1
         JVELRF = MCVRF(IPNT:IPNT) // ' '
         IVELTY = 0
         IF (JVELRF.EQ.VELRF(1)) IVELTY = 1
         IF (JVELRF.EQ.VELRF(2)) IVELTY = 2
         IF (JVELRF.EQ.VELRF(3)) IVELTY = 3
         IF (JVELRF.EQ.VELRF(4)) IVELTY = 4
         IF (IVELTY.GT.0) THEN
            VELTYP = CHVEL(IVELTY)
         ELSE
            VELTYP = '        '
            END IF
C                                       Definition
         IPNT = (IR1-1) * 2 + 2
         JVELTY = MCVRF(IPNT:IPNT) // ' '
         IVELDF = 0
         IF (JVELTY.EQ.VELDF(1)) IVELDF = 1
         IF (JVELTY.EQ.VELDF(2)) IVELDF = 2
C                                       Ignore 'O = offset'
         IF (IVELDF.GT.0) THEN
            VELDEF = CHVEL(IVELDF+4)
         ELSE
            VELDEF = '        '
            END IF
C                                       User set frequency => velocity
C                                       info is crap
         IF ((JVELTY.EQ.'F ') .OR. (JVELTY.EQ.'  ')) THEN
            VELTYP = ' '
            VELDEF = ' '
            END IF
         VER = 1
         SUFQID = -1
         CALL SOUINI ('WRIT', SUBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATBLK, LUNTAB, INOGRP, VELTYP, VELDEF, SUFQID, ISURNO,
     *      SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
         N4SOU = SUBUFF(5)
C                                       Read existing source list
         IF ((FSTVIS(IS).GT.1) .OR. (N4SOU.GT.0)) THEN
            DO 110 LOOP = 1, MXSOU
               SUMOVE(LOOP,IS) = .FALSE.
 110           CONTINUE
            NSOUR(IS) = N4SOU
            DO 100 LOOP = 1,N4SOU
               I4 = LOOP
               CALL TABSOU ('READ', SUBUFF, I4, SUKOLS, SUNUMV,
     *            SOURID(LOOP,IS), SULIST(LOOP,IS), SUQUAL(LOOP,IS),
     *            CALCOD, FLUX, SUFREQ(1,LOOP,IS), BANW,
     *            SUPOS(1,LOOP,IS), SUPOS(2,LOOP,IS), EPOCH, RAAPP,
     *            DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               IF ((EPOCH.EQ.-1) .AND. (.NOT.DOCORD))
     *            SUMOVE(SOURID(LOOP,IS),IS) = .TRUE.
 100           CONTINUE
            END IF
C                                       If more than one stream close
C                                       table.
         CALL TABIO ('CLOS', 0, ISURNO, SUBUFF, SUBUFF, IRET)
C                                       Save catalog header
         CALL COPY (256, CATBLK, CATOUT(1,IS))
 500     CONTINUE
C                                       resume appending to existing
C                                       files
      DOBREK = .FALSE.
C                                       Initialize FQ & SU tables.
      CALL FLMTAB (.TRUE., .FALSE., .FALSE., .FALSE., .FALSE.,
     *             .FALSE., IRET)
C                                       Enable MCHEAD to write AIPS
C                                       tables.
      STRTAB = .TRUE.
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Error',I3,' creating output file')
 1001 FORMAT ('Ref. date = ',A,' A-C =',F10.6,' B-D =', F10.6, ' GHz')
 1002 FORMAT ('Error',I3,' copying CATBLK')
 1010 FORMAT ('Appending new data to: ',A12,'.',A6,'.',I3,' disk ',I2)
 1020 FORMAT ('Error',I3,' opening visibility file for write')
 1040 FORMAT ('Error',I3,' initializing visibility file for write')
      END
      SUBROUTINE FLMFND (IS, IRET)
C-----------------------------------------------------------------------
C   Finds if the desired output file is already in allowed file table.
C   Input:
C      IS       I        Stream number
C   Input in common:
C      ALLNO    I        Number of allowed output files
C      ALLDSK   I(*)     Output file disk numbers
C      ALLCNO   I(*)     Output file cat. slot numbers
C      ALLBAN   C(*)*1   Output file band code
C      ALLCM    C(*)*4   Output file correlator mode
C      ALLFRQ   D(*)     Output file reference frequency (Hz)
C      ALLBW    R(*)     Output file reference Bandwidth (ch. sep) (Hz)
C      ALLRFC   R(*)     Output file reference channel
C      ALLNCH   I(*)     Output file no. channels
C      ALLCHS   I(*)     Output channels in line data (for ch0 files)
C      ALLNIF   I(*)     Output file no. IFs
C      ALLNST   I(*)     Output file no. Stokes'
C      ALLSTK   I(*)     Output file first stokes type
C      ALLIFS   C(*)*4   Output file IF names
C      ALLOBC   I(*)     Output file - no. channels in original data
C      ALLTYP   I(*)     Output file data type (see STTYPE)
C      ALLZMD   I(*)     Output file in Z mode (ch 0 part of spectrum)
C      ALLCCO   I(*)     Output file in corr coefs (1), deciJy (-1)
C      MCCORM   C*4      Correlator mode
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STNOIF   I(*)     No. IFs in each stream
C      STNOCH   I(*)     No. Channels in each stream
C      STNOPL   I(*)     No. polarizations in each stream
C      STBCH    I(*)     1st channel to select in each stream
C      STECH    I(*)     Highest channel to select in each stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C   Output in common:
C      ALLMOD   L(*)     Output file modified flag.
C      DISKO    I(*)     Output disk number
C      CNOOUT   I(*)     Output catalog slot number
C   Output:
C      IRET     I        Return error code, 0=>found, else not
C-----------------------------------------------------------------------
      INTEGER   IS, IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      CHARACTER BBAND*1, RDATE*8
      INTEGER   LOOP, IP, ISTK, NUMCHN, I, LCCO
      LOGICAL   CONT
      DOUBLE PRECISION TOLERF, DTEMP, LTOLER, JDNOMC, JD
C-----------------------------------------------------------------------
      IRET = 1
C                                       Get GST0 and Earth rotation rate
      RDATE = '20070630'
      CALL JULDAY (RDATE, JDNOMC)
      CALL JULDAY (OBSDAT, JD)
      TOLERB = 0.02D0
      IF (JD.GT.JDNOMC) TOLERB = 0.001D0
C                                       correlation coeff?
      LCCO = -1
      IF (ISSUN) THEN
         LCCO = 1
      ELSE
         IF (RICKS) LCCO = 1
         END IF
C                                       Kludge for VLA real-time
      IF (DOMANY.OR.DOBREK) GO TO 999
      TOLERF = DPARM(9)
      IP = STMCI1(IS)
      IF ((IP.EQ.2).OR.(IP.EQ.4)) TOLERF = DPARM(10)
      DTEMP = STFREQ(IS) * 1.0D9
      CALL FLMBC (DTEMP, BBAND)
C                                       Calculate selected channels
      IF (STTYPE(IS).EQ.2) THEN
         NUMCHN = STECH(IS) - STBCH(IS) + 1
         CONT = .FALSE.
      ELSE IF (STTYPE(IS).EQ.1) THEN
         NUMCHN = STECH(IS+1) - STBCH(IS+1) + 1
         CONT = .FALSE.
      ELSE
         NUMCHN = 0
         CONT = .TRUE.
         END IF
C                                       This file's first Stokes'
      ISTK = -1
      IF ((IP.GT.2) .AND. .NOT.CONT) ISTK = -2
C                                       Search list - checking (all)
C                                       band, # polns., 1st Stokes',
C                                       # IFs, IF names, corr. mode,
C                                       total # channels in this file.
C                                       (spectral line only)
C                                       # channels selected in the line
C                                       channels file, # observed
C                                       channels.
      DO 100 LOOP = 1,ALLNO
         IF ((ABS (ALLFRQ(LOOP)-(STFREQ(IS)*1.0D9)).LE.TOLERF) .AND.
     *      (ALLBAN(LOOP).EQ.BBAND) .AND.
     *      (ALLNST(LOOP).EQ.STNOPL(IS)) .AND.
     *      (ALLSTK(LOOP).EQ.ISTK) .AND.
     *      (ALLNIF(LOOP).EQ.STNOIF(IS)) .AND.
     *      (ALLIFS(LOOP).EQ.STIFNA(IS)) .AND.
     *      (ALLCM(LOOP).EQ.MCCORM) .AND.
     *      (ALLNCH(LOOP).EQ.STNOCH(IS)) .AND.
     *      (ALLZMD(LOOP).EQ.STZMOD(IS)) .AND.
     *      ((ALLCCO(LOOP).EQ.LCCO) .OR. (ALLCCO(LOOP).EQ.0)) .AND.
     *      (ALLTYP(LOOP).EQ.STTYPE(IS)) .AND.
     *      ((ALLCHS(LOOP).EQ.NUMCHN) .OR. CONT) .AND.
     *      ((ALLOBC(LOOP).EQ.MCCHAN(IP)) .OR. CONT)) THEN
C                                       possible - check stations
            LTOLER = TOLERB
            DO 90 I = 1,MXANT
               IF ((MCANTN(I)(6:8).NE.'OUT') .AND.
     *            (ALLANN(I,LOOP)(6:8).NE.'OUT')) THEN
                  IF (MCANTN(I).NE.ALLANN(I,LOOP)) GO TO 100
                  IF (I.EQ.MXANT) LTOLER = 2 * LTOLER
                  IF (ABS(MCAXYZ(1,I)-ALLXYZ(1,I,LOOP)).GT.LTOLER)
     *               GO TO 100
                  IF (ABS(MCAXYZ(2,I)-ALLXYZ(2,I,LOOP)).GT.LTOLER)
     *               GO TO 100
                  IF (ABS(MCAXYZ(3,I)-ALLXYZ(3,I,LOOP)).GT.LTOLER)
     *               GO TO 100
                  END IF
 90            CONTINUE
C                                       Found it
            ALLMOD(LOOP) = .TRUE.
            DISKO(IS) = ALLDSK(LOOP)
            CNOOUT(IS) = ALLCNO(LOOP)
            IRET = 0
            GO TO 999
            END IF
 100     CONTINUE
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FLMCRE (IS, IRET)
C-----------------------------------------------------------------------
C   Fills in the output catalog header record, creates the output file.
C   Input:
C      IS       I        Stream number
C   Input from common:
C      XDISO    R        Requested output disk (0=> any)
C      XSOUT    R        Requested output seq. (0=> any)
C      NAMOUT   C*12     Requested output name
C      OBSDAT   C*8      Reference date (used if NAMOUT is blank)
C      XDOCON   R        If > 0.0 then allow appending to old files
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STBCH    I(*)     1st channel to select in each stream
C      STECH    I(*)     Highest channel to select in each stream
C      STIFNA   C*4(*)   Names of VLA IFs included in stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCCORM   R        Correlator mode
C      MCCHAN   I(4)     Line channels for IFs (A,B,C,D)
C   Output in common:
C      KINCS    I(*)     Visibility increment in Stokes' for each stream
C      KINCF    I(*)     Visibility increment in Freq for each stream
C      KINCIF   I(*)     Visibility increment in IF for each stream
C      ALLNO    I        Number of allowed output files
C      ALLDSK   I(*)     Output file disk numbers
C      ALLCNO   I(*)     Output file cat. slot numbers
C      ALLBAN   C(*)*1   Output file band code
C      ALLCM    C(*)*4   Output file correlator mode
C      ALLFRQ   D(*)     Output file reference frequency (Hz)
C      ALLBW    R(*)     Output file reference Bandwidth (ch. sep) (Hz)
C      ALLRFC   R(*)     Output file reference channel
C      ALLNCH   I(*)     Output file no. channels.
C      ALLCHS   I(*)     Output channels in line data
C      ALLNIF   I(*)     Output file no. IFs
C      ALLNST   I(*)     Output file no. Stokes'
C      ALLSTK   I(*)     Output file first stokes type
C      ALLMOD   L(*)     Output file modified flag
C      ALLIFS   C(*)*4   Output file IF names
C      ALLOBC   I(*)     Output file - no. channels in original data
C      ALLTYP   I(*)     Output file data type (see STTYPE)
C      ALLZMD   I(*)     Output file in Z mode (ch 0 part of spectrum)
C      ALLCCO   I(*)     Output file in corr coefs (1), deciJy (-1)
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IS, IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      CHARACTER OLDNAM*12, BNDC*1, KEYWRD(5)*8
      INTEGER   JERR, NUMKEY, KEYLOC(5), VALUE(7), KEYTYP(5), SELCH, I,
     *   J, LCCO
      LOGICAL   T
      HOLLERITH HVALU(7)
      EQUIVALENCE (VALUE, HVALU)
C
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Create new header.
      CALL FLMHED (IS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
C                                       Save increments
      CALL MCKINC (IS)
C     KINCS(IS) = INCS
C     KINCF(IS) = INCF
C     KINCIF(IS) = INCIF
C     IF (CATBLK(KINAX).LE.1) THEN
C        KINCS(IS) = KINCS(IS) * 3
C        KINCF(IS) = KINCF(IS) * 3
C        KINCIF(IS) = KINCIF(IS) * 3
C        END IF
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      OLDNAM = OBSDAT // '    '
      IF (STTYPE(IS).EQ.2) THEN
C                                       Line
         CLAOUT(IS) = 'LINE  '
      ELSE IF (STTYPE(IS).EQ.1) THEN
C                                       Channel 0
         CLAOUT(IS) = 'CH 0  '
      ELSE
C                                       Continuum
         CALL FLMBC (MCSKYF(STMCI1(IS))*1.0D9, BNDC)
         CLAOUT(IS) = BNDC // ' BAND'
         END IF
      SEQOUT(IS) = NINT(XSOUT)
      DISKO(IS) = NINT(XDISO)
C
      CALL MAKOUT (OLDNAM, '      ', 0, CLAOUT(IS), NAMOUT,
     *   CLAOUT(IS), SEQOUT(IS))
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT(IS), KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT(IS)
C                                       Create output file.
      CNOOUT(IS) = 1
      CALL UVCREA (DISKO(IS), CNOOUT(IS), BUF256, IRET)
C                                       Check if it already exists
      IF (IRET.EQ.2) THEN
         IF (XDOCON.LE.0.0) THEN
            WRITE (MSGTXT,1001)
         ELSE
            WRITE (MSGTXT,1002)
            END IF
         GO TO 990
C                                       Other UVCREA error
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1003) IRET
         GO TO 990
         END IF
C                                       correlation coeff?
      LCCO = -1
      IF (ISSUN) THEN
         LCCO = 1
      ELSE
         IF (RICKS) LCCO = 1
         END IF
C                                       Calculate selected channels
      IF (STTYPE(IS).EQ.2) THEN
         SELCH = STECH(IS) - STBCH(IS) + 1
      ELSE IF (STTYPE(IS).EQ.1) THEN
         SELCH = STECH(IS+1) - STBCH(IS+1) + 1
      ELSE
         SELCH = 0
         END IF
C                                       Write keyword/value pairs
      IF (STTYPE(IS).NE.0) THEN
         NUMKEY = 5
         KEYLOC(1) = 1
         KEYTYP(1) = 4
         KEYWRD(1) = 'OBSCHANS'
         VALUE(1) = MCCHAN(STMCI1(IS))
         KEYLOC(2) = 2
         KEYTYP(2) = 3
         KEYWRD(2) = 'CORRMODE'
         CALL CHR2H (8, MCCORM, 1, HVALU(2))
         KEYLOC(3) = 4
         KEYTYP(3) = 4
         KEYWRD(3) = 'SELCHANS'
         VALUE(4) = SELCH
         KEYLOC(4) = 5
         KEYTYP(4) = 3
         KEYWRD(4) = 'VLAIFS'
         CALL CHR2H (8, STIFNA(IS), 1, HVALU(5))
         KEYLOC(5) = 7
         KEYTYP(5) = 4
         KEYWRD(5) = 'CORRCOEF'
         VALUE(7) = LCCO
      ELSE
         NUMKEY = 3
         KEYLOC(1) = 1
         KEYTYP(1) = 3
         KEYWRD(1) = 'CORRMODE'
         CALL CHR2H (8, MCCORM, 1, HVALU(1))
         KEYLOC(2) = 3
         KEYTYP(2) = 3
         KEYWRD(2) = 'VLAIFS'
         CALL CHR2H (8, STIFNA(IS), 1, HVALU(3))
         KEYLOC(3) = 5
         KEYTYP(3) = 4
         KEYWRD(3) = 'CORRCOEF'
         VALUE(5) = LCCO
         END IF
      CALL CATKEY ('WRIT', DISKO(IS), CNOOUT(IS), KEYWRD, NUMKEY,
     *   KEYLOC, VALUE, KEYTYP, BUF256, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1004) JERR
         IRET = JERR
         GO TO 990
         END IF
C                                       Mark in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO(IS)
      FCNO(NCFILE) = CNOOUT(IS)
      FRW(NCFILE) = 1
C                                       Get SEQ. no. used.
      SEQOUT(IS) = CATBLK(KIIMS)
C                                       Save info. Make new file #1 in
C                                       the list. It will be first in
C                                       line to receive further data.
      IF (ALLNO.LT.MXALL) THEN
         ALLNO = ALLNO + 1
         DO 100 I = ALLNO, 2, -1
            ALLDSK(I) = ALLDSK(I-1)
            ALLCNO(I) = ALLCNO(I-1)
            ALLFRQ(I) = ALLFRQ(I-1)
            ALLBAN(I) = ALLBAN(I-1)
            ALLCM(I)  = ALLCM(I-1)
            ALLBW(I)  = ALLBW(I-1)
            ALLRFC(I) = ALLRFC(I-1)
            ALLNCH(I) = ALLNCH(I-1)
            ALLZMD(I) = ALLZMD(I-1)
            ALLCCO(I) = ALLCCO(I-1)
            ALLNIF(I) = ALLNIF(I-1)
            ALLNST(I) = ALLNST(I-1)
            ALLSTK(I) = ALLSTK(I-1)
            ALLMOD(I) = ALLMOD(I-1)
            ALLCHS(I) =  ALLCHS(I-1)
            ALLIFS(I) = ALLIFS(I-1)
            ALLOBC(I) = ALLOBC(I-1)
            ALLTYP(I) = ALLTYP(I-1)
            DO 95 J = 1,MXANT
               ALLANN(J,I) = ALLANN(J,I-1)
               ALLXYZ(1,J,I) = ALLXYZ(1,J,I-1)
               ALLXYZ(2,J,I) = ALLXYZ(2,J,I-1)
               ALLXYZ(3,J,I) = ALLXYZ(3,J,I-1)
 95            CONTINUE
 100        CONTINUE
         ALLDSK(1) = DISKO(IS)
         ALLCNO(1) = CNOOUT(IS)
         ALLFRQ(1) = CATD(KDCRV+JLOCF)
         CALL FLMBC (ALLFRQ(1), BNDC)
         ALLBAN(1) = BNDC
         ALLCM(1)  = MCCORM
         ALLBW(1)  = CATR(KRCIC+JLOCF)
         ALLRFC(1) = CATR(KRCRP+JLOCF)
         ALLNCH(1) = CATBLK(KINAX+JLOCF)
         ALLNIF(1) = CATBLK(KINAX+JLOCIF)
         ALLNST(1) = CATBLK(KINAX+JLOCS)
         ALLSTK(1) = CATD(KDCRV+JLOCS) - 0.5D0
         ALLMOD(1) = T
         ALLZMD(1) = STZMOD(IS)
         ALLCCO(1) = LCCO
         ALLCHS(1) = SELCH
         ALLIFS(1) = STIFNA(IS)
         ALLOBC(1) = MCCHAN(STMCI1(IS))
         ALLTYP(1) = STTYPE(IS)
         DO 105 J = 1,MXANT
            ALLANN(J,1) = MCANTN(J)
            ALLXYZ(1,J,1) = MCAXYZ(1,J)
            ALLXYZ(2,J,1) = MCAXYZ(2,J)
            ALLXYZ(3,J,1) = MCAXYZ(3,J)
 105        CONTINUE
         END IF
C                                       Write history
      CALL FLMHIS (IS)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('DOCONCAT was not set to append to existing files')
 1002 FORMAT ('New data is inconsistent with existing file')
 1003 FORMAT ('Error',I3,' creating output file')
 1004 FORMAT ('Error',I3,' writing keyword/value pairs')
      END
      SUBROUTINE MCKINC (STREAM)
C-----------------------------------------------------------------------
C   Computes axis increments in ModComp form of data
C   Inputs:
C      STREAM   I      Stream number
C   Input in common:
C      STOFFS   I(*)   Stokes order for input stream
C      STOFFF   I(*)   Freq order for input stream
C      STOFIF   I(*)   IF order for input stream
C      STNOPL   I(*)   Stokes number for input stream
C      STNOCH   I(*)   Freq number for input stream
C      STNOIF   I(*)   IF number for input stream
C   Output in common:
C      KINCS    I(*)   Increment for STREAM in Stokes
C      KINCF    I(*)   Increment for STREAM in Frequency
C      KINCIF   I(*)   Increment for STREAM in IF
C-----------------------------------------------------------------------
      INTEGER   STREAM
C
      INTEGER   IH(4), JLS, JLF, JLIF, I
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
C-----------------------------------------------------------------------
      JLS = STOFFS(STREAM)
      JLF = STOFFF(STREAM)
      JLIF = STOFIF(STREAM)
      IH(1) = 3
      IH(1+JLS) = STNOPL(STREAM)
      IH(1+JLF) = STNOCH(STREAM)
      IH(1+JLIF) = STNOIF(STREAM)
      KINCS(STREAM) = 1
      DO 10 I = 1,JLS
         KINCS(STREAM) = KINCS(STREAM) * IH(I)
 10      CONTINUE
      KINCF(STREAM) = 1
      DO 20 I = 1,JLF
         KINCF(STREAM) = KINCF(STREAM) * IH(I)
 20      CONTINUE
      KINCIF(STREAM) = 1
      DO 30 I = 1,JLIF
         KINCIF(STREAM) = KINCIF(STREAM) * IH(I)
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FLMBC (FREQ, BANCOD)
C-----------------------------------------------------------------------
C   Determines VLA band code for a given frequency.
C
C   The ranges of the bands given in the arrays VLOW and VHIGH contain
C   the VLA-specific limits imposed by the availability of LO settings.
C   (The divisions at the lower frequencies are, however, arbitrary).
C   Included for completeness are the advertized and fully supported
C   band limits in the arrays BLOW and BHIGH.  These are not currently
C   used.
C          *added Q-band, GvM, 24Nov93 *
C
C   Input:
C      FREQ     D        Frequency (Hz)
C   Input from special common
C      FLMBCX   C*2      version 33+ observing band code
C   Output:
C      BANCOD   C*(*)    Band code (e.g. 'L','C' etc.)
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQ
      CHARACTER BANCOD*(*)
C
      INTEGER   NOBAND
      PARAMETER (NOBAND=10)
C
      CHARACTER BBAND(NOBAND)*1
      INTEGER   I
      DOUBLE PRECISION VLOW(NOBAND), VHIGH(NOBAND)
      INCLUDE 'MCB.INC'
C      DOUBLE PRECISION BLOW(NOBAND), BHIGH(NOBAND)
C      DATA   BLOW, BHIGH /
C    *   72D6, 306D6, 1.34D9, 4.50D9,  8.00D9, 14.40D9, 22.00D9, 38.0D9,
C    *   75D6, 340D6, 1.73D9, 5.00D9,  8.80D9, 15.40D9, 24.00D9, 52.0D9/
C
C
      DATA VLOW, VHIGH, BBAND
     * /  0D6, 150D6, 0.7D9, 2.D9, 4.D9,  8.D9, 12.D9, 18.0D9, 26.5D9,
     *  40.0D9,
     *  150D6, 700D6, 2.0D9, 4.D9, 8.D9, 12.D9, 18.D9, 26.5D9, 40.0D9,
     *  56.0D9,
     *   '4',   'P',   'L',  'S',  'C',   'X',   'U',    'K',     'A',
     *     'Q'/
C-----------------------------------------------------------------------
C                                       Find band
      IF (MCRCVX.NE.' ') THEN
         BANCOD = MCRCVX(1:1)
      ELSE
         BANCOD = '?'
         DO 50 I = 1,NOBAND
            IF ((FREQ.GE.VLOW(I)) .AND. (FREQ.LT.VHIGH(I)))
     *         BANCOD = BBAND(I)
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLMXPN (LUN, IS, NVS, IRET)
C-----------------------------------------------------------------------
C   Expands output file associated with stream IS.
C   Input:
C      LUN      I        LUN of file
C      IS       I        Stream number
C      NVS      I        Total number of vis wanted in file
C   Input/output in common:
C      FILSIZ   I(*)     Size of output file in vis. rec.
C      LRECO    I(*)     Length of output rec.
C   Output:
C      IRET     I        Return code, 0=>OK, else failed
C-----------------------------------------------------------------------
      INTEGER   LUN, IS, NVS, IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      INTEGER   NREC, ZREC
C-----------------------------------------------------------------------
C                                       Expand file
      NREC = (((NVS - FILSIZ(IS)) * LRECO(IS) * 2) / 512) + 1
      ZREC = NREC
      CALL ZEXPND  (LUN, DISKO(IS), SFILE(IS), NREC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check got what we wanted
      IF (NREC.LT.ZREC) THEN
         IRET = 1
         WRITE (MSGTXT,1000) ZREC, NREC
         GO TO 990
         END IF
C                                       Get new file size
      CALL ZEXIST (DISKO(IS), SFILE(IS), FILSIZ(IS), IRET)
      FILSIZ(IS) = (FILSIZ(IS) * 256.0D0) / LRECO(IS)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMXPN: REQUESTED ',I8,' BLOCKS, GOT ',I8)
      END
      SUBROUTINE FLMCLS (LUNO, INDO, KBIND, NIOUT, FSTVIS, XCOUNT,
     *   LSTIME, OLDTIM, IRET)
C-----------------------------------------------------------------------
C   Flushes I/O buffers, closes files and tables and updates the number
C   of visibilities in the catalog headers.
C   Input:
C      LUNO     I(*)     LUNs for I/O (1 / stream)
C      INDO     I(*)     FTAB pointers for I/O
C      KBIND    I(*)     buffer pointers
C      NIOUT    I(*)     Number of vis in each buffer.
C      FSTVIS   I(*)     First visibility number in each stream
C      XCOUNT   I(*)     Highest vis number + 1 in stream
C      LSTIME   R        End IAT (days)
C      OLDTIM   R        Begin time of scan
C   Input from common:
C      NSTREM   I        Number of output streams (files to create etc.)
C      OSTREM   I        Number of output streams from previous scan
C      OSTNPL   I(*)     Number of polzns in previous scan
C      OSTNIF   I(*)     Number of IF's in previous scan
C      BUFFER   R(*,*)   I/O Buffers
C   Output:
C      IRET     I        Return error code, 0=>OK, else failed.
C   Output in common:
C      STRTAB   L        If true allow MCHEAD to write AIPS tables.
C-----------------------------------------------------------------------
      INTEGER   LUNO(*), INDO(*), KBIND(*), NIOUT(*), FSTVIS(*),
     *          XCOUNT(*), IRET
      REAL      LSTIME, OLDTIM
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      CHARACTER NAME*12, CLASS*6, PTYPE*2, STAT*4
      INTEGER   IS, SEQ, USID, I, HIGH, LSTVIS(MXSTRM), TSTREM, LSIZE,
     *   TSTNPL(MXSTRE), TSTNIF(MXSTRE)
      REAL      TIMEC, DTIME
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Disable MCHEAD from writing AIPS
C                                       tables.
      STRTAB = .FALSE.
C                                       Ensure correct # streams
C                                       closed down
      IF (OSTREM.EQ.-1) OSTREM = NSTREM
      TSTREM = NSTREM
      NSTREM = OSTREM
      CALL COPY (MXSTRE, STNOPL, TSTNPL)
      CALL COPY (MXSTRE, STNOIF, TSTNIF)
      CALL COPY (MXSTRE, OSTNPL, STNOPL)
      CALL COPY (MXSTRE, OSTNIF, STNOIF)
C                                       Finish write
      DO 50 IS = 1,NSTREM
C                                       Expand file?
         IF (XCOUNT(IS).GT.FILSIZ(IS)) THEN
            CALL FLMXPN (LUNO(IS), IS, XCOUNT(IS), IRET)
            IF (IRET.NE.0) THEN
C                                       Expansion failed - save what
C                                       you've got.
               MSGTXT = 'FILE EXPANSION FAILED - SOME DATA LOST'
               CALL MSGWRT (8)
               IRET = 0
C                                       Some data lost in buffer
               XCOUNT(IS) = XCOUNT(IS) - ABS (NIOUT(IS)) - 1
               GO TO 20
               END IF
            END IF
         NIOUT(IS) = - ABS (NIOUT(IS))
         CALL UVDISK ('FLSH', LUNO(IS), INDO(IS), BUFFER(1,IS),
     *      NIOUT(IS), KBIND(IS), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FLUSH'
            GO TO 990
            END IF
C                                       Compress file
         CALL UVSIZE (LRECO(IS), XCOUNT(IS), LSIZE)
         CALL ZCMPRS (ALLDSK(IS), SFILE(IS), LUNO(IS), LSIZE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COMPRESS'
            GO TO 990
            END IF
C                                       Close file
 20      CALL ZCLOSE (LUNO(IS), INDO(IS), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOS'
            GO TO 990
            END IF
C                                      Out of order!
         IF (.NOT.INORDR) CALL CHR2H (2, '  ', 1, CATOUT(KITYP,IS))
C                                      Put vis. count in CATBLK
         CATOUT(KIGCN,IS) = XCOUNT(IS)
         CALL CATIO ('UPDT', DISKO(IS), CNOOUT(IS), CATOUT(1,IS),
     *      'REST', BUF256, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
C                                       Clear status
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISKO(IS), CNOOUT(IS), NAME, CLASS, SEQ,
     *      PTYPE, USID, STAT, BUF256, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1002) IRET
            GO TO 990
            END IF
C                                       Clear in DFIL.INC common
         HIGH = 0
         DO 30 I = 1,NCFILE
            IF ((FVOL(I).EQ.DISKO(IS)) .AND. (FCNO(I).EQ.CNOOUT(IS)))
     *         THEN
               FVOL(I) = 0
               FCNO(I) = 0
               FRW(I) = 0
               END IF
C                                       To reset NCFILE:
            IF ((FVOL(I).NE.0) .OR. (FCNO(I).NE.0)) HIGH = I
 30         CONTINUE
C                                       Reset NCFILE if necessary
            NCFILE = HIGH
C                                       Update AN file
            CALL ANTUPD (IS)
C
 50      CONTINUE
      IS = NSTREM + 5
C                                       Last index record
      CALL COPY (MXSTRM, XCOUNT, LSTVIS)
      TIMEC = (LSTIME + OLDTIM) * 0.5
      DTIME = LSTIME - OLDTIM
      CALL FLMNX (TIMEC, DTIME, FSTVIS, LSTVIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       INDEX table
C                                       Restore # streams etc for
C                                       new scan
      NSTREM = TSTREM
      CALL COPY (MXSTRE, TSTNPL, STNOPL)
      CALL COPY (MXSTRE, TSTNIF, STNOIF)
      OSTREM = NSTREM
      CALL COPY (MXSTRE, STNOPL, OSTNPL)
      CALL COPY (MXSTRE, STNOIF, OSTNIF)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
      IF (IS.LE.NSTREM) THEN
         WRITE (MSGTXT,1990) IS
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMCLS: ERROR ',I3,1X,A,'ING OUTPUT')
 1001 FORMAT ('FLMCLS: ERROR ',I3,' UPDATING OUTPUT CATALOG HEADER')
 1002 FORMAT ('FLMCLS: ERROR ',I3,' CLEARING OUTPUT FILE STATUS')
 1050 FORMAT ('FLMCLS: ERROR ',I3,' WRITING LAST INDEX RECORD')
 1990 FORMAT ('FLMCLS: ERROR OCCURED ON STREAM ',I3)
      END
      SUBROUTINE FLMNX (TIME, DT, FSTVIS, LSTVIS, IRET)
C-----------------------------------------------------------------------
C   Routine to write NX tables.
C   Input:
C      TIME     R        Time (days)
C      DT       R        Time interval (days)
C      LSTVIS   I(*)     Last visibility number per stream
C   Input/Output:
C      FSTVIS   I(*)     First visibility number
C   Input from common:
C      NSTREM   I        Number of streams
C      DISKO    I(*)     Disk number of files
C      CNOOUT   I(*)     Catalog slot number of files
C      OLDSOU   I(*)     Source number just finished, per stream
C      OLDFQI   I(*)     FQ number just finished, per stream
C   Output:
C      IRET     I        Return code, 0=>OK, else TABNDX or NDXINI error
C   Passed thru common:
C      CATOUT   I(*,*)   Catalog header block
C      NXBUFF   I(*)     Buffer for NX table
C      INXRNO   I        Pointer for next record, if  > 0 on input then
C                        create/open the file
C      NXKOLS   I(6)     The column pointer array in order
C      NXNUMV   I(6)     Element count in each column
C-----------------------------------------------------------------------
      INTEGER   FSTVIS(*), LSTVIS(*), IRET
      REAL      TIME, DT
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DUVH.INC'
C
      INTEGER   VER, LUNNX, IS, SUBA, JDSOUR
C-----------------------------------------------------------------------
      IF (NSTREM.NE.OSTREM) GO TO 999
C                                       Loop over streams
      DO 500 IS = 1,NSTREM
         SUBA = 1
         JDSOUR = OLDSOU(IS)
C                                       See if need to open
         VER = 1
         LUNNX = 29
         CALL NDXINI ('WRIT', NXBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNNX, INXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write record
         CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      TIME, DT, JDSOUR, SUBA, FSTVIS(IS), LSTVIS(IS),
     *      OLDFQI(IS), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Update FSTVIS
         FSTVIS(IS) = LSTVIS(IS) + 1
C                                       If more than one stream close
C                                       table.
         CALL TABIO ('CLOS', 0, INXRNO, NXBUFF, NXBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
 500     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FQOLD (CATFQ, IS, IRET)
C-----------------------------------------------------------------------
C   Routine checks to see if old file has FQ table and if so it
C   initializes the FQ comparison arrays.
C   Inputs:
C      CATFQ    I        Catalogue header
C      IS       I        Stream number
C   Outputs:
C      IRET     I        Error, 0 => OK
C   Outputs in common:
C      IFFREQ   D(*,2,*) FQ Frequency Hz, (FQ id, IF,stream)
C      TOTBW    R(*,2,*) FQ total bandwidth Hz.
C      CHBW     R(*,2,*) FQ channel seperation Hz.
C-----------------------------------------------------------------------
      INTEGER   CATFQ(256), IS, IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      INTEGER   LUNFQ, NUMIF, NUMFQE, I, FQSID(MAXIF), FQID, J, IFQN
      REAL      FQCHB(MAXIF), FQTBW(MAXIF)
      DOUBLE PRECISION FQFRQ(MAXIF)
      LOGICAL   TABLE, FQEXIS, FITASC
      CHARACTER FQBCOD(MAXIF)*8
C-----------------------------------------------------------------------
      IRET = 0
C                                       Does table exist
      LUNFQ = 40
      CALL ISTAB ('FQ', DISKO(IS), CNOOUT(IS), 1, LUNFQ, FQBUFF,
     *   TABLE, FQEXIS, FITASC, IRET)
      IF (.NOT. FQEXIS) THEN
         WRITE (MSGTXT,1000) IS
         IRET = 1
         GO TO 990
         END IF
C                                       Zero arrays first
      DO 50 I = 1,2
         DO 40 J = 1,MXFQE
            IFFREQ(J,I,IS) = 0.D0
            CHBW(J,I,IS)   = 0.0
            TOTBW(J,I,IS)  = 0.0
            BNDCOD(J,I,IS) = ' '
 40         CONTINUE
 50      CONTINUE
C                                       Open table and fill arrays
      CALL FQINI ('READ', FQBUFF, DISKO(IS), CNOOUT(IS), 1, CATFQ,
     *   LUNFQ, IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMFQE = FQBUFF(5)
      DO 200 I = 1,NUMFQE
         IFQN = I
         CALL TABFQ ('READ', FQBUFF, IFQN, FQKOLS, FQNUMV, NUMIF, FQID,
     *      FQFRQ, FQCHB, FQTBW, FQSID, FQBCOD, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 100 J = 1,NUMIF
            IFFREQ(FQID,J,IS) = FQFRQ(J)
            TOTBW(FQID,J,IS) = FQTBW(J)
            CHBW(FQID,J,IS) = FQCHB(J)
            BNDCOD(FQID,J,IS) = FQBCOD(J)
 100        CONTINUE
 200     CONTINUE
      CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FQOLD: FQ TABLE MISSING FOR STREAM ',I2)
      END
      SUBROUTINE FLMHIS (IS)
C-----------------------------------------------------------------------
C   Creates and fills a history file and creates and fills
C   the ANtenna file(s).
C   Inputs:
C      IS       I        Stream number
C-----------------------------------------------------------------------
      INTEGER   IS
C
      CHARACTER HILINE*72, ATIME*8, ADATE*12, TELE*8, OBSVR*8, TEXT*8,
     *   REA(9)*24
      INTEGER   LUNHI, IERR, TIME(3), DATE(3), J, ITRIM, IOPT, I
      LOGICAL   T
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      DATA REA /'Ignore Modcomp flags',
     *          'Backend Tsys except 4&p',
     *          'Holography all corrs',
     *          'Weight from NomSens',
     *          'Planet mode',
     *          'Pointing data incl',
     *          'On-line channel zero',
     *          'Ignore changing quals',
     *          'Omit failed ref pointing'/
      DATA LUNHI /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Create/open hist. file.
      CALL HICREA (LUNHI, DISKO(IS), CNOOUT(IS), CATBLK, BUF256,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT(IS), SEQOUT(IS), DISKO(IS),
     *   LUNHI, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       INTAPE, NFILES
      IF (DODISK) THEN
         J = ITRIM (INFILE)
         WRITE (HILINE,2000) TSKNAM, INFILE(:J)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,1999) TSKNAM, NFILES
      ELSE
         WRITE (HILINE,2001) TSKNAM, ITAPE, NFILES
         END IF
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BAND,QUAL
      WRITE (HILINE,2002) TSKNAM, BANDAC(1:1), MCQUAL, SELCOD
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       VLAMODE
      TEXT = MCOMOD
      IF (SOLAR) TEXT(1:1)='S'
      WRITE (HILINE,2008) TSKNAM, TEXT(1:2)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       VLA control program id
      WRITE (HILINE,2020) TSKNAM, MCCPID
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Array conf.
      WRITE (HILINE,2021) TSKNAM, MCARRC(1:2)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Correlator mode
      WRITE (HILINE,2022) TSKNAM, MCCORM
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AP options
      WRITE (HILINE,2023) TSKNAM, MCAPOP
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       User options
C                                       Timerange
      CALL HITIME (TSTART, TEND, LUNHI, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Channel range
      WRITE (HILINE,2025) TSKNAM, STBCH(IS), STECH(IS)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Integration time
      WRITE (HILINE,2009) TSKNAM, CPARM(1)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Ignore flags
      IF (CPARM(2).GT.0.0) THEN
         IOPT = CPARM(2) + 0.01
         DO 10 I = 1,9
            IF (MOD (IOPT,2).EQ.1) THEN
               WRITE (HILINE,2010) TSKNAM, CPARM(2), REA(I)
               CALL HIADD (LUNHI, HILINE, BUF256, IERR)
               IF (IERR.NE.0) GO TO 20
               END IF
            IOPT = IOPT / 2
 10         CONTINUE
         END IF
C                                       Max antenna status
      WRITE (HILINE,2012) TSKNAM, CPARM(3)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Shadow flagging
      WRITE (HILINE,2011) TSKNAM, CPARM(4)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Van Vleck correction
      IF (VLECK) THEN
         WRITE (HILINE,2017) TSKNAM
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Subarray
      WRITE (HILINE,2013) TSKNAM, CPARM(6)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       FQ entry tolerance
      WRITE (HILINE,2014) TSKNAM, CPARM(7)
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CL table increment
      WRITE (HILINE,2015) TSKNAM, 8, CPARM(8), 'CL'
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TY table increment
      WRITE (HILINE,2015) TSKNAM, 9, CPARM(9), 'TY'
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Cal averaging time
      IF (CPARM(10).NE.CPARM(1)) THEN
         WRITE (HILINE,2016) TSKNAM, CPARM(10)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Frequency selection
      IF ((DPARM(9).LE.1.0E10) .OR. (DPARM(10).LE.1.0E10)) THEN
         WRITE (HILINE,2030) TSKNAM, DPARM(1), DPARM(2)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2031) TSKNAM, DPARM(3), DPARM(4)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2032) TSKNAM, DPARM(5), DPARM(6)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2033) TSKNAM, DPARM(7), DPARM(8)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2034) TSKNAM, DPARM(9), DPARM(10)
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       CL table corrections
      IF (OPACIT.EQ.0) THEN
         HILINE = TSKNAM // ' No opacity correction in CL table.'
      ELSE IF (OPACIT.EQ.1) THEN
         WRITE (HILINE,'(A,F5.3)')
     *       TSKNAM // ' Opacity correction in CL table user '
     *       // 'specified: ', ZOPAC
      ELSE IF ((OPACIT.EQ.2) .OR. (OPACIT.EQ.4)) THEN
         HILINE = TSKNAM // ' Opacity correction in CL table weighted'//
     *            ' average of weather'
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,'(A,F4.2)') TSKNAM // '    and season.  weight'//
     *                             ' for weather = ', WTOPAC
         IF (OPACIT.EQ.4) THEN
            CALL HIADD (LUNHI, HILINE, BUF256, IERR)
            IF (IERR.NE.0) GO TO 20
            HILINE = TSKNAM // ' Old opacity model was used'
            END IF
      ELSE
         HILINE = TSKNAM // ' Unknown opacity correction type!'
         END IF
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
      IF (GNTYP.EQ.0) THEN
         HILINE = TSKNAM // ' No gain curve correction in CL table.'
      ELSE IF ((GNTYP.EQ.1) .OR. (GNTYP.EQ.2)) THEN
         HILINE = TSKNAM // ' Gain curve correction in CL table read' //
     *      ' from file:'
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         IF (IN2FIL.EQ.' ') THEN
            HILINE = TSKNAM // '    standard system VLA antenna ' //
     *         'gains file'
         ELSE
            HILINE = TSKNAM // '    ' // IN2FIL
            END IF
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         IF (GNTYP.EQ.1) THEN
            HILINE = TSKNAM // '    with variation as function of ' //
     *               'antenna and band'
         ELSE
            HILINE = TSKNAM // '   with variation as function of ' //
     *               'band only'
            END IF
      ELSE IF (GNTYP.EQ.3) THEN
         HILINE = TSKNAM // 'Gain curve correction in CL table based' //
     *            ' on user specified parms: '
         CALL HIADD (LUNHI, HILINE, BUF256, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,'(A,4(3X,F5.3))') TSKNAM // '    ',
     *                                   (BPARM(J),J=3,6)
      ELSE
         HILINE = TSKNAM // ' Unknown gain curve correction type!'
         END IF
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20

C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSVR)
      WRITE (HILINE,2006) TSKNAM, TELE, OBSVR
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Weighting
      IF (WTSYS.EQ.2) THEN
         HILINE = TSKNAM // '/ Weighted with nominal sensitivities' //
     *      ' times BButler fix'
      ELSE IF (WTSYS.EQ.1) THEN
         HILINE = TSKNAM // '/ Weighted with nominal sensitivities'
      ELSE
         HILINE = TSKNAM // '/ Weighted by integration time only'
         END IF
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Compression
      IF ((DOUVCM) .AND. (WTSYS.GT.0)) THEN
         HILINE = TSKNAM // '/ Data compressed, weighting info reduced'
      ELSE IF (DOUVCM) THEN
         HILINE = TSKNAM // '/ Data compressed on output, weights okay'
      ELSE
         HILINE = TSKNAM // '/ Data not compressed, no loss of weight'
     *      // ' info'
         END IF
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2007) TSKNAM, RLSNAM
      CALL HIADD (LUNHI, HILINE, BUF256, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUNHI, T, BUF256, IERR)
C                                       Write ANtenna file.
      CALL FLMANT (IS)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO(IS), CNOOUT(IS), CATBLK,
     *   'REST', BUF256, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1999 FORMAT (A6,'NFILES=',I5)
 2000 FORMAT (A6,'INFILE=''',A,'''')
 2001 FORMAT (A6,'INTAPE =',I3,', NFILES =',I5)
 2002 FORMAT (A6,'BAND = ''',A1,''', QUAL = ',I5,',  CALC = ''',A4,'''')
 2006 FORMAT (A6,'/ Telescope = ',A8,' Program = ',A)
 2007 FORMAT (A6,'RELEASE = ''',A7,'''')
 2008 FORMAT (A6,'VLAMODE =',1H',A2,1H')
 2009 FORMAT (A6,'CPARM(1) =', F7.3,' / Integration (sec)')
 2010 FORMAT (A6,'CPARM(2) =', F5.0,' / ',A)
 2011 FORMAT (A6,'CPARM(4) =', F5.0,
     *   ' / .lt. 0 => no shadow flag')
 2012 FORMAT (A6,'CPARM(3) =', F5.0,' / Max. IF stat. to pass')
 2013 FORMAT (A6,'CPARM(6) =', F5.0,' / Subarray')
 2014 FORMAT (A6,'CPARM(7) =', F10.3,' / FQ entry tolerance')
 2015 FORMAT (A6,'CPARM(',I1,') =', F7.2, ' / ',A2,
     *   ' table time increment')
 2016 FORMAT (A6,'CPARM(10) =', F7.2,
     *   ' / Cal. integration time (sec)')
 2017 FORMAT (A6,'/ Van Vleck correction applied')
 2020 FORMAT (A6,'/ Control program ID = ',A)
 2021 FORMAT (A6,'/ VLA conf. = ',A)
 2022 FORMAT (A6,'/ correlator mode = ',A)
 2023 FORMAT (A6,'/ AP options = ',A)
 2025 FORMAT (A6,'BCHAN =',I5,' ECHAN =',I5,' / Spectral channels')
 2030 FORMAT (A6,'DPARM(1)=',1PE12.5,' DPARM(2)=',E12.5,
     *   ' / A IF frequency')
 2031 FORMAT (A6,'DPARM(3)=',1PE12.5,' DPARM(4)=',E12.5,
     *   ' / B IF frequency')
 2032 FORMAT (A6,'DPARM(5)=',1PE12.5,' DPARM(6)=',E12.5,
     *   ' / C IF frequency')
 2033 FORMAT (A6,'DPARM(7)=',1PE12.5,' DPARM(8)=',E12.5,
     *   ' / D IF frequency')
 2034 FORMAT (A6,'DPARM(9)=',1PE12.5,' DPARM(10)=',E12.5,
     *   ' / AC, BD tolerance')
      END
      SUBROUTINE FLMTAB (BEGIN, WRITCL, WRITTY, WRITWX, WRITPO,
     *   WRITOF, IRET)
C-----------------------------------------------------------------------
C   Routine to process FQ, SU, CL, TY, WX, PO, and OF tables.  If
C   BEGIN=.true. then the FQ and SU tables are updated if necessary.
C   Input:
C      BEGIN    L   Process SU and FQ tables
C      WRITCL   L   Write to CL and OT tables
C      WRITTY   L   Write to TY tables
C      WRITWX   L   Write to WX tables
C      WRITPO   L   Write to PO tables
C      WRITOF   L   Write to OF tables
C   Input in common:
C   Output:
C      IRET     I        Return error code, 0=OK else failed.
C-----------------------------------------------------------------------
      LOGICAL   BEGIN, WRITCL, WRITTY, WRITWX, WRITPO, WRITOF
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (OSTREM.NE.NSTREM) GO TO 999
      IF (BEGIN) THEN
C                                       new FQ entry
         CALL FLMFQ (IRET)
         IF (IRET.NE.0) GO TO 999
C                                       check/update SU table
         CALL FLMSU (IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       new CL entry
      IF (WRITCL) THEN
         CALL FLMCL (IRET)
         IF (IRET.NE.0) GO TO 999
         ENDIF
C                                       new TY entry
      IF (WRITTY) THEN
         CALL FLMTY (IRET)
         IF (IRET.NE.0) GO TO 999
         ENDIF
C                                       new WX entry
      IF (WRITWX) THEN
         CALL FLMWX (IRET)
         IF (IRET.NE.0) GO TO 999
         ENDIF
C                                       new PO entry
      IF (WRITPO) THEN
         CALL FLMPO (IRET)
         IF (IRET.NE.0) GO TO 999
         ENDIF
C                                       new OF entry
      IF (WRITOF) THEN
         CALL FLMOF (IRET)
         IF (IRET.NE.0) GO TO 999
         ENDIF
C
 999  RETURN
      END
      SUBROUTINE FLMFQ (IRET)
C-----------------------------------------------------------------------
C   Creates and writes the FQ table, only writes new entry when needed.
C   Input from common:
C      IFQRNO   I        Next FQ record in table
C      FQBUFF   I        Buffer, etc. for FQ file
C      CPARM    R(10)    User specified array
C                                   7 => FQ entry tolerance
C   Input from MODCOMP common:
C      NSTREM   I        Number of output streams required for data
C      STNOIF   I(*)     Number of IFs in each stream
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)     VLA reference IF for 2nd IF of stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      MCSKYF   D(4)     Sky frequency (GHz)
C      MCBANW   R(4)     Bandwidth (A,B,C,D) Hz
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C   Output in common:
C      CURFQI   I(*)     Current FQ id number per stream
C      IFFREQ   D(*,2,*) FQ Frequency Hz (FQ id, IF, stream)
C      TOTBW    R(*,2,*) FQ total bandwidth Hz (ditto)
C      CHBW     R(*,2,*) FQ channel seperation Hz (ditto)
C   Output:
C      IRET     I        Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INTEGER   IS, VER, LUNFQ, IR1, IR2, NUMFQE, FQSID(MAXIF), FQID, I,
     *   NUMIF, IIPT, IRTEMP(2), NFR, FR1, FR2, JR1, JR2
      REAL      FQCHB(MAXIF), FQTBW(MAXIF), BW1, BW2, CH1, CH2
      DOUBLE PRECISION FQFRQ(MAXIF), SKYFHZ(2), VELDEL, FTEMP, FF,
     *   FRQDEL(2), VDOPP
      LOGICAL   GIVMES
      CHARACTER FQBCOD(MAXIF)*8, BNDC*1
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (IRTEMP, FTEMP)
C
      SAVE GIVMES
C
      DATA GIVMES / .TRUE. /
C                                       Max. velocity difference due to
C                                       Doppler tracking (m/s)
      DATA VDOPP  / 34000.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      VELDEL = VDOPP / VELITE
C                                       Loop over streams
      DO 500 IS = 1,NSTREM
         NUMIF = STNOIF(IS)
C                                       See if need to open
         VER = 1
         LUNFQ = 40
         CALL FQINI ('WRIT', FQBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNFQ, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         NUMFQE = FQBUFF(5)
C                                       Init. comparison arrays
C                                       if necessary
         IF (NUMFQE.EQ.0) THEN
            DO 50 I=1,MXFQE
               IFFREQ(I,1,IS) = 0.0D0
               IFFREQ(I,2,IS) = 0.0D0
               TOTBW(I,1,IS) = 0.0
               TOTBW(I,2,IS) = 0.0
               CHBW(I,1,IS) = 0.0
               CHBW(I,2,IS) = 0.0
               BNDCOD(I,1,IS) = ' '
               BNDCOD(I,2,IS) = ' '
 50            CONTINUE
            NUMFQE = 1
            END IF
         IF (NUMFQE.GT.MXFQE) THEN
            IRET = 1
            WRITE (MSGTXT,1000) NUMFQE, MXFQE
            GO TO 990
            END IF
C                                       See if need new entry
         IR1 = STMCI1(IS)
         IR2 = STMCI2(IS)
C                                       Get reference frequency from
C                                       header using EQUIVALENCE.
         IIPT = ((KDCRV+JLOCF-1) * NWDPDP) + 1
         CALL COPY (NWDPDP, CATOUT(IIPT,IS), IRTEMP)
         SKYFHZ(1) = ((MCSKYF(IR1) * 1.0D9) - FTEMP)
         SKYFHZ(2) = ((MCSKYF(IR2) * 1.0D9) - FTEMP)
C                                       Bandwidth and channel separation
         BW1 = MCBANW(IR1)
         BW2 = MCBANW(IR2)
         CH1 = MCHSEP(IR1)
         CH2 = MCHSEP(IR2)
C                                       Continuum
         IF (STTYPE(IS).EQ.0) THEN
            CH1 = BW1
            CH2 = BW2
C                                       Channel 0
         ELSE IF (STTYPE(IS).EQ.1) THEN
            JR1 = STMCI1(IS+1)
            NFR = STNOCH(IS+1)
            FR1 = (NFR+1) / 8 + 1 + STZMOD(IS+1)
            FR2 = NFR - ((NFR+1)/8)
            CH1 = (FR2 - FR1 + 1) * MCHSEP(JR1)
            JR2 = STMCI2(IS+1)
            CH2 = (FR2 - FR1 + 1) * MCHSEP(JR2)
            END IF
C                                       Set FQ tolerance
C                                       Spectral line
         IF (STTYPE(IS).NE.0) THEN
            IF (CPARM(7).LT.0.0) THEN
               FRQDEL(1) = 1.0D20
               FRQDEL(2) = 1.0D20
            ELSE IF (CPARM(7).EQ.0.0) THEN
               FRQDEL(1) = MCSKYF(IR1) * 1.0D9 * VELDEL
               FRQDEL(2) = MCSKYF(IR2) * 1.0D9 * VELDEL
            ELSE IF (CPARM(7).GT.0.0) THEN
               FRQDEL(1) = CPARM(7) * 1.0D3
               FRQDEL(2) = CPARM(7) * 1.0D3
               END IF
C                                       Continuum
         ELSE
            IF (CPARM(7).LT.1.0E-6) THEN
               FRQDEL(1) = 1.0D5
               FRQDEL(2) = 1.0D5
            ELSE
               FRQDEL(1) = CPARM(7) * 1.0D3
               FRQDEL(2) = CPARM(7) * 1.0D3
               END IF
            END IF
C
         IF (GIVMES) THEN
            IF (FRQDEL(1).GT.1.0D19) THEN
               WRITE (MSGTXT,1010)
            ELSE IF (NUMIF.EQ.1) THEN
               WRITE (MSGTXT,1020) FRQDEL(1)/1.0D3
            ELSE
               WRITE (MSGTXT,1020) FRQDEL(1)/1.0D3, FRQDEL(2)/1.0D3
               END IF
            CALL MSGWRT (6)
            GIVMES = .FALSE.
            END IF
C                                       Check against table entries
         DO 100 I = 1,NUMFQE
            IF (NUMIF.EQ.1) THEN
               IF ((ABS(SKYFHZ(1)-IFFREQ(I,1,IS)).LE.FRQDEL(1)) .AND.
     *            (ABS(BW1-TOTBW(I,1,IS)).LE.1.E-4*BW1) .AND.
     *            (ABS(CH1-CHBW(I,1,IS)).LE.1.E-4*CH1)) GO TO 200
            ELSE
               IF ((ABS(SKYFHZ(1)-IFFREQ(I,1,IS)).LE.FRQDEL(1)) .AND.
     *            (ABS(SKYFHZ(2)-IFFREQ(I,2,IS)).LE.FRQDEL(2)) .AND.
     *            (ABS(BW1-TOTBW(I,1,IS)).LE.1.E-4*BW1) .AND.
     *            (ABS(BW2-TOTBW(I,2,IS)).LE.1.E-4*BW2) .AND.
     *            (ABS(CH1-CHBW(I,1,IS)).LE.1.E-4*CH1)  .AND.
     *            (ABS(CH2-CHBW(I,2,IS)).LE.1.E-4*CH2)) GO TO 200
               END IF
 100        CONTINUE
C                                       Write a new entry
C                                       Copy necessary parms into
C                                       FQ arrays
         FQFRQ(1) = SKYFHZ(1)
         FQFRQ(2) = SKYFHZ(2)
         FQCHB(1) = CH1
         FQCHB(2) = CH2
         FQTBW(1) = BW1
         FQTBW(2) = BW2
         FQSID(1) = 1
         FQSID(2) = 1
         FQID     = IFQRNO
         FF = MCSKYF(IR1) * 1.0D9
         CALL FLMBC (FF, BNDC)
         FQBCOD(1) = 'VLA_' // BNDC
         FF = MCSKYF(IR2) * 1.0D9
         CALL FLMBC (FF, BNDC)
         FQBCOD(2) = 'VLA_' // BNDC
         CALL TABFQ ('WRIT', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, FQFRQ, FQCHB, FQTBW, FQSID, FQBCOD, IRET)
         IF (IRET.NE.0) GO TO 999
         CURFQI(IS) = FQID
C                                       Fill in the comparison arrays
         IFFREQ(FQID,1,IS) = FQFRQ(1)
         IFFREQ(FQID,2,IS) = FQFRQ(2)
         TOTBW(FQID,1,IS) = FQTBW(1)
         TOTBW(FQID,2,IS) = FQTBW(2)
         CHBW(FQID,1,IS) = FQCHB(1)
         CHBW(FQID,2,IS) = FQCHB(2)
         BNDCOD(FQID,1,IS) = FQBCOD(1)
         BNDCOD(FQID,2,IS) = FQBCOD(2)
         GO TO 300
C
 200     CURFQI(IS) = I
C                                       Close the table
 300     CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
 500     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLMFQ: ',I4,' rows in FQ table > maximum of ',I4)
 1010 FORMAT ('FLMFQ: All frequencies will have the same FQ number')
 1020 FORMAT ('FLMFQ: FQ entry tolerance = ',2(1PD12.3,:),' kHz')
      END
      SUBROUTINE FLMSU (IRET)
C-----------------------------------------------------------------------
C   Routine to write source info to SU table.
C   Input from common:
C      ISURNO   I        Next source record in table
C      SUBUFF   I        Buffer etc. for source file
C      NSOUR    I(*)     Number of sources in source list
C      SULIST   C(*,*)*16 Names of sources in list per stream
C      IDSOUR   I(*,4)   Ids of sources on list
C      SUCORD   D(2,*,*) Source EPO coordinates in radians
C      DOCORD   L        Planet/Sun indicator
C   Input from MODCOMP common:
C      MCSNAM   C*16     Source name
C      MCQUAL   I        Source qualifier
C      MCALCD   C*2      Calibrator code (character)
C      NSTREM   I        Number of output streams required for data
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)     VLA reference IF for 2nd IF of stream
C      STNOCH   I(*)     No. Channels in each stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      MCRAEP   D        Right Ascension at standard epoch (radians)
C      MCDCEP   D        Declination at standard epoch (radians)
C      MCRAAP   D        Apparent Right Ascension (radians)
C      MCDCAP   D        Apparent Declination (radians)
C      MCEPOC   I        Epoch year
C      MCZSF    R        Zero spacing flux (IF?)
C      MCRVEL   D(4)     Radial velocity for IFs A-D (Km/sec)
C      MCREST   D(4)     Line rest Frequency for IFs A-D (MHz)
C      MCVRF    C*8      Velocity rest frame
C      MCBANW   R(4)     Bandwidth (A,B,C,D) Hz
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      AVGIAT   D        Average IAT of current integration
C    Output in common:
C      CURSOU   I(*)     Current source ID number per stream
C      ISURNO   I        Next source record in table
C      NSOUR    I(*)     Number of sources in source list
C      SULIST   C(*,*)*16 Names of sources in list
C      SOURID   I(*,*)   Ids of sources on list
C      SUFREQ   D(2,*,*) Source frequency offset
C      SUQUAL   I(*,*)   Source qualifier
C      SUMOVE   L(*,*)   Source motion indicator
C    Output:
C      IRET     I        Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      CHARACTER SORNAM*16, CALCOD*4, TSIGN*1, STRNG*4, JVELTY*1,
     *   VELRF(4)*1, VELDF(3)*1, CHVEL(6)*8, JVELRF*1, LVELTY*8,
     *   LVELDE*8, KEYW(2)*8
      INTEGER   I, IDSOU, IQUAL, LUNSU, IR1, IR2, VER, IS, SLIMIT, IIPT,
     *   IRTEMP(2), SUFQID, TIT(3), ISPOSS, IPNT, IVELTY, IVELDF, NFR,
     *   FR1, FR2, KLOCS(2), KEYTYP(2), JR1
      REAL      FLUX(4,2), BWTEMP, TTIME, TITSEC, POLAR(2)
      HOLLERITH KEYVAL(4)
      DOUBLE PRECISION  BANDWW, RAEPO, DECEPO, RAAPP, DECAPP, EPOCH,
     *   LSRVEL(2), PMRA, PMDEC, FREQO(2), RESTFQ(2), TIMED, JD0,
     *   CURTTF(2,MXSTRM), FTEMP, OBSPOS(3), RAOBS, DECOBS
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (IRTEMP, FTEMP)
      DATA POLAR /2 * 0.0/
      DATA OBSPOS /3 * 0.0D0/
      DATA VELRF, VELDF /'G','T','B','L', 'V','Z','O'/
      DATA CHVEL /'GEOCENTR', 'TOPOCENT', 'BARYCENT',
     *   'LSR     ', 'RADIO   ', 'OPTICAL '/
      DATA KEYW /'VELTYP','VELDEF'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Send user a message.
      TIMED = AVGIAT
      TTIME = SNGL (AVGIAT)
      CALL TFDHMS (TTIME, 1, TSIGN, TIT, TITSEC)
      BWTEMP = MCBANW(1) * 1.0E-6
      WRITE (MSGTXT,1050) MCSNAM, MCQUAL, BWTEMP, TIT, TITSEC
      IF (MSGTXT(59:59).EQ.' ') MSGTXT(59:59) = '0'
      IF (MSGTXT(60:60).EQ.' ') MSGTXT(60:60) = '0'
      CALL MSGWRT (4)
      RAEPO = MCRAEP * RAD2DG
      DECEPO = MCDCEP * RAD2DG
      IF (IGQUAL) THEN
         IQUAL = 0
      ELSE
         IQUAL = MCQUAL
         END IF
C                                       Loop over stream
      DO 100 IS = 1,NSTREM
C                                       Get source info.
         SLIMIT = NSOUR(IS)
C                                       new SU table entry?
         ISPOSS = 0
         IF (SLIMIT.GT.0) THEN
            DO 10 I = 1,SLIMIT
               CURSOU(IS) = SOURID(I,IS)
               IF ((SULIST(I,IS)(:14).EQ.MCSNAM(:14)) .AND.
     *             (SUQUAL(I,IS).EQ.IQUAL)) THEN
                  IF ((.NOT.DOCORD) .OR.
     *                ((ABS(SUPOS(1,I,IS)-RAEPO).LT.1.0D-6) .AND.
     *                 (ABS(SUPOS(2,I,IS)-DECEPO).LT.1.0D-6))) GO TO 100
                  ISPOSS = I
                  END IF
 10            CONTINUE
            END IF
C                                       New source
         NSOUR(IS) = NSOUR(IS) + 1
         IDSOU = NSOUR(IS)
C                                       Rename?
         IF (ISPOSS.GT.0) THEN
            CALL ZEHEX (IDSOU, 2, STRNG)
            IF (STRNG(:2).NE.MCSNAM(15:)) THEN
               MSGTXT = 'COORDINATES INCONSISTENT FOR ' // MCSNAM
               CALL MSGWRT (7)
               MCSNAM(15:) = STRNG(:2)
               MSGTXT = 'RENAMED TO ' // MCSNAM
               CALL MSGWRT (7)
               END IF
            END IF
         CURSOU(IS) = IDSOU
         SOURID(IDSOU,IS) = CURSOU(IS)
         SORNAM = MCSNAM
         SULIST(IDSOU,IS) = MCSNAM
         SUQUAL(IDSOU,IS) = IQUAL
         IR1 = STMCI1(IS)
         IR2 = STMCI2(IS)
C                                       Get reference frequency from
C                                       header using EQUIVALENCE.
         IIPT = ((KDCRV+JLOCF-1) * NWDPDP) + 1
         CALL COPY (NWDPDP, CATOUT(IIPT,IS), IRTEMP)
         CURTTF(1,IS) = FTEMP + IFFREQ(CURFQI(IS),1,IS)
         CURTTF(2,IS) = FTEMP + IFFREQ(CURFQI(IS),2,IS)
         SUFREQ(1,IDSOU,IS) = MCSKYF(IR1) * 1.0D9 - CURTTF(1,IS)
         SUFREQ(2,IDSOU,IS) = MCSKYF(IR2) * 1.0D9 - CURTTF(2,IS)
         FLUX(1,1) = 0.0
         FLUX(2,1) = 0.0
         FLUX(3,1) = 0.0
         FLUX(4,1) = 0.0
         FLUX(1,2) = 0.0
         FLUX(2,2) = 0.0
         FLUX(3,2) = 0.0
         FLUX(4,2) = 0.0
         CALCOD = MCALCD(1:1) // '   '
C                                       Position
         RAAPP = MCRAAP * RAD2DG
         DECAPP = MCDCAP * RAD2DG
         SUPOS(1,IDSOU,IS) = RAEPO
         SUPOS(2,IDSOU,IS) = DECEPO
         EPOCH = MCEPOC
         SUMOVE(IDSOU,IS) = SUMOVE(IDSOU,IS) .OR.
     *      ((.NOT.DOCORD) .AND. (EPOCH.EQ.-1))
         IR1 = STMCI1(IS)
         IR2 = STMCI2(IS)
         IF (STTYPE(IS).EQ.2) THEN
            BANDWW = MCHSEP(IR1)
         ELSE IF (STTYPE(IS).EQ.1) THEN
            JR1 = STMCI1(IS+1)
            NFR = STNOCH(IS+1)
            FR1 = (NFR+1) / 8 + 1 + STZMOD(IS+1)
            FR2 = NFR - ((NFR+1)/8)
            BANDW = (FR2 - FR1 + 1) * MCHSEP(JR1)
         ELSE
            BANDW = MCBANW(IR1)
            END IF

         FREQO(1) = SUFREQ(1,IDSOU,IS)
         FREQO(2) = SUFREQ(2,IDSOU,IS)
C                                       force our idea of apparent
         IF (.NOT.SUMOVE(IDSOU,IS)) THEN
            JD0 = REFMJD + 2400000.5D0
            CALL JPRECS (JD0, EPOCH, 1.0D-6, 1, .TRUE., OBSPOS, POLAR,
     *         DG2RAD * RAEPO, DG2RAD * DECEPO, RAAPP, DECAPP)
            RAAPP = RAAPP * RAD2DG
            DECAPP = DECAPP * RAD2DG
            END IF
C                                       Line data?
         IPNT = STMCI1(IS)
         IPNT = (IPNT-1) * 2 + 2
         JVELTY = MCVRF(IPNT:IPNT) // ' '
C                                       spectral line w Doppler track
         IF ((STTYPE(IS).NE.0) .AND. (JVELTY.NE.'F') .AND.
     *      (JVELTY.NE.' ')) THEN
C                                       LSR velocity
            LSRVEL(1) = MCRVEL(IR1) * 1.0D3
            LSRVEL(2) = MCRVEL(IR2) * 1.0D3
C                                       Line rest frequency
            RESTFQ(1) = MCREST(IR1) * 1.0D6
            RESTFQ(2) = MCREST(IR2) * 1.0D6
C                                       continuum, line w/o tracking
         ELSE
C                                       Hide silliness on tape
            LSRVEL(1) = 0.0D0
            LSRVEL(2) = 0.0D0
            RESTFQ(1) = 0.0D0
            RESTFQ(2) = 0.0D0
            END IF
         PMRA = 0.0D0
         PMDEC = 0.0D0
C                                       Open table
         LUNSU = 28
         INOGRP = STNOIF(IS)
         VER = 1
         SUFQID = -1
         CALL SOUINI ('WRIT', SUBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNSU, INOGRP, VELTYP, VELDEF, SUFQID, ISURNO,
     *      SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       velocity type and def
         LVELTY = ' '
         LVELDE = ' '
C                                       Decode velocity reference
C                                       frame, definition
         IF ((VELTYP.EQ.' ') .AND. (VELDEF.EQ.' ')) THEN
            IR1 = STMCI1(IS)
            IPNT = (IR1-1) * 2 + 1
            JVELRF = MCVRF(IPNT:IPNT)
            IVELTY = 0
            IF (JVELRF.EQ.VELRF(1)) IVELTY = 1
            IF (JVELRF.EQ.VELRF(2)) IVELTY = 2
            IF (JVELRF.EQ.VELRF(3)) IVELTY = 3
            IF (JVELRF.EQ.VELRF(4)) IVELTY = 4
            IF (IVELTY.GT.0) THEN
               LVELTY = CHVEL(IVELTY)
            ELSE
               LVELTY = '        '
               END IF
C                                       Definition
            IPNT = (IR1-1) * 2 + 2
            JVELTY = MCVRF(IPNT:IPNT)
            IVELDF = 0
            IF (JVELTY.EQ.VELDF(1)) IVELDF = 1
            IF (JVELTY.EQ.VELDF(2)) IVELDF = 2
C                                       Ignore 'O = offset'
            IF (IVELDF.GT.0) THEN
               LVELDE = CHVEL(IVELDF+4)
            ELSE
               LVELDE = '        '
               END IF
C                                       User set frequency => velocity
C                                       info is crap
            IF ((JVELTY.EQ.'F ') .OR. (JVELTY.EQ.'  ')) THEN
               LVELTY = ' '
               LVELDE = ' '
               END IF
            END IF
C                                       Write if just changed
         IF ((LVELDE.NE.' ') .OR. (LVELTY.NE.' ')) THEN
            KLOCS(1) = 1
            KLOCS(2) = 3
            KEYTYP(1) = 3
            KEYTYP(2) = 3
            CALL CHR2H (8, LVELTY, 1, KEYVAL(KLOCS(1)))
            CALL CHR2H (8, LVELDE, 1, KEYVAL(KLOCS(2)))
            CALL TABKEY ('WRIT', KEYW, 2, SUBUFF, KLOCS, KEYVAL,
     *         KEYTYP, IRET)
            END IF
         RAOBS = RAEPO
         DECOBS = DECEPO
         CALL TABSOU ('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SORNAM, IQUAL, CALCOD, FLUX, FREQO, BANDWW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       If more than one stream close
C                                       table.
         CALL TABIO ('CLOS', 0, ISURNO, SUBUFF, SUBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Found ',A,':',I5,F7.3,' MHz at IAT ',I3,'/ ',2(I2.2,':'),
     *   F4.1)
      END
      SUBROUTINE FLMPO (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill PO table
C   Input from common:
C      CURSOU   I(*)     Current source per stream
C      CATOUT   I(*,*)   Catalog header block per stream
C      DISKO    I(*)     Disk number per stream
C      CNOOUT   I(*)     Output CNO per stream
C      NSTREM   I        Number of output streams required for data
C      MCRAAP   D        Apparent Right Ascension (radians)
C      MCDCAP   D        Apparent Declination (radians)
C      MCIATG   D        IAT of geometry computations
C      MCIATI   D        IAT at middle of integration (radians)
C      OBSDAT   C*8      Observing date
C    Output:
C      IRET     I        Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DPOV.INC'
      CHARACTER ODATE*8
      LOGICAL FIRST, ALREDY
      INTEGER LUNPO, POVER, IS, DOFF, IPORNO, POREV, POKOLS(MAXPOC),
     *   PONUMV(MAXPOC), POBUFF(512), NTIME(MXSTRM), ITIME
      DOUBLE PRECISION RAAPP, DECAPP, SDIST, TIMED(10000,MXSTRM), TIME2,
     *   XSCAL
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      SAVE TIMED, NTIME, FIRST
C
      DATA FIRST / .TRUE. /
C-----------------------------------------------------------------------
      IRET = 0
      IF (FIRST) THEN
         DO 100 IS = 1, MXSTRM
            NTIME(IS) = 0
            DO 200 ITIME = 1, 10000
               TIMED(ITIME,IS) = -1.0D0
 200           CONTINUE
 100        CONTINUE
         FIRST = .FALSE.
         END IF
      XSCAL = 1.0D0 / MAX (1, MCLNF1)
C                                       Set time of the Current MC
C                                       integration.
      DOFF = MCDATE - IDNINT(REFMJD)
C this should work, but what's written on tape is wrong :(...
C     TIME2 = DOFF + MCIATG / TWOPI
C so, to deal with it, go through the following contortion...
C see notes in /home/planetas/solarsystem/comets/posncorr/my.13sep2001
C     convert MCIATI to seconds; add small slop to prevent precision
C     problems:
C     TIME2 = 86400.0D0 * MCIATI / TWOPI + 0.01D0
      TIME2 = 86400.0D0 * (AVGIAT * XSCAL - DOFF) + 0.01D0
C truncate to previous 10 sec tick; convert to fractional days;
C add day offset:
      TIME2 = DOFF + (TIME2 - DMOD(TIME2, 10.0D0)) / 86400.0D0
      ODATE = OBSDAT
C                                       Position
C     RAAPP = MCRAAP * RAD2DG
C     DECAPP = MCDCAP * RAD2DG
      RAAPP = AVRAAP * RAD2DG * XSCAL
      DECAPP = AVDEAP * RAD2DG * XSCAL
C                                       Distance (not on archive tape)
      SDIST = 0.0D0
C                                       Loop over stream
      DO 300 IS = 1,NSTREM
C                                       Is this a moving source?
         IF (SUMOVE(CURSOU(IS),IS)) THEN
C note here that strictly speaking, TIMED should be a function of
C source also, but i presume that you can't observe 2 different
C sources within the same 10 sec. tick anyway.
            ALREDY = .FALSE.
            DO 400 ITIME = 1, NTIME(IS)
               IF (DABS(TIME2-TIMED(ITIME,IS)).LT.1.D-9) ALREDY = .TRUE.
 400           CONTINUE
            IF (.NOT. ALREDY) THEN
C                                       Open and write to table
               NTIME(IS) = NTIME(IS) + 1
               TIMED(NTIME(IS),IS) = TIME2
               LUNPO = 28
               POVER = 1
               CALL POINI ('WRIT', POBUFF, DISKO(IS), CNOOUT(IS), POVER,
     *            CATOUT(1,IS), LUNPO, IPORNO, POKOLS, PONUMV, ODATE,
     *            POREV, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL TABPO ('WRIT', POBUFF, IPORNO, POKOLS, PONUMV,
     *            TIME2, CURSOU(IS), RAAPP, DECAPP, SDIST, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       If more than one stream close
C                                       table.
               CALL TABIO ('CLOS', 0, IPORNO, POBUFF, POBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
 300     CONTINUE
C                                       Set time for next
      TIME2 = (MCIATI / TWOPI) + DOFF
C                                       subtract .05 sec
      TIMNPO = SNGL (TIME2) + TIMPOI - 5.787E-7
C
 999  RETURN
      END
      SUBROUTINE FLMWX (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill WX table
C   Input from common:
C      CURSOU   I(*)     Current source per stream
C      CATOUT   I(*,*)   Catalog header block per stream
C      DISKO    I(*)     Disk number per stream
C      CNOOUT   I(*)     Output CNO per stream
C      NSTREM   I        Number of output streams required for data
C      SELPGM   C*6      Observing code
C      OBSDAT   C*8      Observing date
C      MCACB    I        Array control bits
C                        bit 1 -> weather station broken
C                        bit 5 -> weather info input by operator
C      MCWEAT   R(5)     Weather info:
C                        1 - wind speed (m/s)
C                        2 - wind direction (East from North)
C                        3 - surface temperature (C)
C                        4 - surface pressure (mbar)
C                        5 - dew point temperature (C)
C      REFMJD   D        Reference MJD (mod. Julian date).
C      MCDATE   I        Current MJD.
C      MCIATI   D        IAT at middle of integration (radians)
C      MCINTG   I        Integration time in waveguide cycles (52 msec).
C      TIMWXI   R        Time interval (days)
C    Output:
C     IRET      I        Return error code, 0=>OK, otherwise abort.
C     TIMNWX    R        Time for next record
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DWXV.INC'
      DOUBLE PRECISION TIMED
      INTEGER   LUNWX, VER, IS, WXBUFF(512), IWXRNO, WXKOLS(MAXWXC),
     *   WXNUMV(MAXWXC), TABVER, BITS(32), SUBA
      LOGICAL ISBROK, ISOPER
      REAL      WEATHR(5)
      CHARACTER OBSCOD*8, ODATE*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Weather data only exists on
C                                       tapes with revision >= 4.
      IF (MCFREV.LT.4) GO TO 999
C                                       Is there valid weather data?
      CALL ZGTBIT (32, MCACB, BITS)
      ISBROK = BITS(31).EQ.1
      ISOPER = BITS(27).EQ.1
      TIMED = AVGIAT
      DO 10 IS = 1,5
         WEATHR(IS) = AVWEAT(IS) / MAX (1, MCLNF1)
 10      CONTINUE
      TIMED = TIMED / MAX (1, MCLNF1)
C                                       most conservative.  could allow
C                                       it to go through on operator
C                                       input data, but i don't.
      IF (.NOT.ISBROK) THEN
         SUBA = 1
C                                       Loop over stream
         DO 100 IS = 1,NSTREM
C                                       Open and write to table
            LUNWX = 28
            VER = 1
            TABVER = IWXREV
            OBSCOD = SELPGM
            ODATE = OBSDAT
            CALL WXINI ('WRIT', WXBUFF, DISKO(IS), CNOOUT(IS), VER,
     *         CATOUT(1,IS), LUNWX, IWXRNO, WXKOLS, WXNUMV, OBSCOD,
     *         ODATE, TABVER, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL TABWX ('WRIT', WXBUFF, IWXRNO, WXKOLS, WXNUMV, TIMED,
     *         SUMINT/86400., 0, SUBA, WEATHR(3), WEATHR(4), WEATHR(5),
     *         WEATHR(1), WEATHR(2), 0.0, 0.0, 0.0, 0.0, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       If more than one stream close
C                                       table.
            CALL TABIO ('CLOS', 0, IWXRNO, WXBUFF, WXBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
 100        CONTINUE
         END IF
C                                       Set time for next
      TIMNWX = SNGL (TIMED) + TIMWXI - 5.787E-7
C
 999  RETURN
      END
      SUBROUTINE FLMOF (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill OF (Online Flags) table
C   Input from common:
C      CURSOU   I(*)     Current source per stream
C      CURFQI   I(*)     FQ id per stream.
C      CATOUT   I(*,*)   Catalog header block per stream
C      DISKO    I(*)     Disk number per stream
C      CNOOUT   I(*)     Output CNO per stream
C      NSTREM   I        Number of output streams required for data
C      SELPGM   C*6      Observing code
C      OBSDAT   C*8      Observing date
C      MCANTS   L(28)    If true, antenna in subarray
C      MCANCB   I        Array control bits
C      REFMJD   D        Reference MJD (mod. Julian date).
C      MCDATE   I        Current MJD.
C      MCIATI   D        IAT at middle of integration (radians)
C      SUMINT   R        Sum of integration time (seconds)
C      MCFREV   I        Tape revision.
C    Output:
C     IRET      I        Return error code, 0=>OK, otherwise abort.
C     TIMNWX    R        Time for next record
C   BIT PATTERN MEANING:
C        1   reference pointing requested, but not applied
C        2   antenna shadowed at source change time
C        4   antenna off source
C        8   first LO not locked
C       16   Tsys fluctuating
C       32   antenna flagged bad by operator
C       64   back-end total power out of range
C      128   back-end filters mis-set
C      256   L8 module not locked
C      512   L6 module not locked
C     1024   sub-reflector not in position
C     2048   source change in progress
C     4096   phase switching disabled
C     8192   round-trip phase correction disabled
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DOFV.INC'
      REAL      TIME
      INTEGER   LUNOF, VER, IS, OFBUFF(512), IOFRNO, OFKOLS(MAXOFC),
     *   OFNUMV(MAXOFC), TABVER, BITS(32), IANT, ISUB, II, NIF, NUMPOL,
     *   LBITS(32), ANFLAG, STATUS(2,2), ZOR
      CHARACTER OBSCOD*8, ODATE*8, ANNAME*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
      ANNAME = 'VLA'
C                                       refn. ptg. bit only defined
C                                       in the same way as current
C                                       for revision >= 25
C     IF (MCFREV.LT.25) GO TO 999
C                                       Set time of the Current MC
      TIME = AVGIAT / MAX (1, MCLNF1)
C                                       Loop over stream
      DO 100 IS = 1,NSTREM
C                                       Open and write to table
         LUNOF = 28
         VER = 1
         TABVER = OFREV
         OBSCOD = SELPGM
         ODATE = OBSDAT
         NUMPOL = STNOPL(IS)
         IF (NUMPOL.GT.2) NUMPOL = 2
         NIF = STNOIF(IS)
         CALL OFINI ('WRIT', OFBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNOF, IOFRNO, OFKOLS, OFNUMV, NIF, NUMPOL,
     *      ANNAME, OBSCOD, ODATE, TABVER, IRET)
         IF (IRET.NE.0) GO TO 999
         ISUB = 1
         DO 200 IANT = 1,MXANT
C                                       Skip antennas not in subarray
            IF (.NOT.MCANTS(IANT)) GO TO 200
C                                       check the flags.
C           CALL ZGTBIT (32, MCANCB(IANT), BITS)
            CALL ZGTBIT (32, AVANCB(IANT), BITS)
            CALL FILL (32, 0, LBITS)
C                                       reference pointing.
            LBITS(1) = BITS(21)
C                                       shadowing.
            LBITS(2) = BITS(22)
C                                       off source.
            LBITS(3) = ZOR (BITS(14), BITS(26))
C                                       other errors (see subr OFLAGS).
            LBITS(4) = BITS(7)
            LBITS(5) = BITS(8)
            LBITS(6) = BITS(9)
            LBITS(7) = BITS(10)
            LBITS(8) = BITS(11)
            LBITS(9) = BITS(12)
            LBITS(10) = BITS(13)
            LBITS(11) = BITS(15)
            LBITS(12) = BITS(16)
            LBITS(13) = BITS(24)
            LBITS(14) = BITS(27)
            CALL ZPTBIT (32, ANFLAG, LBITS)
            II = AVSTAT(IANT)
            STATUS(2,2) = MOD (II, 16)
            II = II / 16
            STATUS(2,1) = MOD (II, 16)
            II = II / 16
            STATUS(1,2) = MOD (II, 16)
            II = II / 16
            STATUS(1,1) = MOD (II, 16)
            IF ((ANFLAG.NE.0) .OR. (AVSTAT(IANT).NE.0)) THEN
               CALL TABOF ('WRIT', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME,
     *            CURSOU(IS), IANT, ISUB, CURFQI(IS), ANFLAG, STATUS,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 200        CONTINUE
C                                       Close all open tables
         CALL TABIO ('CLOS', 0, IOFRNO, OFBUFF, OFBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C
      TIMNOF = TIME + TIMOFI - 5.787E-7
 999  RETURN
      END
      SUBROUTINE FLMCL (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill CL table and optionally OT table
C   Input from common:
C      REFMJD   D        Reference MJD (mod. Julian date).
C      MCDATE   I        Current MJD.
C      MCIATI   D        IAT at middle of integration (radians)
C      MCINTG   I        Integration time in waveguide cycles (52 msec).
C      MCANTS   L(28)    If true, antenna in subarray
C      MCSCOS   R(6)     Sin and cos of h, A and eta
C      MCFREV   I        Format revision level
C      MCACB    I        Array control bits
C      MCWEAT   R(5)     Weather info:
C                        3 - surface temperature (C)
C                        4 - surface pressure (mbar)
C                        5 - dew point temperature (C)
C      TIMCLI   R        Time interval (days)
C      CURSOU   I(*)     Source number per stream
C      CURFQI   I(*)     FQ id per stream.
C      DISKO    I(*)     Disk number of file
C      CNOOUT   I(*)     Catalog slot number of file
C      CATFRQ   D(*)     Catalogue reference frequency for each stream
C      GNTYP    I        Gain curve type indicator:
C                        0 - don't do a gain curve
C                        1 - do a gain curve as fn. of antenna and band
C                        2 - do a gain curve as fn. of band only
C                        3 - do a gain curve, user specified
C      OPACIT   I        Determines opacity calculation type:
C                        0 -> no opacity correction
C                        1 -> opacity correction, with fixed zenith
C                             opacity (given in ZOPAC)
C                        2 -> opacity correction, with zenith opacity
C                             calculated from weighted average of
C                             weather data and day of year.
C                             New method based on K band, IF dependent
C                        4 -> opacity correction, with zenith opacity
C                             calculated from weighted average of
C                             weather data and day of year.
C                             Old method not based on K band
C      ZOPAC    R        zenith opacity
C      WTOPAC   R        weight for surface weather opacity part
C   Input/Output in common
C      TIMNCL   R        Time of next CL record
C   Output:
C      IRET     I        Return code, 0=>OK, else TABCAL or CALINI error
C   Passed through common:
C      CATOUT   I(*,*)   Catalog header block
C      CLBUFF   I(*)     Buffer for CL table
C      ICLRNO   I        Pointer for next record, if  > 0 on input then
C                        create/open the file.
C      CLKOLS   I(MAXCLC) The column pointer array in order.
C      CLNUMV   I(MAXCLC) Element count in each column.
C      TYBUFF   I(*)     Buffer for TY table
C      ITYRNO   I        Pointer for next record, if  > 0 on input then
C                        create/open the file.
C      TYKOLS   I(MAXTYC) The column pointer array in order.
C      TYNUMV   I(MAXTYC) Element count in each column.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DOTTV.INC'
C
      CHARACTER BBAND*1, CBAND(NOBAND)*1
      INTEGER   VER, NUMPOL, NIF, SUBA, LUNCL, LUNOT, NUMANT, WANT,
     *          IR1, IR2, IS, IIPT, IRTEMP(2), IPOL, IIF, IERR, DOFF
      REAL      GMMOD, IFR, TIME
      DOUBLE PRECISION RDELAY, WDELAY, FFAC1, FFAC2, TDELAY, PDYR1,
     *   PDYL1, PDYR2, PDYL2, PPYR1, PPYL1, PPYR2, PPYL2, FTEMP,
     *   CURTTF(2,MXSTRM)
C     DOUBLE PRECISION LSTG, GPH
      INTEGER   REFA(2,2), NTERM, BITS(32), IBAND
      LOGICAL   ISBROK, ISOPER, PRINTD(2)
      REAL      DOPOFF(2), ATMOS, DATMOS, MBDELY(2), CLOCK(2), ZA,
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,2), CIMAG(2,2), ELEV,
     *   CDELAY(2,2), CRATE(2,2), WEIGHT(2,2), CGAIN, COPAC, OOPAC, WT1,
     *   NH2O, WEATHR(5), COPACS(2), AZO(2)
      DOUBLE PRECISION    GEODLY(3), TIMED
      EQUIVALENCE (IRTEMP, FTEMP)
C
      SAVE OOPAC, PRINTD
C
      DATA VER,NUMANT /1,MXANT/
      DATA GMMOD /1.0/
      DATA REFA /4*0/
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /8*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, CDELAY, CRATE, WEIGHT /12*0.0, 4*1.0/
      DATA LUNCL, LUNOT /26,27/
      DATA CBAND / '4', 'P', 'L', 'S', 'C', 'X', 'U', 'K', 'A', 'Q' /
      DATA OOPAC / -1.0 /
      DATA PRINTD / 2*.FALSE. /
C-----------------------------------------------------------------------
C                                       Set time of the Current MC
C                                       integration.
      DOFF = MCDATE - IDNINT(REFMJD)
      TIME = (MCIATI / TWOPI) + DOFF
C                                       averaged time for CL rec
      TIMED = AVGIAT / MAX (1, MCLNF1)
      DO 10 IS = 1,5
         WEATHR(IS) = AVWEAT(IS) / MAX (1, MCLNF1)
 10      CONTINUE
      TIME8 = TIMED
C                                       Loop over streams
      DO 500 IS = 1,NSTREM
         NUMPOL = STNOPL(IS)
         IF (NUMPOL.GT.2) NUMPOL = 2
         SUBA = 1
         NIF = STNOIF(IS)
         JDSOUR = CURSOU(IS)
         CALL FLMBC (1.0D9*CATFRQ(IS), BBAND)
         DO 120 IIF = 1,NOBAND
            IF (BBAND.EQ.CBAND(IIF)) IBAND = IIF
 120        CONTINUE
C                                       Maximum number of antennas
         NUMANT = MXANT
C                                       Open CL table
         VER = 1
         NTERM = 1
         CALL CALINI ('WRIT', CLBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNCL, ICLRNO, CLKOLS, CLNUMV, NUMANT,
     *      NUMPOL, NIF, NTERM, GMMOD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Open OT table
         IF (DOOTT) THEN
            CALL OTTINI ('WRIT', OTBUFF, DISKO(IS), CNOOUT(IS), VER,
     *         CATOUT(1,IS), LUNOT, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Write record
         IR1 = STMCI1(IS)
         IR2 = STMCI2(IS)
C                                       Conversion peculiar phase to
C                                       phase delay.
         FFAC1 = 1.0D0 / (32768.0D0 * (MCSKYF(IR1)*1.0D9))
         FFAC2 = 1.0D0 / (32768.0D0 * (MCSKYF(IR2)*1.0D9))
C                                       LST at time of geometry
C        LSTG = MCLSTI + (MCIATG - MCIATI) * DPDAY / 360.0D0
C        GPH = (LSTG - MCRAAP) / TWOPI + 0.25D0
C                                       Get reference frequency from
C                                       header using EQUIVALENCE to
C                                       FTEMP.
         IIPT = ((KDCRV+JLOCF-1) * NWDPDP) + 1
         CALL COPY (NWDPDP, CATOUT(IIPT,IS), IRTEMP)
C                                       Doppler offset
         CURTTF(1,IS) = FTEMP + IFFREQ(CURFQI(IS),1,IS)
         CURTTF(2,IS) = FTEMP + IFFREQ(CURFQI(IS),2,IS)
         DOPOFF(1) = (MCSKYF(IR1) * 1.0D9) - CURTTF(1,IS) -
     *      SUFREQ(1,JDSOUR,IS)
         DOPOFF(2) = (MCSKYF(IR2) * 1.0D9) - CURTTF(2,IS) -
     *      SUFREQ(2,JDSOUR,IS)
C                                       Point IR1, IR2 to R-hand pol
         IF (IR1.GT.2) IR1 = IR1 - 2
         IF (IR2.GT.2) IR2 = IR2 - 2
         DO 200 IANT = 1,MXANT
C                                       Skip antennas not in subarray
            IF (.NOT.MCANTS(IANT)) GO TO 200
C                                       Current delays
            TDELAY = AVANTD(IANT) / MAX (1, MCLNF1) * 1.0D-9
            PDYR1 = MCANPD(IR1,IANT) * 1.0D-9
            PDYL1 = MCANPD(IR1+2,IANT) * 1.0D-9
            PDYR2 = MCANPD(IR2,IANT) * 1.0D-9
            PDYL2 = MCANPD(IR2+2,IANT) * 1.0D-9
            PPYR1 = MCANPP(IR1,IANT) * FFAC1
            PPYL1 = MCANPP(IR1+2,IANT) * FFAC1
            PPYR2 = MCANPP(IR2,IANT) * FFAC1
            PPYL2 = MCANPP(IR2+2,IANT) * FFAC2
            WDELAY = -AVGUVW(3,IANT) / MAX (1, MCLNF1) * 1.0D-9
            RDELAY = TDELAY - WDELAY
C                                       W delay = geometric delay
            GEODLY(1) = WDELAY
C                                       get the real part as a combo
C                                       of gain curve and opacity
C                                       Find band
            IF ((OPACIT.EQ.0) .AND. (GNTYP.EQ.0)) THEN
               CGAIN = 1.0
               COPAC = 1.0
            ELSE
               ELEV = ASIN (AVSCOS(1) / MAX (1, MCLNF1))
               IF (OPACIT.EQ.0) THEN
                  COPAC = 1.0
               ELSE
                  IF ((OPACIT.EQ.2) .OR. (OPACIT.EQ.4)) THEN
                     IF (WTOPAC.NE.0.0) THEN
                        WT1 = WTOPAC
                        IF (MCFREV .LT. 4) THEN
C                                       no weather data for rev < 4.
                           IF (.NOT.PRINTD(1)) THEN
                              MSGTXT = ' Archive tape revision < 4, ' //
     *                          'no weather data - setting weight to 0.'
                              CALL MSGWRT (3)
                              PRINTD(1) = .TRUE.
                              END IF
                           WT1 = 0.0
                        ELSE
C                                       Is there valid weather data?
                           CALL ZGTBIT (32, MCACB, BITS)
                           ISBROK = BITS(31) .EQ. 1
                           ISOPER = BITS(27) .EQ. 1
                           IF (ISBROK) THEN
                              WT1 = 0.0
                              IF (.NOT.PRINTD(2)) THEN
                                 MSGTXT = ' Weather data suspect ' //
     *                                    '- setting weight to 0.'
                                 CALL MSGWRT (3)
                                 PRINTD(2) = .TRUE.
                                 END IF
                              END IF
                           END IF
                        END IF
                     IF (OPACIT.EQ.4) THEN
                        CALL OPACTY (IBAND, WEATHR(3), WEATHR(5),
     *                     WEATHR(4), WT1, MCDATE, TIMED, NH2O, ZOPAC)
                     ELSE
                        CALL OPACTY (8, WEATHR(3), WEATHR(5), WEATHR(4),
     *                     WT1, MCDATE, TIMED, NH2O, ZOPAC)
                        CALL KBOPAC (NIF, CURTTF(1,IS), ZOPAC, ELEV,
     *                     AZO, COPACS)
                        END IF
C                                       only print if delta tau0 > 0.1%
                     IF (ABS(ZOPAC-OOPAC).GE.0.001) THEN
                        IF (OPACIT.EQ.2) THEN
                           WRITE (MSGTXT,1120) AZO(1), ZOPAC
                        ELSE
                           WRITE (MSGTXT,1121) ZOPAC
                           END IF
                        CALL MSGWRT (3)
                        OOPAC = ZOPAC
                        END IF
                     END IF
C                                       I use a simple secant, not
C                                       the more complicated one like in
C                                       CLCOR.  i'm not convinced that
C                                       one's right anyway. bjb.
C                                       AVSCOS(2) is avg 1/MCSCOS(1)
                  COPAC = ZOPAC * (AVSCOS(2) / MAX (1, MCLNF1))
C                                       If you want it like CLCOR, then:
C                 COPAC = ZOPAC/(MCSCOS(1)+(0.00143/(TAN(ELEV)+0.0045)))
                  COPAC = SQRT (EXP (COPAC))
                  END IF
               ZA = 90.0 * (1 - 2 * ELEV / PI)
               IF (GNTYP.EQ.0) THEN
                  CGAIN = 1.0
               ELSE
                  IF (GNTYP.EQ.2) THEN
                     WANT = MXANT+1
                  ELSE
                     WANT = IANT
                     END IF
                  IF (NEEDGN) THEN
                     CALL FLMGN (IERR)
                     NEEDGN = .FALSE.
                     END IF
                  CGAIN = AGAINS(IBAND,1,WANT) + ZA*AGAINS(IBAND,2,WANT)
     *               + ZA*ZA * AGAINS(IBAND,3,WANT)
     *               + ZA*ZA*ZA * AGAINS(IBAND,4,WANT)
                  IF (CGAIN.LE.0.0) THEN
                     WANT = MXANT + 1
                     CGAIN = AGAINS(IBAND,1,WANT) +
     *                  ZA*AGAINS(IBAND,2,WANT) +
     *                  ZA*ZA * AGAINS(IBAND,3,WANT) +
     *                  ZA*ZA*ZA * AGAINS(IBAND,4,WANT)
                     END IF
                  IF (CGAIN.LE.0.0) CGAIN = 1.0
C                                       The correction for the gain is
C                                       1 over the gain curve value.
                  CGAIN = 1 / CGAIN
                  END IF
               END IF
            DO 100 IPOL = 1,NUMPOL
               DO 100 IIF = 1,NIF
                  IF (OPACIT.NE.2) THEN
                     CREAL(IPOL,IIF) = CGAIN * COPAC
                  ELSE
                     CREAL(IPOL,IIF) = CGAIN * COPACS(IIF)
                     END IF
 100           CONTINUE
C                                       CL table
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *         NIF, TIMED, TIMCLI, JDSOUR, IANT, SUBA, CURFQI(IS), IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, CDELAY, CRATE, WEIGHT, REFA,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       OT table
            IF (DOOTT) THEN
C                                       TIMCLI already in /INPARM/
               DT = TIMCLI
               CALL ZGTBIT (32, AVANCB(IANT), BITS)
               OTT = BITS(25).EQ.1
               CALL TABOTT ('WRIT', OTBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 200        CONTINUE
C                                       Close all open tables
         CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (DOOTT) THEN
            CALL TABIO ('CLOS', 0, IOTRNO, OTBUFF, OTBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 500     CONTINUE
C                                       Set time for next
      TIMNCL = TIME + TIMCLI - 5.787E-7
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('Zenith opacity of IF 1 set to ',F5.3,' (',F5.3,
     *   ' at K-band)')
 1121 FORMAT ('Zenith opacity set to ',F5.3)
      END
      SUBROUTINE FLMTY (IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill TY table
C   Input from common:
C      REFMJD   D        Reference MJD (mod. Julian date).
C      MCDATE   I        Current MJD.
C      MCIATI   D        IAT at middle of integration (radians)
C      MCINTG   I        Integration time in waveguide cycles (52 msec).
C      MCANTS   L(28)    If true, antenna in subarray
C      TIMTYI   R        Time interval (days)
C      CURSOU   I(*)     Source number per stream
C      CURFQI   I(*)     FQ id per stream.
C      DISKO    I(*)     Disk number of file
C      CNOOUT   I(*)     Catalog slot number of file
C   Input/Output in common
C      TIMNTY   R        Time of next TY record
C   Output:
C      IRET     I        Return code, 0=>OK, else TABCAL or CALINI error
C   Passed through common:
C      CATOUT   I(*,*)   Catalog header block
C      CLBUFF   I(*)     Buffer for CL table
C      ICLRNO   I        Pointer for next record, if  > 0 on input then
C                        create/open the file.
C      CLKOLS   I(MAXCLC) The column pointer array in order.
C      CLNUMV   I(MAXCLC) Element count in each column.
C      TYBUFF   I(*)     Buffer for TY table
C      ITYRNO   I        Pointer for next record, if  > 0 on input then
C                        create/open the file.
C      TYKOLS   I(MAXTYC) The column pointer array in order.
C      TYNUMV   I(MAXTYC) Element count in each column.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DOTTV.INC'
C
      CHARACTER BBAND*1
      INTEGER   VER, NUMPOL, NIF, SUBA, LUNTY, NUMANT, IR1, IR2, IS,
     *   DOFF
      REAL      TIME, TSYS(2,2), TANT(2,2), TIME4, XSCAL
      LOGICAL   DOSUN
      DATA LUNTY /28/
C-----------------------------------------------------------------------
      CALL RFILL (4, FBLANK, TSYS)
      CALL RFILL (4, FBLANK, TANT)
C                                       Set time of the Current MC
C                                       integration.
      DOFF = MCDATE - IDNINT(REFMJD)
      TIME = (MCIATI / TWOPI) + DOFF
C                                       Find band
      CALL FLMBC (FREQ, BBAND)
      XSCAL = 1.0 / MAX (1, MCLNF1)
C                                       Loop over streams
      DO 500 IS = 1,NSTREM
         NUMPOL = STNOPL(IS)
         IF (NUMPOL.GT.2) NUMPOL = 2
         SUBA = 1
         NIF = STNOIF(IS)
         JDSOUR = CURSOU(IS)
C                                       Maximum number of antennas
         NUMANT = MXANT
         VER = 1
C                                       Open TY table
         CALL TYINI ('WRIT', TYBUFF, DISKO(IS), CNOOUT(IS), VER,
     *      CATOUT(1,IS), LUNTY, ITYRNO, TYKOLS, TYNUMV, NUMPOL, NIF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write record
         IR1 = STMCI1(IS)
         IR2 = STMCI2(IS)
C                                       LST at time of geometry
C        LSTG = MCLSTI + (MCIATG - MCIATI) * DPDAY / 360.0D0
C        GPH = (LSTG - MCRAAP) / TWOPI + 0.25D0
C                                       Point IR1, IR2 to R-hand pol
         IF (IR1.GT.2) IR1 = IR1 - 2
         IF (IR2.GT.2) IR2 = IR2 - 2
         DO 200 IANT = 1,MXANT
C                                       Skip antennas not in subarray
            IF (.NOT.MCANTS(IANT)) GO TO 200
C                                       Solar "System temperatures"
            IF (SOLAR) THEN
               DOSUN = MCALCD(1:1).EQ.' '
C                                       Sun
               IF (DOSUN) THEN
                  IF (IFFLAG(IR1,IANT)) THEN
                     TSYS(1,1) = FBLANK
                  ELSE
                     TSYS(1,1) = AVTSYS(1,1,IANT) * XSCAL
                     END IF
                  IF (IFFLAG(IR1+2,IANT)) THEN
                     TSYS(2,1) = FBLANK
                  ELSE
                     TSYS(2,1) = AVTSYS(2,1,IANT) * XSCAL
                     END IF
                  IF (IFFLAG(IR2,IANT)) THEN
                     TSYS(1,2) = FBLANK
                  ELSE
                     TSYS(1,2) = AVTSYS(1,2,IANT) * XSCAL
                     END IF
                  IF (IFFLAG(IR2+2,IANT)) THEN
                     TSYS(2,2) = FBLANK
                  ELSE
                     TSYS(2,2) = AVTSYS(2,2,IANT) * XSCAL
                     END IF
C                                       Calibrator
               ELSE
                  TSYS(1,1) = 1.0
                  TSYS(2,1) = 1.0
                  TSYS(1,2) = 1.0
                  TSYS(2,2) = 1.0
                  END IF
            ELSE
               TSYS(1,1) = AVTSYS(1,1,IANT) * XSCAL
               TSYS(2,1) = AVTSYS(2,1,IANT) * XSCAL
               TSYS(1,2) = AVTSYS(1,2,IANT) * XSCAL
               TSYS(2,2) = AVTSYS(2,2,IANT) * XSCAL
               END IF
C                                       revisions 25 and up: fill TANT
C                                       with FE or BE TSYS. Depends on
C                                       band, override using CPARM(2)
            IF (MCFREV.GE.25) THEN
               TANT(1,1) = AVTANT(1,1,IANT) * XSCAL
               TANT(2,1) = AVTANT(2,1,IANT) * XSCAL
               TANT(1,2) = AVTANT(1,2,IANT) * XSCAL
               TANT(2,2) = AVTANT(2,2,IANT) * XSCAL
               END IF
C                                       TY table
            TIME4 = AVGIAT * XSCAL
            CALL TABTY ('WRIT', TYBUFF, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *         NIF, TIME4, SUMINT/86400., JDSOUR, IANT, SUBA,
     *         CURFQI(IS), TSYS, TANT, IRET)
            IF (IRET.NE.0) GO TO 999
 200        CONTINUE
C                                       Close all open tables
         CALL TABIO ('CLOS', 0, ITYRNO, TYBUFF, TYBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
 500     CONTINUE
C                                       Set time for next
      TIMNTY = TIME + TIMTYI - 5.787E-7
C
 999  RETURN
      END
      SUBROUTINE FLMANT (IS)
C-----------------------------------------------------------------------
C   Creates and fills the antenna file from info in common.
C   Input:
C      IS       I        Output stream number
C   Output in common:
C      STRXYZ   D(*)     current antenna positions in this stream
C-----------------------------------------------------------------------
      INTEGER   IS
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      INTEGER   IERR, VER, LUNAN, I, NAT, ITBUFF(512), I4
      DOUBLE PRECISION   JD, GASTM, JDNOMC
      EQUIVALENCE (TBUFF, ITBUFF)
      DATA LUNAN /27/
C-----------------------------------------------------------------------
C                                       Setup for AN table initization
C***??? Values on tape?
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
      UT1UTC = 0.0
      DATUTC = 0.0
      ANAME = 'VLA '
C                                       old values
C                                       ARRAYC(1) = -1601162.D0
C                                       ARRAYC(2) =  -5042003.D0
C                                       ARRAYC(3) =  3554915.D0
C                                       new values 2-Jul-2001
      ARRAYC(1) = -1601185.365D0
      ARRAYC(2) =  -5041977.547D0
      ARRAYC(3) =  3554875.87D0
C                                       Get GST0 and Earth rotation rate
      RDATE = '20070630'
      CALL JULDAY (RDATE, JDNOMC)
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD)
      CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
      TOLERB = 0.02D0
      IF (JD.GT.JDNOMC) TOLERB = 0.001D0
C                                       Get reference frequency
      SAFREQ = CATD(KDCRV+JLOCF)
C
      VER = 1
      ANFQID = -1
      NAT = MCNANT
      TIMSYS = 'IAT'
      NOPCAL = 2
      NUMORB = 0
      ANTNIF = STNOIF(IS)
      XYZHAN = 'RIGHT'
      TFRAME = 'VLA'
C                                       Create/init file
      CALL ANTINI ('WRIT', TBUFF, DISKO(IS), CNOOUT(IS), VER, CATBLK,
     *   LUNAN, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
C                                       See if entries exist
      I4 = ITBUFF(5)
      IF (I4.GT.0) GO TO 30
      IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
      ANNAME = 'VLA'
      STAXOF = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA = 0
      MNTSTA = 0
      POLAA = 0.0
      POLAB = 0.0
C                                       RCP
      POLCA(1) = 0.0
      POLCA(2) = 0.0
      POLCA(3) = 0.0
      POLCA(4) = 0.0
C                                       LCP
      POLCB(1) = 0.0
      POLCB(2) = 0.0
      POLCB(3) = 0.0
      POLCB(4) = 0.0
      POLTYA = 'R '
      POLTYB = 'L '
      IF (NAT.LE.0) GO TO 30
C                                       AN records
      DIAMAN = 25.
      CALL RFILL (ANTNIF, 0.0, FWHMAN)
      DO 20 I = 1,MXANT
         STRXYZ(1,I) = MCAXYZ(1,I)
         STRXYZ(2,I) = MCAXYZ(2,I)
         STRXYZ(3,I) = MCAXYZ(3,I)
         STAXYZ(1) = MCAXYZ(1,I) * 1.0D-9 * VELITE
         STAXYZ(2) = MCAXYZ(2,I) * 1.0D-9 * VELITE
         STAXYZ(3) = MCAXYZ(3,I) * 1.0D-9 * VELITE
         STAXOF = MCANBA(I) * 1.0D-9 * VELITE
         NOSTA = I
         ANNAME = MCANTN(I)
         CALL TABAN ('WRIT', TBUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
 20      CONTINUE
C                                       Fill in header and close
 30   CALL TABIO ('CLOS', 0, IANRNO, ITBUFF, ITBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Error ',I3,' occurred writing antenna (AN) table')
      END
      SUBROUTINE VLANAM (ANTXYZ, NOSTA, ANNAME, MCB, MAXSTN)
C-----------------------------------------------------------------------
C   Routine to determine the name of the VLA pad for an antenna.
C   Looks up the "X" coordinate to determine the physical pad number.
C   Inputs:
C      ANTXYZ   D(3)   Antenna x, y, and z (nsec)
C      NOSTA    I      Antenna number
C      MCB      I      Control bits
C   In/out:
C      MAXSTN   I(3)   max pad number on W. E. N arms
C   Output:
C      ANNAME   C*8    Pad name
C-----------------------------------------------------------------------
      INTEGER   NOSTA, MCB, MAXSTN(3)
      CHARACTER ANNAME*8
      DOUBLE PRECISION ANTXYZ(3)
C
      INTEGER   LOOP, NPAD, IPAD, BITS(32)
      CHARACTER PADNW(24)*3, PADNE(24)*3, PADNN(24)*3, PADOUT*3, VLA*5,
     *   EVLA*5
      REAL      LXW(24), LXE(24), LXN(24)
C
      DATA PADNW /'W01','W02','W03','W04','W05','W06','W07','W08','W09',
     *   'W10','W12','W14','W16','W18','W20','W24','W28','W32','W36',
     *   'W40','W48','W56','W64','W72'/
      DATA PADNE /'E01','E02','E03','E04','E05','E06','E07','E08','E09',
     *   'E10','E12','E14','E16','E18','E20','E24','E28','E32','E36',
     *   'E40','E48','E56','E64','E72'/
      DATA PADNN /'N01','N02','N03','N04','N05','N06','N07','N08','N09',
     *   'N10','N12','N14','N16','N18','N20','N24','N28','N32','N36',
     *   'N40','N48','N56','N64','N72'/
      DATA LXW /77.,49.,96.,156.,229.,312.,406.,510.,623.,747.,1021.,
     *   1328.,1667.,2041.,2446.,3354.,4391.,5470.,6671.,7988.,10926.,
     *   14206.,17843.,21803./
      DATA LXE /151.,38.,73.,119.,173.,236.,305.,382.,466.,558.,765.,
     *   1000.,1257.,1548.,1868.,2552.,3331.,4180.,5119.,6127.,8325.,
     *   10814.,13620.,16204./
      DATA LXN /2.5,-100.,-175.,-250.,-362.,-495.,-646.,-813.,-995.,
     *   -1193.,-1632.,-2126.,-2673.,-3271.,-3917.,-5539.,-6976.,
     *   -8770.,-10733.,-12858.,-17583.,-22919.,-28827.,-35283./
      DATA PADOUT, VLA, EVLA /'OUT','VLA:_','EVLA:'/
      DATA NPAD /24/
C-----------------------------------------------------------------------
      ANNAME = VLA // PADOUT
      CALL ZGTBIT (32, MCB, BITS)
      IF (BITS(23).EQ.1) ANNAME(:5) = EVLA
C                                       West arm
      IPAD = 0
      DO 100 LOOP = 1,NPAD
         IF (ABS (ANTXYZ(1)-LXW(LOOP)).LE.2.0) IPAD = LOOP
 100     CONTINUE
      IF (IPAD.GT.0) THEN
         ANNAME(6:8) = PADNW(IPAD)
         MAXSTN(1) = MAX (MAXSTN(1), IPAD)
         GO TO 999
         END IF
C                                       Southeast arm
      IPAD = 0
      DO 200 LOOP = 1,NPAD
         IF (ABS (ANTXYZ(1)-LXE(LOOP)).LE.2.0) IPAD = LOOP
 200     CONTINUE
      IF (IPAD.GT.0) THEN
         ANNAME(6:8) = PADNE(IPAD)
         MAXSTN(2) = MAX (MAXSTN(2), IPAD)
         GO TO 999
         END IF
C                                       North arm
      IPAD = 0
      DO 300 LOOP = 1,NPAD
         IF (ABS (ANTXYZ(1)-LXN(LOOP)).LE.2.0) IPAD = LOOP
 300     CONTINUE
      IF (IPAD.GT.0) THEN
         ANNAME(6:8) = PADNN(IPAD)
         MAXSTN(3) = MAX (MAXSTN(3), IPAD)
         GO TO 999
         END IF
C                                       Pie Town
      IF (NOSTA.EQ.29) THEN
         IF (ABS(ANTXYZ(1)+46200.D0).LE.2.0) THEN
            ANNAME = 'VLA:_VPT'
         ELSE
            ANNAME = 'VPT:_OUT'
            END IF
         GO TO 999
         END IF
C                                       master pad
      IF (ABS(ANTXYZ(1)-1148.2571).LE.4.0) THEN
         ANNAME(6:8) = 'MPD'
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLMHED (IS, IRET)
C-----------------------------------------------------------------------
C   Routine in which the catalog header is constructed.
C   Input:
C      IS       I        Stream number
C   Input from AIPS common:
C      DOUVCM   L        True if data to be compressed format
C   Input from MODCOMP common:
C      STLEN    I(*)     No. words in vis. data in each stream
C      STFREQ   D(*)     Reference frequency for each stream
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)     VLA reference IF for 2nd IF of stream
C      STNOIF   I(*)     No. IFs in each stream.
C      STNOCH   I(*)     No. Channels in each stream
C      STNOPL   I(*)     No. polarizations in each stream
C      STOFIF   I(*)     0-rel index of IF axis in each stream
C      STOFFF   I(*)     0-rel index of Frequency axis in each stream
C      STOFFS   I(*)     0-rel index of Stokes parameter in each stream
C      STBCH    I(*)     1st channel to select in each stream
C      STECH    I(*)     Highest channel to select in each stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      MCSSLO   D(4)     Signed sum of LOs IFs A-D (GHz)
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCMODE   I        Correlator mode code:
C                         1=cont,
C                         2=1A, 3=1B, 4=1C, 5=1D,
C                         6=2AB, 7=2AC, 8=2AD, 9=2BC, 10=2BD, 11=2CD,
C                        12=4, 13=PA, 14=PB,
C                        15=1A/C, 16=1B/C, 17=1C/C, 18=1D/C,
C                        19=2AC/C, 20=2BD/C,
C                        21=2A, 22=4A
C   Input/Output:
C      CATBLK   I(256)   Modified output catalog header.
C   Output:
C      IRET     I        Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IS, IRET
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'MC2.INC'
C
      CHARACTER MULTI*8, RTYPES(9)*8, TYPES(7)*8, UNITS*8, TELE*8,
     *   INSTR*8
      INTEGER   I, NAXIS, NRAN, NDIM(7), INDXX, IR1, ITEMP, NFR, FR1,
     *   FR2
      REAL      CRPIX(7), CRINC(7)
      DOUBLE PRECISION CRVAL(7)
C
      DATA MULTI /'MULTI '/
C                                         No. random parameters.
      DATA NRAN /7/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN',
     *   'BASELINE','TIME1   ','SOURCE  ','FREQSEL ',
     *   'WEIGHT  ','SCALE   '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /6/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                         Axis dimensions
      DATA NDIM /3,4,1,1,1,1,1/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 0.0D0, 1.0D0, 3*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 5*1.0/
C                                       Units
      DATA UNITS /'UNCALIB '/
C                                       Telescope/instrument
      DATA TELE, INSTR /'VLA     ','VLA     '/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL CATINI (CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
      IF (DOUVCM) NRAN = 9
      KLOCWT = 7
      DO 10 I = 1,NRAN
         INDXX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDXX))
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDXX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDXX))
 30      CONTINUE
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       Compressed?
      IF (DOUVCM) CATBLK(KINAX) = 1
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = MCEPOC
C                                       Sort order ('TB'=>time baseline)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       Number of vis.
      IF (DOMANY) THEN
         CATBLK(KIGCN) = 2000
      ELSE
         CATBLK(KIGCN) = 10000
         END IF
C                                       Polarization axis
C     JLOCS = STOFFS(IS)
      JLOCS = 1
      CATBLK(KINAX+JLOCS) = STNOPL(IS)
      CATR(KRCIC+JLOCS) = CRINC(2)
      CATR(KRCRT+JLOCS) = 0.0
      CATR(KRCRP+JLOCS) = CRPIX(2)
      CATD(KDCRV+JLOCS) = CRVAL(2)
C                                       Kludge for bad split band freq.
      IF ((STMCI1(IS).GT.2) .AND. (MCMODE.GT.1))
     *   CATD(KDCRV+JLOCS) = -2.0
      INDXX = KHCTP + JLOCS * 2
      CALL CHR2H (8, TYPES(2), 1, CATH(INDXX))
C                                       Frequency axis
C     JLOCF = STOFFF(IS)
      JLOCF = 2
      CATBLK(KINAX+JLOCF) = STNOCH(IS)
      IR1 = STMCI1(IS)
      CATR(KRCRT+JLOCF) = 0.0
C                                       Spectral line
      IF (STTYPE(IS).EQ.2) THEN
         CATR(KRCIC+JLOCF) = MCHSEP(IR1)
         CATD(KDCRV+JLOCF) = STFREQ(IS) * 1.0D9
C
C The following ITEMP definition is incorrect for BW codes 8 and 9. It
C assumes that MCSSLO(IR1) coincides with the frequency of the first
C channel. For BW codes 8 and 9, there is a full total bandwidth offset
C between MCSLLO(IR1) and the frequency of channel 1. Changed by Gustaaf
C van Moorsel, 13-Feb-95, to define first channel to be 0.5*MCBANW away
C from MCSKYF, independent of MCSSLO.
C
C        _____________________
C       |          |          |                          (BW code 1 - 7)
C    MCSSLO      MCSKYF      last
C     first                 channel
C    channel
C        ___________________________________________
C       |                     |          |          |    (BW code 8 - 9)
C    MCSSLO                 first      MCSKYF      last
C                          channel                channel
C
C         ITEMP = ((ABS (MCSSLO(IR1) - MCSKYF(IR1)) * 1.0D9) /
C     *      ABS (MCHSEP(IR1))) + 0.5
         ITEMP = MCBANW(IR1)/2.0 / ABS (MCHSEP(IR1)) + 0.5
         CATR(KRCRP+JLOCF) = ITEMP - (MCCHOF(IR1) + STBCH(IS) - 1)
C                                       Continuum
      ELSE
         CATD(KDCRV+JLOCF) = STFREQ(IS) * 1.0D9
         CATR(KRCIC+JLOCF) = MCBANW(IR1)
         CATR(KRCRP+JLOCF) = 1.0
C                                       Channel 0
         IF (STTYPE(IS).EQ.1) THEN
            IR1 = STMCI1(IS+1)
            NFR = STNOCH(IS+1)
            FR1 = (NFR+1) / 8 + 1 + STZMOD(IS+1)
            FR2 = NFR - ((NFR+1)/8)
            CATR(KRCIC+JLOCF) = (FR2 - FR1 + 1) * MCHSEP(IR1)
            END IF
         END IF
C
      INDXX = KHCTP + JLOCF * 2
      CALL CHR2H (8, TYPES(3), 1, CATH(INDXX))
C                                       Rest Frequency
      CATD(KDRST) = MCREST(IS) * 1.0D6
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = CATR(KRCRP+JLOCF)
      CATBLK(KIALT) = 0
C                                       IF axis
C     JLOCIF = STOFIF(IS)
      JLOCIF = 3
      CATBLK(KINAX+JLOCIF) = STNOIF(IS)
      CATR(KRCIC+JLOCIF) = CRINC(4)
      CATR(KRCRT+JLOCIF) = 0.0
      CATR(KRCRP+JLOCIF) = CRPIX(4)
      CATD(KDCRV+JLOCIF) = CRVAL(4)
      INDXX = KHCTP + JLOCIF * 2
      CALL CHR2H (8, TYPES(4), 1, CATH(INDXX))
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
      IF (REFDAY.NE.' ') CALL CHR2H (8, REFDAY, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, MULTI, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Project name.
      CALL CHR2H (8, MCPGID, 1, CATH(KHOBS))
C                                       Finished.
      IRET = 0
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FLMDAT (NUMVIS, IPTRO, BLNDX, IRET)
C-----------------------------------------------------------------------
C   Reads MODCOMP data and returns one visibility at a time for
C   up to 4 output streams.
C   Note: on the first call (NUMVIS=1) the necessary information to
C   define the output file should be filled into the appropriate commons
C   and a return code of -1 returned.  This will cause the output file
C   to be created and I/O initialized but no data will be written as a
C   result of this call.  When all data is exhausted an IRET(1) = -3
C   on return indicates this.  No data will be written to the output
C   file unless IRET(n)=0.
C   Inputs:
C      NUMVIS   I        Visibility number, -1=> final call, no data
C                        passed but allows any operations to be
C                        completed.
C                        If NUMVIS = 1 then the information about the
C                        file should be determined and a IRET=-1 but no
C                        data returned.
C      IPTRO    I(*)     Pointer into BUFFER to store the next
C                        visibility record.
C   Inputs from common:
C      CATBLK   I(256)   Catalog header record.  See Going AIPS.
C   Input/output:
C      BLNDX    I        Baseline index
C   Output in common:
C      NSTREM   I        Number of output streams
C      SELECT   R(11)    Data selection array; this array is maintained
C                        by FLMDAT.
C      DPDAY    D        Earth rotation rate in deg/day
C      TMCBCH   I        User specified BCHAN
C      TMCECH   I        User specified ECHAN
C      BUFFER   R(*,*)   Buffer holding the visibility records
C   Output:
C      IRET     I(*)     Return code 1 per stream  -3 => End of data
C                               -4 => find new output files
C                               -2 => ignore visibility
C                               -1 => Initialize output, should only be
C                                     returned on NUMVIS=1
C                                0 => valid data
C                               >0 => error, terminate
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IPTRO(*), BLNDX, IRET(*)
C
      INTEGER   IA1, IA2, IFIL, IERR, IROUND, LOOP, BLPTR, ITRMJD,
     *          NSHAD, NBADPT, NOFF, NOTHER
      LOGICAL  T, F, GOOD, GOODS, DONEW, TIMSEL
      DOUBLE PRECISION XRDAY, XDAT, GSTIA0, GASTM, TRMJD
      REAL      TRUTIM
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MC2.INC'
      LOGICAL   SHANT(MXANT), BADPTG(MXANT), OFFSOU(MXANT),
     *          OTHERF(MXANT)
      SAVE NSHAD, NBADPT, NOFF, NOTHER, SHANT, BADPTG, OFFSOU, OTHERF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FILL (MXSTRM, 0, IRET)
C                                       Check NUMVIS
      IF (NUMVIS.GE.2) GO TO 100
      IF (NUMVIS.LT.0) GO TO 900
C                                       ====== Init first time
      STIAT = -1.0D10
C                                       Channel selection
      TMCBCH = IROUND (XBCH)
      TMCECH = IROUND (XECH)
C                                       Initialize counters
      NSHAD = 0
      NBADPT = 0
      NOFF = 0
      NOTHER = 0
C                                       Set selection criteria.
      NEWFIL = F
      BAIL = F
      IFSLIM = IROUND (CPARM(3))
      IF ((IFSLIM.GE.0) .AND. (IFSLIM.LE.3)) IFSLIM = 3
      IF ((IFSLIM.LT.0) .AND. (IFSLIM.GE.-3)) IFSLIM = -3
      CPARM(3) = IFSLIM
C                                       Source qualifier
      SELECT(1) = QUAL
C                                       Observing band
      SELBAN = BAND(1:1)
      SELBA2 = BAND(2:2)
      IF (SELBA2.EQ.' ') SELBA2 = SELBAN
C                                       Observing program name
      SELPGM = VLAOBS
C                                       Integration time
CCCC                                       Subtract 0.2 seconds.
CCCC      SELECT(5) = ((CPARM(1)-0.2) / 86400.0) * TWOPI
C                                       Do not subtract 0.2 seconds,
C                                       see how it goes.
      SELECT(5) = (CPARM(1) / 86400.0) * TWOPI
C                                       Observing mode
      SELMOD = VLAMOD
C                                       Subarray number
      SELECT(7) = CPARM(6)
C                                       Selection by no. channels
      SELECT(8) = CPARM(5)
C                                       Start, stop times
      TSTART = XTR(1) + (XTR(2) / 24.0) + (XTR(3) / 1440.) +
     *   (XTR(4) / 86400.)
      IF (TSTART.EQ.0.0) THEN
         XTR(1) = -99.0
         TSTART = -99.0
         END IF
      TEND = XTR(5) + (XTR(6) / 24.0) + (XTR(7) / 1440.) +
     *   (XTR(8) / 86400.)
      IF (TEND.EQ.0.0) THEN
         XTR(5) = 999.0
         TEND = 999.0
         END IF
      IF (TEND.LE.TSTART) THEN
         XTR(1) = -99.
         XTR(5) = 999.
         TSTART = -99.0
         TEND = 999.0
         MSGTXT = 'WARNING: INCORRECT NON-ZERO TIMERANG OVERRIDDEN'
         CALL MSGWRT (7)
         END IF
      TIMSEL = (TSTART.NE.-99.0) .OR. (TEND.NE.999.0)
      SELECT(9) = TSTART * TWOPI
      SELECT(10) = TEND * TWOPI
C                                       Cal averaging time
CCCC      SELECT(11) = ((CPARM(10)-0.2) / 86400.0) * TWOPI
      SELECT(11) = (CPARM(10) / 86400.0) * TWOPI
C                                       Frequency selection
      FRESEL(1,1) = DPARM(1) * 1.0D-9
      FRESEL(2,1) = DPARM(9) * 1.0D-9
      FRESEL(1,2) = DPARM(3) * 1.0D-9
      FRESEL(2,2) = DPARM(10) * 1.0D-9
      FRESEL(1,3) = DPARM(5) * 1.0D-9
      FRESEL(2,3) = DPARM(9) * 1.0D-9
      FRESEL(1,4) = DPARM(7) * 1.0D-9
      FRESEL(2,4) = DPARM(10) * 1.0D-9
      FRESEL(1,1) = FRESEL(1,1) + DPARM(2) * 1.0D-9
      FRESEL(1,2) = FRESEL(1,2) + DPARM(4) * 1.0D-9
      FRESEL(1,3) = FRESEL(1,3) + DPARM(6) * 1.0D-9
      FRESEL(1,4) = FRESEL(1,4) + DPARM(8) * 1.0D-9
C                                       Initialize /MODCOM/
C                                       Number of FORMAT 1 records
C                                       stacked
      MCLNF1 = 0
      MCINIT = T
      GOTHED = F
      ISEOF = F
      ISEOT = F
C                                       Number of files read.
      NOFILE = 0
      DAYOFF = 0
C                                       Initialize FDVEC
      CALL FILL (50, 0, FDVEC)
C                                       Init Disk
      IF (DODISK) THEN
         FDVEC(1) = 29
         FDVEC(2) = 2048 * 13
         FDVEC(3) = TBSIZE * NBITWD / 8
         CALL CHR2H (48, INFILE, 1, FDVEC(7))
         FDVEC(33) = MAX (0, NFILES) + 1
C                                       Open tape
         CALL FLDKIO ('OPRD', FDVEC, TAPBUF, TAPIND, IRET(1))
         IF (IRET(1).GT.1) THEN
            WRITE (MSGTXT,1000) IRET(1)
            GO TO 990
            END IF
C                                       Initialize, open, position
C                                       tape.
      ELSE
         FDVEC(1) = 129 - ITAPE
         FDVEC(2) = 26624
         FDVEC(3) = TBSIZE * NBITWD / 8
         FDVEC(5) = ITAPE
C                                       Open tape
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TAPIND, IRET(1))
         IF (IRET(1).GT.1) THEN
            WRITE (MSGTXT,1010) IRET(1)
            GO TO 990
            END IF
C                                       Skip to correct file: on-line
         IF (ONLINE) THEN
            CALL ZOLAVF (FDVEC, NFILES, IRET(1))
            IF (IRET(1).LE.1) THEN
               IRET(1) = 0
            ELSE
               GO TO 999
               END IF
C                                       Skip to correct file: off line
         ELSE
            IF (NFILES.GT.0) CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40),
     *         NFILES, IRET(1))
            IFIL = 1 - NFILES
            IF ((NFILES.LE.0) .AND. (CPARM(10).LE.0.01))
     *         CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IRET(1))
            IF (IRET(1).NE.0) THEN
               WRITE (MSGTXT,1020) IRET(1)
               GO TO 990
               END IF
            END IF
         END IF
C                                       Find first data record.
      DAYOFF = XTR(1)
      IF (DAYOFF.EQ.-99) DAYOFF = 0
      CALL MCREC ('INIT', IERR)
      OSTREM = NSTREM
      CALL COPY (MXSTRE, STNOPL, OSTNPL)
      CALL COPY (MXSTRE, STNOIF, OSTNIF)
      IRET(1) = IERR
      IF (IERR.EQ.4) IRET(1) = -3
C                                       Possible message if no data
C                                       found.
      WRITE (MSGTXT,1030)
      IF (IERR.NE.0) GO TO 990
      BLNDX = 0
C                                       Reference MJAD
C                                       Get MJAD and Obs. date.
C                                       NOTE: REFMJD may be reset in
C                                       FLMCRE.
      REFMJD = MCDATE
      IF (REFDAY.NE.' ') THEN
         CALL JULDAY (REFDAY, REFMJD)
         REFMJD = REFMJD - 2400000.5D0
         END IF
C                                       Reference day offset.
      DAYOFF = IDNINT(REFMJD) - MCDATE
C                                       Check time found
      IF (REFDAY.NE.' ') THEN
C                                       Find true time on tape
         CALL JULDAY (REFDAY, TRMJD)
         TRMJD = TRMJD - 2400000.5D0
         ITRMJD = IDNINT(TRMJD)
         TRUTIM = (AVGIAT - XTR(1) + MCDATE - ITRMJD) * TWOPI
C                                       Find correct time
         IF (TRUTIM.LT.SELECT(9)) THEN
            CALL MCREC ('READ', IERR)
            IRET(1) = IERR
            IF (IERR.EQ.4) IRET(1) = -3
            IF (ISEOT) IRET(1) = -3
            END IF
         END IF
      XRDAY = MCDATE
      XDAT = XRDAY + 2400000.5D0
C                                       earth rotation rate
      CALL GSTROT (XDAT, GSTIA0, GASTM, DPDAY)
C                                       Convert Julian date to calender
      IF (REFDAY.EQ.' ') THEN
         IF (TIMSEL) THEN
            MSGTXT = '*** You specified TIMERANGE, '
     1               // 'but left REFDAY blank   ***'
            CALL MSGWRT(8)
            MSGTXT = '*** This combination may lead to problems'
     1               // '            ***'
            CALL MSGWRT(8)
            MSGTXT = '*** check your output carefully;'
     1               // ' in case of problems ***'
            CALL MSGWRT(8)
            MSGTXT = '*** either specify both, '
     1               // 'or leave both blank         ***'
            CALL MSGWRT(8)
            END IF
         CALL GREG (XDAT, OBSDAT)
      ELSE
         OBSDAT = REFDAY
         END IF
      CALL OFLAGS (SHANT, BADPTG, OFFSOU, OTHERF)
C                                       Send user a message.
      WRITE (MSGTXT,1050) SELPGM, MCFREV
      CALL MSGWRT (3)
C                                       Looking at Sun?
      ISSUN = SOLAR .AND. (MCALCD(1:1).EQ.' ')
C                                       Initialization done
      IRET(1) = -1
      GO TO 999
C                                       ====== Next record
C
C                                       Determine antennas
 100  BLNDX = BLNDX + 1
C                                       Check if time for a read.
      IF (BLNDX.GT.((MCNANT*(MCNANT+1))/2)) THEN
C                                       Time for new output files?
         DONEW = NEWFIL
C                                       Look for next data

         CALL MCREC ('READ', IERR)
C                                       Check for end of data.
         IRET(1) = IERR
         IF (IERR.EQ.4) IRET(1) = -3
         IF (ISEOT) IRET(1) = -3
C                                       Quit if too many parity errors
C                                       But keep what you have
         IF (IERR.EQ.3) IRET(1) = -3
         IF (IERR.NE.0) GO TO 999
C                                       New output files?
         IF ((DONEW) .OR. (OSTREM.NE.NSTREM) .OR. (BAIL)) THEN
            BLNDX = 0
            IRET(1) = -4
            BAIL = .FALSE.
            GO TO 999
            END IF
         CALL OFLAGS (SHANT, BADPTG, OFFSOU, OTHERF)
C                                       Baseline index
         BLNDX = 1
C                                       Looking at Sun?
         ISSUN = SOLAR .AND. (MCALCD(1:1).EQ.' ')
         END IF
C                                       Copy data from common
      IA1 = ANTBAS(BLNDX,1)
      IA2 = ANTBAS(BLNDX,2)
      BLPTR = BLNDX - 1
      BLPTR = BLPTR * LENBAS
      GOOD = F
C                                       Fill data into output streams
      DO 200 LOOP=1,NSTREM
         CALL FLMFLL (IA1, IA2, BLPTR, LOOP, BUFFER(IPTRO(LOOP), LOOP),
     *      GOODS)
         IF (.NOT.GOODS) IRET(LOOP)=-2
         GOOD = GOOD .OR. GOODS
 200     CONTINUE
C                                       Count flagged vis only when they
C                                       are GOOD otherwise.  Go from
C                                       most severe to most benign
      IF (GOOD) THEN
         IF (OTHERF(IA1) .OR. OTHERF(IA2)) THEN
            NOTHER = NOTHER + 1
            GOOD = .FALSE.
         ELSE IF (OFFSOU(IA1) .OR. OFFSOU(IA2)) THEN
            NOFF = NOFF + 1
            GOOD = .FALSE.
         ELSE IF (SHANT(IA1) .OR. SHANT(IA2)) THEN
            NSHAD = NSHAD + 1
            GOOD = .FALSE.
         ELSE IF (BADPTG(IA1) .OR. BADPTG(IA2)) THEN
            NBADPT = NBADPT + 1
            GOOD = .FALSE.
            END IF
         END IF
      IF (GOOD) GO TO 999
      CALL FILL (MXSTRM, 0, IRET)
      GO TO 100
C                                       ====== Close MODCOMP file
 900  IF (DODISK) THEN
         CALL FLDKIO ('CLOS', FDVEC, TAPBUF, TAPIND, IRET(1))
      ELSE
         CALL TAPIO ('CLOS', FDVEC, TAPBUF, TAPIND, IRET(1))
         END IF
      IRET(1) = 0
C                                       Tell no. flagged vis.
      IF (NSHAD.GT.0) THEN
         WRITE (MSGTXT,1060) NSHAD, SHADOW*VELITE/1.0D9
         CALL MSGWRT (5)
         END IF
      IF (NBADPT.GT.0) THEN
         WRITE (MSGTXT,1070) NBADPT
         CALL MSGWRT (5)
         END IF
      IF (NOFF.GT.0) THEN
         WRITE (MSGTXT,1080) NOFF
         CALL MSGWRT (5)
         END IF
      IF (NOTHER.GT.0) THEN
         WRITE (MSGTXT,1090) NOTHER
         CALL MSGWRT (5)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Error',I7,' opening VLA archive data disk file.')
 1010 FORMAT ('Error',I7,' opening VLA archive data tape.')
 1020 FORMAT ('Error',I7,' positioning tape.')
 1030 FORMAT ('No data found meeting selection criteria!')
 1050 FORMAT ('Program = ',A6,'; Tape revision number =',I5,'.')
 1060 FORMAT ('Dropped ',I7,' shadowed vis at limit ',1PE11.4, ' meter')
 1070 FORMAT ('Dropped ',I7,' bad pointing vis')
 1080 FORMAT ('Dropped ',I7,' off source vis')
 1090 FORMAT ('Dropped ',I7,' seriously flagged vis')
      END
      SUBROUTINE OFLAGS (SHANT, BADPTG, OFFSOU, OTHERF)
C-----------------------------------------------------------------------
C   Checks and records on-line system flags
C
C   Input from commons:
C      MCANTS   L(*)     If true, antenna in subarray
C      MCANCB   I(*)     Antenna control bits
C      MCHOLO   L        True if holography mode
C      MCFREV   I        Tape revision
C      IFFLAG   L(4,*)   IF flags
C      AVGUVW   R(3,28)  Average antenna u,v,w in nsec
C      SHADOW   R        Shadow amount
C      ONSHAD   I        Use on-line shadowing bit?
C      PASPTG   L        Pass failed reference pointing?
C      PASOTH   L        Pass more serious errors?
C   Outputs:
C      SHANT    L(*)     Shadow flags
C      BADPTG   L(*)     Reference pointing flags
C      OFFSOU   L(*)     Off source flags
C      OTHERF   L(*)     Off source flags
C-----------------------------------------------------------------------
      LOGICAL   SHANT(*), BADPTG(*), OFFSOU(*), OTHERF(*)
C
      INTEGER   IANT, BITS(32), II
      LOGICAL   F
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                    On-line flags.  Flagging Conditions
C                                    (see computing memo 188):
C                                       bit    condition
C                                        6     off source
C                                        10    shadowed (tape rev >= 20)
C                                        11    refn ptg failed
C                                              (tape rev >= 25)
C                                        16    source change
C                                        17    subreflector out of posn
C                                        18    off source
C                                        19    L6 not locked
C                                        20    L8 not locked
C                                        21    backend filters misset
C                                        22    backend TP out of range
C                                        23    operator flag
C                                        24    fluctuating Tsys
C                                        25    first LO not locked
C                                    Remember that the index in the BITS
C                                    array is 32 - the above number
C
      CALL LFILL (MXANT, F, SHANT)
      CALL LFILL (MXANT, F, BADPTG)
      CALL LFILL (MXANT, F, OFFSOU)
      CALL LFILL (MXANT, F, OTHERF)
      IF (MCHOLO) GO TO 999
      DO 100 IANT = 1, MXANT
         IF (.NOT.MCANTS(IANT)) GO TO 100
         CALL ZGTBIT (32, MCANCB(IANT), BITS)
         IF ((ONSHAD) .AND. (MCFREV.GE.20) .AND. (MCFREV.LT.30))
     *      SHANT(IANT) = BITS(22).EQ.1
         IF ((ONSHAD) .AND. (MCFREV.GE.34))
     *      SHANT(IANT) = BITS(22).EQ.1
         IF (.NOT.PASPTG .AND. (MCFREV.GE.25))
     *      BADPTG(IANT) = BITS(21).EQ.1
         IF (.NOT.PASOTH) THEN
            OFFSOU(IANT) = (BITS(14).EQ.1) .OR. (BITS(26).EQ.1)
            DO 200 II = 7, 16
               IF (II.NE.14)
     *            OTHERF(IANT) = OTHERF(IANT) .OR. (BITS(II).EQ.1)
200            CONTINUE
            END IF
100      CONTINUE
C if tape revision too old, have to do it the old-fashioned way...
C                                       also post-ModComp era 30-33
      IF ((.NOT.ONSHAD .AND. (SHADOW.GT.0.0)) .OR.
     *    (ONSHAD .AND. (MCFREV.LT.20)) .OR.
     *    (ONSHAD .AND. (MCFREV.GE.30) .AND. (MCFREV.LT.34))) THEN
         CALL MCSHAD (SHANT)
         END IF
999   RETURN
      END
      SUBROUTINE FLMFLL (IA1, IA2, BLPTR, NST, ST, GOODS)
C-----------------------------------------------------------------------
C   This is the routine which actually fills the visibility record into
C   the output data stream.
C
C   In "holography" data from the VLA, reference antennas are pointed
C   towards the nominal source position, as normal.  All other antennas
C   are pointed to an offset position controlled by the observer.  The
C   angular offset from the nominal position, for each antenna, is
C   written to tape in the same variables that are normally used for
C   u,v,w.  In this mode, only data on a baseline between a reference
C   antenna and an offset (non-reference) is considered valid.
C
C   Inputs:
C      IA1      I        Antenna ID #1
C      IA2      I        Antenna ID #2
C      BLPTR    I        Pointer to data for this baseline
C      NST      I        Output stream number
C   Input from AIPS commons:
C      DOUVCM   L        True if data to be compressed format
C   Input from MODCOMP commons:
C      MCDATA   R(*)     Visibility data accumulation array.  Baseline
C                        pointer in MC data for IA1<IA2 is:
C                        ((IA1-1)*MCNANT) - (((IA1+1)*IA1)/2) + IA2
C      STLEN    I(*)     No. words in vis. data in each stream
C      STPNT    I(*)     Pointer in MCDATA for next vis.  1/stream
C      DOAC     L(*)     If true keep auto correlations 1/stream
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STNOIF   I(*)     No. IFs in each stream
C      STNOCH   I(*)     No. Channels in each stream
C      STNOPL   I(*)     No. polarizations in each stream
C      STOFFS   I(*)     0-rel index of Stokes parameter in each stream
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCANCB   I(28)    Antenna control bits
C      MCHOLO   L        True if holography mode
C   Input from FILLM commons:
C      AVGUVW   R(3,28)  Average antenna u,v and w. (nsec)
C      AVGIAT   D        Average IAT end time (Days)
C      CURSOU   I(*)     Current source ID number per stream
C      CURFQI   I(*)     Current FQ id number per stream
C      ISSUN    L        True if observing the sun
C      KLOCWT   I        Offset from beginning of vis record of WT
C   Input from UVHDR common:
C      ILOCU    I        Offset from beginning of vis record of U
C      ILOCV    I        Offset from beginning of vis record of V
C      ILOCW    I        Offset from beginning of vis record of W
C      ILOCT    I                      "                        Time
C      ILOCB    I                      "                    Baseline
C      ILOCSU   I                      "                    Source id
C      ILOCFQ   I                      "                    Freq id
C      NRPARM   I        Number of random parameters
C   Outputs:
C      ST(*)    R        Visibilty data
C      GOODS    L        Data quality flag
C-----------------------------------------------------------------------
      REAL      ST(*)
      INTEGER   NST, IA1, IA2, BLPTR
      LOGICAL   GOODS
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DUVH.INC'
      DOUBLE PRECISION SKYF
      INTEGER   INDXX, NCOPY, LOOP, MCHAN, MPOLN, NREF, IANREF, BIT(32)
      REAL      ORDBUF(UVBFSS)
C-----------------------------------------------------------------------
C                                       Holography mode?
      IF (MCHOLO) THEN
C                                       Identify reference antennas.
         IANREF = IA1
         NREF = 0
         CALL ZGTBIT (32, MCANCB(IA1), BIT)
         IF (BIT(31).NE.0) THEN
            NREF = NREF + 1
            IANREF = IA2
            END IF
         CALL ZGTBIT (32, MCANCB(IA2), BIT)
         IF (BIT(31).NE.0) THEN
            NREF = NREF + 1
            IANREF = IA1
            END IF
         GOODS = NREF.EQ.1.OR.ALLREF
         IF (.NOT. GOODS) GO TO 999
C                                       Store pseudo values for u,v
         ST(1+ILOCU) = AVGUVW(1,IANREF)
         ST(1+ILOCV) = AVGUVW(2,IANREF)
         ST(1+ILOCW) = AVGUVW(3,IANREF)
      ELSE
C                                       Store real values for u,v,w
         SKYF = CATFRQ(NST)
         ST(1+ILOCU) = (AVGUVW(1,IA1) - AVGUVW(1,IA2)) * SKYF
         ST(1+ILOCV) = (AVGUVW(2,IA1) - AVGUVW(2,IA2)) * SKYF
         ST(1+ILOCW) = (AVGUVW(3,IA1) - AVGUVW(3,IA2)) * SKYF
         END IF
C
      ST(1+ILOCB) = IA1 * 256 + IA2
      ST(1+ILOCT) = AVGIAT
      ST(1+ILOCSU) = CURSOU(NST)
      ST(1+ILOCFQ) = CURFQI(NST)

C                                       Check if good
      INDXX = BLPTR + STPNT(NST)
      GOODS = .FALSE.
      NCOPY = STLEN(NST)
C                                       make pseudo-cont here
      IF ((SUMCH0) .AND. (STTYPE(NST).EQ.1)) THEN
         CALL FLMCH0 (BLPTR, NST, GOODS)
C                                       skip if new visibility all zero
      ELSE
         DO 110 LOOP = 0,NCOPY-1,3
            GOODS = GOODS .OR. (MCDATA(LOOP+INDXX).NE.0.0)
     *         .OR. (MCDATA(LOOP+INDXX+1).NE.0.0)
 110        CONTINUE
         END IF
C                                       Trap unwanted auto-correlations
      GOODS = GOODS .AND. (DOAC(NST) .OR. (IA1.NE.IA2))
      IF (GOODS) THEN
C                                       Flip baseline if necessary
         MCHAN = STNOCH(NST) * STNOIF(NST)
         MPOLN = STNOPL(NST)
C                                       Sun?
         IF (ISSUN) THEN
            IF (.NOT.ISCORC) CALL MCSCAL (.TRUE., IA1, IA2, NST,
     *         MCDATA(INDXX))
         ELSE
            IF (((ISCORC) .AND. (.NOT.RICKS)) .OR.
     *         ((.NOT.ISCORC) .AND. (RICKS)))
     *         CALL MCSCAL (RICKS, IA1, IA2, NST, MCDATA(INDXX))
            END IF
C                                       fix weights
         CALL MCWAIT (WTSYS, IA1, IA2, NST, MCDATA(INDXX))
C                                       flip order after using
C                                       IA1, IA2
         IF (IA1.GT.IA2) CALL MCFLIP (MCHAN, MPOLN, STOFFS(NST), ST,
     *      MCDATA(INDXX), MCDATA(INDXX))
C                                       sort axes if needed
         CALL MCORDR (NST, MCDATA(INDXX), ORDBUF)
C                                       compressed?
         IF (DOUVCM) THEN
            CALL ZUVPAK (NCOPY/3, ORDBUF, ST(1+KLOCWT),
     *         ST(1+NRPARM))
         ELSE
            CALL RCOPY (NCOPY, ORDBUF, ST(NRPARM+1))
            END IF
         END IF
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MCORDR (STREAM, VISIN, VISOUT)
C-----------------------------------------------------------------------
C   Put data in output axis order from ModComp axis order
C   Inputs:
C      STREAM   I      Stream number
C      VISIN    R(*)   ModComp Ordered buffer
C   Input from common:
C      STNOIF   I(*)   No. IFs in each stream
C      STNOCH   I(*)   No. Channels in each stream
C      STNOPL   I(*)   No. polarizations in each stream
C      STZMOD   I(*)   Number of zero-lag channels to prepend
C      KINCS    I(*)   Visibility increment in Stokes' for each stream
C      KINCF    I(*)   Visibility increment in Freq for each stream
C      KINCIF   I(*)   Visibility increment in IF for each stream
C   Input/Output:
C      VISOUT   R(*)   Visibilities in output order
C-----------------------------------------------------------------------
      INTEGER   STREAM
      REAL      VISIN(*), VISOUT(*)
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   IIF, IFREQ, IS, NIF, NFREQ, NS, IFOFF, FQOFF, INDXX,
     *   JNCS, JNCF, JNCIF, IFOJJ, FQOJJ, INDJJ
C-----------------------------------------------------------------------
C                                       Setup: input stream
      NIF = STNOIF(STREAM)
      NFREQ = STNOCH(STREAM)
      NS = STNOPL(STREAM)
C                                       output for this stream
      JNCS = 3
      JNCF = JNCS * NS
      JNCIF = JNCF * NFREQ
C                                       Loop correcting data
C                                       for each IF, AC or BD
      DO 500 IIF = 1,NIF
         IFOFF = (IIF-1) * KINCIF(STREAM) + 1
         IFOJJ = (IIF-1) * JNCIF + 1
C                                       for each spectral frequency
         DO 400 IFREQ = 1,NFREQ
            FQOFF = IFOFF + (IFREQ-1) * KINCF(STREAM)
            FQOJJ = IFOJJ + (IFREQ-1) * JNCF
C                                       for each polarization
            DO 300 IS = 1,NS
C                                       if factor is not too small
               INDXX = FQOFF + (IS-1) * KINCS(STREAM)
               INDJJ = FQOJJ + (IS-1) * JNCS
               VISOUT(INDJJ) = VISIN(INDXX)
               VISOUT(INDJJ+1) = VISIN(INDXX+1)
               VISOUT(INDJJ+2) = VISIN(INDXX+2)
 300           CONTINUE
 400        CONTINUE
 500     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FLMCH0 (BLPTR, NST, GOODS)
C-----------------------------------------------------------------------
C   FLMCH0 sums up channel 0 data from the inner 3/4 of the spectral
C   channels.
C   Inputs:
C      BLPTR    I        Pointer to data for this baseline
C      NST      I        Output stream number
C   In/out COMMON
C      GOODS    L        Some data non zero
C-----------------------------------------------------------------------
      INTEGER   BLPTR, NST
      LOGICAL   GOODS
C
      INTEGER   IIF, IIS, IFR, FR1, FR2, INDXL, INDXC, STL, NIF, NFR,
     *   NS, IOFF
      REAL      VR, VI, VW
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      STL = NST + 1
      IF ((STNOIF(NST).NE.STNOIF(STL)) .OR.
     *   (STNOPL(NST).NE.STNOPL(STL))) THEN
         MSGTXT = 'FLMCHO: NUMBER OF POLARIZATIONS DO NOT MATCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NIF = STNOIF(STL)
      NFR = STNOCH(STL)
      NS = STNOPL(STL)
      FR1 = (NFR+1) / 8 + 1 + STZMOD(STL)
      FR2 = NFR - ((NFR+1)/8)
      GOODS = .FALSE.
      DO 50 IIF = 1,NIF
         DO 40 IIS = 1,NS
            IOFF = (IIS-1) * KINCS(STL) + (IIF-1) * KINCIF(STL) + BLPTR
     *         + STPNT(STL)
            INDXC = (IIS-1) * KINCS(NST) + (IIF-1) * KINCIF(NST) + BLPTR
     *         + STPNT(NST)
            VR = 0.0
            VI = 0.0
            VW = 0.0
            INDXL = IOFF
            DO 20 IFR = 1,NFR
               IF ((MCDATA(INDXL).NE.0.0) .OR. (MCDATA(INDXL+1).NE.0.0))
     *            GOODS = .TRUE.
               INDXL = INDXL + KINCF(STL)
 20            CONTINUE
            INDXL = IOFF + (FR1-1) * KINCF(STL)
            DO 30 IFR = FR1,FR2
               IF (MCDATA(INDXC+2).GT.0.0) THEN
                  VR = VR + MCDATA(INDXL) * MCDATA(INDXL+2)
                  VI = VI + MCDATA(INDXL+1) * MCDATA(INDXL+2)
                  VW = VW + MCDATA(INDXL+2)
                  END IF
               INDXL = INDXL + KINCF(STL)
 30            CONTINUE
            IF (VW.GT.0.0) THEN
               MCDATA(INDXC) = VR / VW
               MCDATA(INDXC+1) = VI / VW
            ELSE
               MCDATA(INDXC) = 0.0
               MCDATA(INDXC+1) = 0.0
               MCDATA(INDXC+2) = 0.0
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MCSHAD (SHANT)
C-----------------------------------------------------------------------
C   Finds shadowed data.  If an antenna is shadowed by any other antenna
C   it is flagged.
C   Inputs from MC2 common:
C      SHADOW   R        Shadowing limit in nsec
C      AVGUVW   R(3,*)   Antenna u, v, w in nsec
C      MCANTS   L(*)     Antenna present in subarray?
C   Output:
C      SHANT    L(28)    True if antenna shadowed
C-----------------------------------------------------------------------
      INCLUDE 'MC2.INC'
      LOGICAL   SHANT(MXANT)
C
      INTEGER   IA1, IA2
      REAL      UV2
C-----------------------------------------------------------------------
C                                       Initialize
      DO 10 IA1 = 1,MXANT
         SHANT(IA1) = .FALSE.
 10      CONTINUE
      IF (SHADOW.LT.1.0E-10) GO TO 999
C                                       Loop over antennas
      DO 200 IA1 = 1,MXANT
C                                       Don't do missing antennas
         IF (.NOT.MCANTS(IA1)) GO TO 200
         DO 100 IA2 = 1,MXANT
C                                       Is this antenna in front of
C                                       the one under consideration?
            IF (MCANTS(IA2) .AND. (IA2.NE.IA1) .AND.
     *          (AVGUVW(3,IA2).GT.AVGUVW(3,IA1))) THEN
C                                       OK, check the separation
               UV2 = SQRT ((AVGUVW(1,IA1) - AVGUVW(1,IA2))**2 +
     *            (AVGUVW(2,IA1) - AVGUVW(2,IA2))**2)
C                                       and flag if warranted
C               SHANT(IA1) = SHANT(IA1) .OR. (UV2.LE.SHADOW)
               IF (UV2.LE.SHADOW) THEN
                  SHANT(IA1) = .TRUE.
                  END IF
               END IF
 100        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MCSCAL (DIVIDE, IA1, IA2, STREAM, VIS)
C-----------------------------------------------------------------------
C   Scale correlation coefficients by the  "nominal sensitivity".
C   Inputs:
C      DIVIDE   I         Divide by the nominal sensitivity?
C      IA1      I         First antenna number
C      IA2      I         Second antenna number
C      STREAM   I         Stream number
C   Input from common:
C      MCANNS   R(4,28)   "Nominal" sensitivity
C      MCANTF   R(4,28)   Front-end derived system temperature
C      MCANTB   R(4,28)   Back-end derived system temperature
C      STMCI1   I(*)      VLA reference IF for 1st IF of stream
C                         (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)      VLA reference IF for 2nd IF of stream
C      STNOIF   I(*)      No. IFs in each stream
C      STNOCH   I(*)      No. Channels in each stream
C      STNOPL   I(*)      No. polarizations in each stream
C      STZMOD   I(*)      Number of zero-lag channels to prepend
C      KINCS    I(*)      Visibility increment in Stokes' for each stream
C      KINCF    I(*)      Visibility increment in Freq for each stream
C      KINCIF   I(*)      Visibility increment in IF for each stream
C   Input/Output:
C      VIS      R(*)      Visibilities
C-----------------------------------------------------------------------
      LOGICAL   DIVIDE
      INTEGER   IA1, IA2, STREAM
      REAL      VIS(*)
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   IIF, IFREQ, IS, NIF, NFREQ, NS, RXOFF, IFOFF, FQOFF,
     *   INDXX, IP1, IP2, IPP1(4), IPP2(4), COUNT
      REAL      CORFAC(8), CFACT
      SAVE COUNT
C                                       Count Number of print messages
      DATA COUNT/0/
C                                       Set indexes to CORFAC array
C     DATA IPP1 /1,1,3,3/, IPP2 /5,7,5,7/   old values
      DATA IPP1 /1,3,1,3/, IPP2 /5,7,7,5/
C-----------------------------------------------------------------------
C                                       Correction factors
      DO 100 IS = 1,4
         IF (MCANNS(IS,IA1).EQ.FBLANK) THEN
            CORFAC(IS) = 0.0
         ELSE IF (MCANNS(IS,IA1).GT.1.0E-10) THEN
            CORFAC(IS) = SQRT (MCANNS(IS,IA1))
            IF (DIVIDE) CORFAC(IS) = 1.0 / CORFAC(IS)
         ELSE
            CORFAC(IS) = SQRT (0.333)
            END IF
         IF (MCANNS(IS,IA2).EQ.FBLANK) THEN
            CORFAC(IS+4) = 0.0
         ELSE IF (MCANNS(IS,IA2).GT.1.0E-10) THEN
            CORFAC(IS+4) = SQRT (MCANNS(IS,IA2))
            IF (DIVIDE) CORFAC(IS+4) = 1.0 / CORFAC(IS+4)
         ELSE
            CORFAC(IS+4) = SQRT (0.333)
            END IF
 100        CONTINUE
C                                       Setup
      NIF = STNOIF(STREAM)
      NFREQ = STNOCH(STREAM)
      NS = STNOPL(STREAM)
      RXOFF = STMCI1(STREAM) - 1
      IF (RXOFF.GT.1) RXOFF = RXOFF - 2
C                                       Receiver offset either 0 or 1
      RXOFF = MAX( MIN( RXOFF, 1), 0)
C                                       Loop correcting data
C                                       for each IF, AC or BD
      DO 500 IIF = 1,NIF
         IFOFF = (IIF-1) * KINCIF(STREAM) + 1
C                                       for each spectral frequency
         DO 400 IFREQ = 1,NFREQ
            FQOFF = IFOFF + (IFREQ-1) * KINCF(STREAM)
C                                       for each polarization
            DO 300 IS = 1,NS
C                                       factor for both antennas and
C                                       each polarizaion LCP, RCP
               IP1 = IPP1(IS) + RXOFF
               IP2 = IPP2(IS) + RXOFF
               CFACT = CORFAC(IP1) * CORFAC(IP2)
C                                       if factor is not too small
               INDXX = FQOFF + (IS-1) * KINCS(STREAM)
               IF (CFACT.EQ.0.0) THEN
                  VIS(INDXX+2) = - ABS(VIS(INDXX+2))
               ELSE IF (ABS(CFACT).GT.1.0E-10) THEN
                  VIS(INDXX)   = VIS(INDXX)   * CFACT
                  VIS(INDXX+1) = VIS(INDXX+1) * CFACT
               ELSE
                  VIS(INDXX+2) = - ABS(VIS(INDXX+2))
                  IF (COUNT.LT.4) THEN
                     WRITE(MSGTXT,1000,ERR=999) IP1, CORFAC(IP1),
     *                  IP2, CORFAC(IP2)
                     CALL MSGWRT(6)
                     WRITE(MSGTXT,1100,ERR=999) IIF, IFREQ,
     *                  IS, STREAM
                     CALL MSGWRT(6)
                     WRITE(MSGTXT,1200,ERR=999) KINCIF(STREAM),
     *                  KINCF(STREAM), KINCS(STREAM)
                     CALL MSGWRT(6)
                     COUNT = COUNT + 1
                     END IF
                  END IF
 300           CONTINUE
 400        CONTINUE
C                                       Other IF
         RXOFF = STMCI2(STREAM) - 1
         IF (RXOFF.GT.1) RXOFF = RXOFF - 2
C                                       Receiver offset either 0 or 1
         RXOFF = MAX( MIN( RXOFF, 1), 0)
 500     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('MCSCAL: Factor error; C(',I3,')=',E10.3,
     *        ' * C(',I3,')=',E10.3)
1100  FORMAT ('MCSCAL: IF    =',I3,', FREQ    =',I3,', IS     =',I3,
     *        ', STRM=',I3)
1200  FORMAT ('MCSCAL: IF inc=',I5,', FREQ inc=',I3,', POL inc=',I3)
      END
      SUBROUTINE MCWAIT (DOWAIT, IA1, IA2, STREAM, VIS)
C-----------------------------------------------------------------------
C   Correct weights to be based on bandwidth and system temperatures
C   Inputs:
C      DOWAIT   I        Apply weights or just flag on nom sens bad
C      IA1      I        First antenna number
C      IA2      I        Second antenna number
C      STREAM   I        Stream number
C   Input from common:
C      MCANNS   R(4,28)  "Nominal" sensitivity
C      MCANTF   R(4,28)  Front-end derived system temperature
C      MCANTB   R(4,28)  Back-end derived system temperature
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                         (1,2,3,4=>A,B,C,D)
C      STMCI2  I(*)      VLA reference IF for 2nd IF of stream
C      STNOIF  I(*)      No. IFs in each stream
C      STNOCH  I(*)      No. Channels in each stream
C      STNOPL  I(*)      No. polarizations in each stream
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      KINCS   I(*)      Visibility increment in Stokes' for each stream
C      KINCF   I(*)      Visibility increment in Freq for each stream
C      KINCIF  I(*)      Visibility increment in IF for each stream
C   Input/Output:
C      VIS     R(*)      Visibilities
C-----------------------------------------------------------------------
      INTEGER   DOWAIT, IA1, IA2, STREAM
      REAL      VIS(*)
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   IIF, IFREQ, IS, NIF, NFREQ, NS, RXOFF, IFOFF, FQOFF,
     *   INDXX, IP1, IP2, IPP1(4), IPP2(4), COUNTW, IBW
      CHARACTER BBAND*1, MDATE*8
      LOGICAL   POR4
      DOUBLE PRECISION ODATE, CDATE
      REAL      CORFAC(8), CFACT, RBW, XBW, EXTRA
      EQUIVALENCE (IBW, RBW)
      SAVE COUNTW
C                                       Count Number of print messages
      DATA COUNTW/0/
C                                       Set indexes to CORFAC array
      DATA IPP1 /1,3,1,3/, IPP2 /5,7,7,5/
      DATA MDATE /'19980730'/
C-----------------------------------------------------------------------
C                                       Find band
      CALL FLMBC (FREQ, BBAND)
      POR4 = (BBAND.EQ.'P') .OR. (BBAND.EQ.'4')
      IBW = CATOUT(KRCIC+JLOCF,STREAM)
      IF (RBW.LE.0.0) RBW = 1000.0
C                                       Input weight = deltat/[10 sec]
C                                       RBW = bandwidth in Hz
C                                       Use: input wt = 1 => 10 sec
C                                       K = 0.1 = ???
C                                       eta_c = 0.78 = corr. efficiency
C                                       factor
C                                       .12 = 2 * (k*etac)**2 * Deltat
      XBW = SQRT (0.12 * RBW) / SQRT (1000.)
C                                       Correction factors
      DO 10 IS = 1,4
         IF (MCANNS(IS,IA1).EQ.FBLANK) THEN
            CORFAC(IS) = 0.0
         ELSE IF (DOWAIT.LE.0) THEN
            CORFAC(IS) = 1.0
         ELSE IF (RICKS) THEN
            CORFAC(IS) = XBW
         ELSE IF (MCANNS(IS,IA1).GT.1.0E-10) THEN
            CORFAC(IS) = XBW / MCANNS(IS,IA1)
         ELSE
            CORFAC(IS) = XBW / 0.333
            END IF
         IF (MCANNS(IS,IA2).EQ.FBLANK) THEN
            CORFAC(IS+4) = 0.0
         ELSE IF (DOWAIT.LE.0) THEN
            CORFAC(IS+4) = 1.0
         ELSE IF (RICKS) THEN
            CORFAC(IS+4) = XBW
         ELSE IF (MCANNS(IS,IA2).GT.1.0E-10) THEN
            CORFAC(IS+4) = XBW / MCANNS(IS,IA2)
         ELSE
            CORFAC(IS+4) = XBW / 0.333
            END IF
 10      CONTINUE
C                                       Setup
      NIF = STNOIF(STREAM)
      NFREQ = STNOCH(STREAM)
      NS = STNOPL(STREAM)
      RXOFF = STMCI1(STREAM) - 1
      IF (RXOFF.GT.1) RXOFF = RXOFF - 2
C                                       Receiver offset either 0 or 1
      RXOFF = MAX (MIN (RXOFF, 1), 0)
C                                       Bryan Butler factor (Memo 108)
      EXTRA = 1.0
      IF (DOWAIT.EQ.2) THEN
         CALL JULDAY (OBSDAT, ODATE)
         CALL JULDAY (MDATE, CDATE)
         IF ((STTYPE(STREAM).GT.0) .OR. (ODATE.LE.CDATE)) THEN
            EXTRA = 2.3667
         ELSE
            EXTRA = 2.9417
            END IF
         END IF
C                                       Loop correcting data
C                                       for each IF, AC or BD
      DO 500 IIF = 1,NIF
         IFOFF = (IIF-1) * KINCIF(STREAM) + 1
C                                       for each spectral frequency
         DO 400 IFREQ = 1,NFREQ
            FQOFF = IFOFF + (IFREQ-1) * KINCF(STREAM)
C                                       for each polarization
            DO 300 IS = 1,NS
C                                       factor for both antennas and
C                                       each polarizaion LCP, RCP
               IP1 = IPP1(IS) + RXOFF
               IP2 = IPP2(IS) + RXOFF
               CFACT = CORFAC(IP1) * CORFAC(IP2) * EXTRA
C                                       if factor is not too small
               INDXX = FQOFF + (IS-1) * KINCS(STREAM)
               IF (CFACT.EQ.0.0) THEN
                  VIS(INDXX+2) = -ABS (VIS(INDXX+2))
               ELSE IF (ABS(CFACT).GT.1.0E-10) THEN
                  VIS(INDXX+2) = VIS(INDXX+2) * CFACT
               ELSE
                  VIS(INDXX+2) = -ABS (VIS(INDXX+2))
                  IF (COUNTW.LT.4) THEN
                     WRITE(MSGTXT,1000,ERR=999) IP1, CORFAC(IP1),
     *                                          IP2, CORFAC(IP2)
                     CALL MSGWRT (6)
                     WRITE(MSGTXT,1100,ERR=999) IIF, IFREQ,
     *                  IS, STREAM
                     CALL MSGWRT (6)
                     WRITE(MSGTXT,1200,ERR=999) KINCIF(STREAM),
     *                  KINCF(STREAM), KINCS(STREAM)
                     CALL MSGWRT (6)
                     COUNTW = COUNTW + 1
                     END IF
                  END IF
 300           CONTINUE

 400        CONTINUE
C                                       Other IF
         RXOFF = STMCI2(STREAM) - 1
         IF (RXOFF.GT.1) RXOFF = RXOFF - 2
C                                       Receiver offset either 0 or 1
         RXOFF = MAX (MIN (RXOFF, 1), 0)
 500     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('MCWAIT: Factor error; C(',I3,')=',E10.3,
     *        ' * C(',I3,')=',E10.3)
1100  FORMAT ('MCWAIT: IF    =',I3,', FREQ    =',I3,', IS     =',I3,
     *        ', STRM=',I3)
1200  FORMAT ('MCWAIT: IF inc=',I5,', FREQ inc=',I3,', POL inc=',I3)
      END
      SUBROUTINE MCREC (OPCODE, IRET)
C-----------------------------------------------------------------------
C   Reads records from a MODCOMP file and converts it into local data.
C   FORMAT 1 (data) records will be selected by data selection criteria
C   obtained from common and desired data records will be returned.
C   Various data validity checks will be made.
C   Inputs:
C      OPCODE   C*4      Opcode 'READ' or 'INIT'.  INIT will set up I/O
C                        and begin processing data, returning the first
C                        selected FORMAT 1 record
C   Input/output in common:
C      SELECT   R(11)    Selection parameters
C                         1 => source qualifier, -1 => all
C                         5 => Averaging time (rad.)
C                         7 => subarray, 0 => any
C                         8 => No. channels wanted
C                         9 => Start time (rad)
C                        10 => End time (rad)
C                        11 => Cal averaging time (rad.)
C      SELBAN   C*1      Band code 1st IF (blank => any)
C      SELBA2   C*1      Band code 2nd IF (blank => any)
C      SELPGM   C*6      Observing program name
C      SELMOD   C*2      Observing mode (blank => any)
C   Output in common:
C      BANDAC   C*2      Band code for AC continuum data (e.g. 'C')
C      BANDBD   C*2      Band code for BD continuum data
C      MCBLK    I        Current tape block number
C      MCNBLK   I        Number of tape blocks per logical record
C      MCLRL    I        Logical Record length (words)
C      MCFMT    I        Format type (only 1 wanted)
C      MCFREV   I        Format revision level
C      MCDATE   I        Date (MJAD)
C      MCIATC   I        IAT (interupt count since midnight)
C      MCCPID   C*6      Control program ID
C      MCSDA    I        Pointer to subarray data area (in tape buffer,
C                        in 16 bit words)
C      MCADA    I        Pointer to first antenna data area
C      MCLADA   I        Length (words) of each antenna data area
C      MCNANT   I        Number of antennas in this subarray
C      MCCDA    I(4)     Pointer to correlator data area, 1 per CDA
C      MCLCDA   I(2,4)   Length of CDA baseline entry, header (MC words)
C      MCNBPR   I        Number of 2KB blocks per integration
C      MCSAID   I        Subarray ID
C      MCSNAM   C*16     Source name
C      MCQUAL   I        Source qualifier
C      MCARRC   C*2      Array Configuration (1 char?)
C      MCPGID   C*8      Observing program ID
C      MCUSID   I        User ID number
C      MCOMOD   C*2      Observing mode
C      MCALCD   C*2      Calibrator code
C      MCRCV    C*2      Observing band code
C      MCINTG   I        Integration time in waveguide cycles
C      MCLSTE   R        LST stop time (radians)
C      MCLSTB   R        LST start time (radians)
C      MCRAEP   D        Right Ascension at standard epoch (radians)
C      MCDCEP   D        Declination at standard epoch (radians)
C      MCRAAP   D        Apparent Right Ascension (radians)
C      MCDCAP   D        Apparent Declination (radians)
C      MCSSLO   D(4)     Signed sum of LOs IFs A-D (GHz)
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCIATI   D        IAT at middle of integration (radians)
C      MCLSTI   D        LST at end of integration (radians)
C      MCIATG   D        IAT of geometry computations
C      MCSR     R        Current surface refractivity (n-1)
C      MCZAPP   R        Estimated zenith atmos. phase path (nsec)
C      MCSCOS   R(6)     Sin and cos of h, A and eta
C      MCBCOD   I        Bandwidth code
C      MCFEFC   I        Front end control codes
C      MCRCC    I        Recirculator control codes
C      MCZSF    R        Zero spacing flux (IF?)
C      MCUVLM   R(2)     UV limits for online ANTSOL (nsec?)
C      MCACB    I        Array control bits
C      MCWEAT   R(5)     Weather info (???)
C      MCRVEL   D(4)     Radial velocity for IFs A-D (Km/sec)
C      MCREST   D(4)     Line rest Frequency for IFs A-D (MHz)
C      MCVRF    C*8      Velocity rest frame  2 per IF
C                           1) 'G'=geocentric, 'T'=topocentric,
C                              'B'=barycentric, 'L'=LSR
C                           2) 'V'=radio, 'Z'=optical, 'O'=offset
C      MCCORM   C*4      Correlator mode (? char)
C      MCAPOP   C*4      AP options (4 char ?)
C      MCEPOC   I        Epoch year
C      MCCHOF   I(4)     Channel offsets
C      MCANCB   I(28)    Antenna control bits
C      MCANNS   R(4,28)  Antenna nominal sensitivity (unitless)
C      MCANTF   R(4,28)  Front-end derived system temperature
C      MCANTB   R(4,28)  Back-end derived system temperature
C      MCANPD   R(4,28)  Antenna peculiar delay 1/IF (nsec)
C      MCANPP   I(4,28)  Antenna peculiar phase (turns) 1/IF
C      MCIFCB   I(4,28)  Antenna control bits 1/IF
C      MCANTD   D(28)    Antenna total delay at"geometry" epoch (nsec)
C      MCAUVW   R(3,28)  Antenna u,v, and w at center of integration
C                        for specified Epoch (nsec)
C      MCAXYZ   D(3,28)  Antenna x, y, and z (nsec)
C      MCANBA   R(28)    Antenna Ba (k-term)
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      MCMODE   I        Correlator mode code:
C                         1=cont,
C                         2=1A, 3=1B, 4=1C, 5=1D,
C                         6=2AB, 7=2AC, 8=2AD, 9=2BC, 10=2BD, 11=2CD,
C                        12=4, 13=PA, 14=PB,
C                        15=1A/C, 16=1B/C, 17=1C/C, 18=1D/C,
C                        19=2AC/C, 20=2BD/C,
C                        21=2A, 22=4A
C      MCBCH    I        First channel number selected
C      MCECH    I        Highest channel number selected
C      NSTREAM  I        Number of output streams required for data
C      LENBAS   I        No. words per baseline, incl. all streams
C      STLEN    I(*)     No. words in vis. data in each stream
C      STPNT    I(*)     Pointer in MCDATA for next vis.  1/stream
C      DOAC     L(*)     If true keep auto correlations 1/stream
C      STFREQ   D(*)     Reference frequency for each stream
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)     VLA reference IF for 2nd IF of stream
C      STNOIF   I(*)     No. IFs in each stream
C      STNOCH   I(*)     No. Channels in each stream
C      STNOPL   I(*)     No. polarizations in each stream
C      STOFIF   I(*)     0-rel index of IF axis in each stream
C      STOFFF   I(*)     0-rel index of Frequency axis in each stream
C      STOFFS   I(*)     0-rel index of Stokes parameter in each stream
C      STBCH    I(*)     1st channel to select in each stream
C      STECH    I(*)     Highest channel to select in each stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      STCDA    I(4)     Stream number for each DCA, 0=> ignore
C      DOCH0    I(4)     If true, first channel in CDA is "channel 0"
C                        STCDA points to ch 0, +3 to first line channel
C      STCOFF   I(4,2)   0-rel offset of 1st word of each CDA to
C                        baseline data area in MCDATA for each 1st
C                        correlator in baseline and for the first
C                        spectral channel selected or the second
C                        polarization for continuum
C      ANTBAS   I(500,2) Gives first and second antenna for each
C                        baseline index
C      MCLNF1   I        Number of Format 1 data records accumulated
C      MCLPR    I(*)     Last partial baseline record as local integers
C      MCWLEF   I        Number of (16 bit) words in MCLPR
C      MCDATA   R(*)     Visibility data accumulation array.  Baseline
C                        pointer in MC data for IA1<IA2 is:
C                        ((IA1-1)*MCNANT) - (((IA1+1)*IA1)/2) + IA2
C      AVGUVW   R(3,28)  Average antenna u,v and w. (nsec)
C      AVGIAT   D        Average IAT end time (Days)
C      SUMINT   R        Sum of integration time (seconds)
C   Output:
C      IRET     I        Return error code:
C                           0=> valid data, 4=>end of file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER OPCODE*(*)
C
      DOUBLE PRECISION PTIME
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      PTIME = AVGIAT
      CALL MCRECO (OPCODE, IRET)
      IF (AVGIAT.LT.PTIME) THEN
         MSGTXT = '**************************************************'
         CALL MSGWRT (8)
         CALL MSGWRT (8)
         MSGTXT = '******        DATA NOT IN TIME ORDER        ******'
         CALL MSGWRT (8)
         MSGTXT = '******    UVSRT + INDXR WILL BE REQUIRED    ******'
         CALL MSGWRT (8)
         MSGTXT = '**************************************************'
         CALL MSGWRT (8)
         CALL MSGWRT (8)
         END IF
C
C     If end of file is detected (IRET = 4) then advance over files
C     until the time range is exceeded, NCOUNT files have been
C     read, or an error is detected. Note that MCRECO updates NOFILE
C     as needed.
C
 10   IF ((IRET.EQ.4) .AND. (STIAT.LT.SELECT(10)) .AND. (.NOT.ISEOT)
     *    .AND. (NOFILE.LT.NCOUNT)) THEN
         CALL MCRECO ('INIT', IRET)
C
C        Redo the original call if valid data was found:
C
C         IF ((IRET.EQ.0) .AND. (OPCODE.NE.'INIT')) CALL MCRECO (OPCODE,
C     *      IRET)
         IF (AVGIAT.LT.PTIME) THEN
            MSGTXT = '***********************************************'
            CALL MSGWRT (8)
            CALL MSGWRT (8)
            MSGTXT = '*****    DATA FILES NOT IN TIME ORDER    ******'
            CALL MSGWRT (8)
            MSGTXT = '*****   UVSRT + INDXR WILL BE REQUIRED   ******'
            CALL MSGWRT (8)
            MSGTXT = '***********************************************'
            CALL MSGWRT (8)
            CALL MSGWRT (8)
            END IF
         GO TO 10
         END IF
C
C     If a read error is detected then change the status to indicate
C     end of file to prevent repeated failures:
C
      IF (IRET.NE.0) IRET = 4
C
 999  RETURN
      END
      SUBROUTINE MCRECO (OPCODE, IRET)
C-----------------------------------------------------------------------
C   Returns a selected, averaged Modcomp archive record of FORMAT 1
C   Inputs:
C      OPCODE   C*4      OPCODE 'READ' => read record,
C                               'INIT' = Initialize
C   Output:
C      IRET     I        Return code. 0=>OK, 4=>end of data,
C                        10=data not Modcomp archive.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      INTEGER   BLKNO, MSGSAV, LOOP, LIM
      LOGICAL   T, F, DOINIT, NEXTR, PERROR
C
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      PERROR = F
C                                       Init I/O (using TAPIO)
      DOINIT = OPCODE.EQ.'INIT'
      IF (DOINIT) THEN
         GOTHED = F
         IF (.NOT.ISEOF) THEN
            IF ((DODISK) .AND. (NOFILE.GT.0)) THEN
               CALL FLDKIO ('CLOS', FDVEC, TAPBUF, TAPIND, IRET)
               FDVEC(33) = FDVEC(33) + 1
               CALL FLDKIO ('OPRD', FDVEC, TAPBUF, TAPIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, FDVEC(33)
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               END IF
            NOFILE = NOFILE + 1
            END IF
         MCLNF1 = 0
         END IF
C                                       Get next MODCOMP record.
 100  CALL MCHEAD (DOINIT, NEXTR, IRET)
C                                       Check end of file, PE
      PERROR = IRET.EQ.3
      IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 950
      IF (IRET.NE.0) GO TO 999
C                                       See if integration finished
      IF (NEXTR) GO TO 300
C                                       Accumulate
      BLKNO = 1
      CALL MCSUM (BLKNO)
C                                       Do rest of record
      LIM = MCNBLK
      DO 200 LOOP = 2,LIM
C                                       Read next block.
         CALL MCREAD (IRET)
C                                       Check end of file, PE
         PERROR = IRET.EQ.3
         IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 950
         IF (IRET.NE.0) GO TO 999
C                                       Accumulate
         BLKNO = LOOP
C                                       Check block number
         IF (BLKNO.NE.MCBLK) THEN
C                                       Lost sync. - treat as parity
C                                       error.
            PERROR = T
            GO TO 950
            END IF
         CALL MCSUM (BLKNO)
 200     CONTINUE
C                                       Next record
      GO TO 100
C                                       Finished accumulation,
C                                       normalize RECORD.
 300     CALL MCNORM
C                                       Done - return
         GO TO 999
C                                       Parity error
 950  IF (PERROR) THEN
C                                       Ignore rest of record.
         BLKNO = BLKNO + 1
         IF (BLKNO.LE.MCNBLK) THEN
            LIM = MCNBLK
            DO 960 LOOP = BLKNO,LIM
C                                       Suppress ZERROR shit
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL MCREAD (IRET)
               MSGSUP = MSGSAV
C                                       Too many errors?
               IF (PECNT.GT.PELIM) THEN
                  IRET = 4
                  GO TO 999
                  END IF
C                                       Only care about EOF
               IF (IRET.EQ.4) GO TO 999
C                                       If block 1 process
               IF (MCBLK.EQ.1) THEN
                  GOTHED = T
C                                       Force new integration
                  STIAT = -1.0E10
                  GO TO 100
                  END IF
 960           CONTINUE
            END IF
         END IF
      PERROR = F
C                                       Next record
      IF ((IRET.NE.4) .AND. (.NOT.ISEOT)) GO TO 100
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCRECO: ERROR',I5,' OPENING DISK FILE',I5)
      END
      SUBROUTINE MCREAD (IRET)
C-----------------------------------------------------------------------
C   Reads a MODCOMP VLA archive format record.
C   Inputs:
C      OPCODE   C*4      Opcode for I/O, only READ supported
C   Output:
C      IRET     I        Return code, 0=>OK, 4=>EOF, otherwise failed
C   Output in common:
C      MCBLK    I        Physical block number (of MCNBLK)
C      MCNBLK   I        Number of blocks in logical record
C      ISEOF    L        If true then an EOF was found
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LRWORD
      LOGICAL   T, F, ISDONE
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Trap previous EOF
      IF (ISEOF) THEN
         ISEOF = F
         IRET = 4
         GO TO 999
         END IF
C                                       Ignore if error limit exceeded
      IF (PECNT.GT.PELIM) THEN
         IRET = 4
         GO TO 999
         END IF
C                                       Check TELL
      CALL MCTELL (ISDONE)
      IF (ISDONE) THEN
         ISEOF = T
         IRET = 4
         PELIM = -1
         GO TO 999
         END IF
C                                       Do transfer
      IF (DODISK) THEN
         CALL FLDKIO ('READ', FDVEC, TAPBUF, TAPIND, IRET)
      ELSE
         MSGSUP = 1000
         CALL TAPIO ('READ', FDVEC, TAPBUF, TAPIND, IRET)
         END IF
      MSGSUP = 0
      IF (IRET.EQ.10) IRET = 0
C                                       Increment error counter
      IF (IRET.EQ.3) PECNT = PECNT + 1
C                                       Trap label records (length=80)
C                                       can't happen with disk files
      IF ((FDVEC(42).LE.80) .AND. (IRET.EQ.0)) THEN
C                                       Loop until EOF or long record
 100     CONTINUE
         MSGSUP = 1000
         CALL TAPIO ('READ', FDVEC, TAPBUF, TAPIND, IRET)
         MSGSUP = 0
         IF (IRET.EQ.10) IRET = 0
C                                       Increment error counter
         IF (IRET.EQ.3) PECNT = PECNT + 1
         IF (PECNT.GT.PELIM) THEN
            IRET = 4
            GO TO 999
            END IF
C                                       If double file mark, bail out
         IF ((IRET.EQ.6) .OR. (ISEOF .AND. (IRET.EQ.4))) THEN
            ISEOT = T
            ISEOF = T
            IRET = 4
            GO TO 999
            END IF
         ISEOF = IRET.EQ.4
         IF (ISEOF) THEN
            IF (NOFILE.GE.NCOUNT) THEN
               ISEOT = T
               ISEOF = T
               IRET = 4
               GO TO 999
            ELSE
               NOFILE = NOFILE + 1
               END IF
            END IF
         IF ((FDVEC(42).LE.80) .OR. ISEOF .OR. (IRET.EQ.3)) GO TO 100
      ELSE
         IF (IRET.EQ.6) THEN
            ISEOT = .TRUE.
            IRET = 4
            END IF
C                                       Trap double file mark
         ISEOT = ISEOT .OR. (ISEOF .AND. IRET.EQ.4)
         ISEOF = IRET.EQ.4
         END IF
      IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 999
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Block numbers - MCBLK & MCNBLK
      CALL ZI16IL (2, 1, TAPBUF(TAPIND), MCBLK)
C                                       Disk needs idea of how much to
C                                       read
      IF ((DODISK) .AND. (MCBLK.EQ.1)) THEN
         CALL ZI32IL (1, 2, TAPBUF(TAPIND), LRWORD)
         LRWORD = 2 * LRWORD + 4 * MCNBLK
         LRWORD = (LRWORD-1) / 2048 + 1
         IF (LRWORD.GE.13) THEN
            FDVEC(43) = LRWORD - 13
         ELSE
            FTAB(FDVEC(40)+5) = FTAB(FDVEC(40)+5) + LRWORD - 13
            IF (MCNBLK.NE.1) THEN
               MSGTXT = 'PROBLEM IN CORRECTING RECORD POSITION ON DISK'
               CALL MSGWRT (9)
               END IF
            END IF
         END IF
C                                       Done
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCREAD: Error ',I4,' reading VLA archive tape')
      END
      SUBROUTINE MCFLIP (NCHAN, NPOLN, STORD, RPARM, VIS, VIS2)
C-----------------------------------------------------------------------
C   Reverses a baseline.
C   Inputs:
C      NCHAN    I        Number of frequency channels
C      NPOLN    I        Number of polarization channels
C                        (RR, LL, RL, LR)
C      STORD    I        Order of stokes axis, 1=> before freq
C   Input/Output
C      RPARM    R(*)     Random parameters
C      VIS      R(3,NPOLN,NCHAN) Visibility data (real, imag., weight)
C      VIS2     R(3,NCHAN,NPOLN) Visibility data (real, imag., weight)
C
C Note that holography needs the phase reversal, and suitable reordering
C of the antenna numbers, but not a reversal of u&v. It does mean that a
C further phase inversion will be required at some later stage since we
C really want the antenna ordering to be :  (target ant) * (ref ant).
C However, we cannot do it here in FILLM as we will run into confusion
C troubles when we come to calibrate.
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NPOLN, STORD
      REAL      RPARM(6), VIS(3,NPOLN,NCHAN), VIS2(3,NCHAN,NPOLN)
C
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'MC2.INC'
C
      INTEGER   IFREQ, IA1, IA2
      REAL      TEMPR, TEMPI, TEMPW
C-----------------------------------------------------------------------
C                                       Switch antennas
      IA1 = RPARM(1+ILOCB) / 256 + 0.1
      IA2 = RPARM(1+ILOCB) - (IA1 * 256) + 0.1
      RPARM(1+ILOCB) = IA1 + (256 * IA2)
C                                       Flip u,v,w
      IF (.NOT. MCHOLO) THEN
        RPARM(1+ILOCU) = - RPARM(1+ILOCU)
        RPARM(1+ILOCV) = - RPARM(1+ILOCV)
        RPARM(1+ILOCW) = - RPARM(1+ILOCW)
        END IF
C                                       Check data order
C                                       Polarization before frequency
      IF (STORD.NE.1) GO TO 500
C                                       Conjugate data.
C                                       Branch by number of
C                                       polarizations
      IF (NPOLN.EQ.1) THEN
C                                       1 polarization
         DO 100 IFREQ = 1,NCHAN
            VIS(2,1,IFREQ) = -VIS(2,1,IFREQ)
 100        CONTINUE
         END IF
      IF (NPOLN.EQ.2) THEN
C                                       2 polarizations
         DO 200 IFREQ = 1,NCHAN
            VIS(2,1,IFREQ) = -VIS(2,1,IFREQ)
            VIS(2,2,IFREQ) = -VIS(2,2,IFREQ)
 200        CONTINUE
         END IF
      IF (NPOLN.EQ.4) THEN
C                                       4 polarizations
         DO 300 IFREQ = 1,NCHAN
            VIS(2,1,IFREQ) = -VIS(2,1,IFREQ)
            VIS(2,2,IFREQ) = -VIS(2,2,IFREQ)
C                                       RL, LR pol. (switch)
            TEMPR = VIS(1,3,IFREQ)
            TEMPI = VIS(2,3,IFREQ)
            TEMPW = VIS(3,3,IFREQ)
            VIS(1,3,IFREQ) = VIS(1,4,IFREQ)
            VIS(2,3,IFREQ) = -VIS(2,4,IFREQ)
            VIS(3,3,IFREQ) = VIS(3,4,IFREQ)
            VIS(1,4,IFREQ) = TEMPR
            VIS(2,4,IFREQ) = -TEMPI
            VIS(3,4,IFREQ) = TEMPW
 300        CONTINUE
         END IF
      GO TO 999
C                                       Frequency before polarization
C                                       Conjugate data.
C                                       Branch by number of
C                                       polarizations
 500  IF (NPOLN.EQ.1) THEN
C                                       1 polarization
         DO 600 IFREQ = 1,NCHAN
            VIS2(2,IFREQ,1) = -VIS2(2,IFREQ,1)
 600        CONTINUE
         END IF
      IF (NPOLN.EQ.2) THEN
C                                       2 polarizations
         DO 700 IFREQ = 1,NCHAN
            VIS2(2,IFREQ,1) = -VIS2(2,IFREQ,1)
            VIS2(2,IFREQ,2) = -VIS2(2,IFREQ,2)
 700        CONTINUE
         END IF
      IF (NPOLN.EQ.4) THEN
C                                       4 polarizations
         DO 800 IFREQ = 1,NCHAN
            VIS2(2,IFREQ,1) = -VIS2(2,IFREQ,1)
            VIS2(2,IFREQ,2) = -VIS2(2,IFREQ,2)
C                                       RL, LR pol. (switch)
            TEMPR = VIS2(1,IFREQ,3)
            TEMPI = VIS2(2,IFREQ,3)
            TEMPW = VIS2(3,IFREQ,3)
            VIS2(1,IFREQ,3) = VIS2(1,IFREQ,4)
            VIS2(2,IFREQ,3) = -VIS2(2,IFREQ,4)
            VIS2(3,IFREQ,3) = VIS2(3,IFREQ,4)
            VIS2(1,IFREQ,4) = TEMPR
            VIS2(2,IFREQ,4) = -TEMPI
            VIS2(3,IFREQ,4) = TEMPW
 800        CONTINUE
         END IF
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MCHEAD (DOINIT, NEXTR, IRET)
C-----------------------------------------------------------------------
C   Reads first block of integration and extracts the header
C   information from an integration and converts into local form.
C   If this integration is wanted, if not, then skips until a
C   desired record is found.  Does most of the initialization for
C   interpreting the data.
C   On the first call default selection parameters are filled in.
C   Inputs:
C      DOINIT   L        True if called during an initialization
C                        operation
C   Inputs from common:
C      TAPBUF   I(*)     Tape buffer
C      FDVEC    I(50)    Tape file descriptor block
C      STRTAB   L        If true allow writing AIPS tables
C   Input/output in common:
C      MCINIT   L        If true then this is the first call, initialize
C      ISEOF    L        If true an EOF was encountered on last read
C   Outputs to common:
C      TAPIND   I        Tape buffer pointer
C      MCBLK    I        Current tape block number
C      MCNBLK   I        Number of tape blocks per logical record
C      MCLRL    I        Logical Record length (words)
C      MCFMT    I        Format type (only 1 wanted)
C      MCFREV   I        Format revision level
C      MCDATE   I        Date (MJAD)
C      MCIATC   I        IAT (interupt count since midnight)
C      MCCPID   C*8      Control program ID
C      MCSDA    I        Pointer to subarray data area (in tape buffer,
C                        in 16 bit words)
C      MCADA    I        Pointer to first antenna data area
C      MCLADA   I        Length (words) of each antenna data area
C      MCNANT   I        Number of antennas in this subarray
C      MCCDA    I(4)     Pointer to correlator data area, 1 per CDA
C      MCLCDA   I(2,4)   Length of CDA header (MC words), baseline entry
C      MCNBPR   I        Number of 2KB blocks per integration
C      MCSAID   I        Subarray ID
C      MCSNAM   C*16     Source name
C      MCQUAL   I        Source qualifier
C      MCARRC   C*2      Array Configuration (1 char?)
C      MCPGID   C*6      Observing program ID
C      MCUSID   I        User ID number
C      MCOMOD   C*2      Observing mode
C      MCALCD   C*2      Calibrator code
C      MCRCV    C*2      Observing band code
C      MCNCPB   I        Number of correlators per baseline (?)
C      MCINTG   I        Integration time in waveguide cycles
C      MCLSTE   R        LST stop time (radians)
C      MCLSTB   R        LST start time (radians)
C      MCRAEP   D        Right Ascension at standard epoch (radians)
C      MCDCEP   D        Declination at standard epoch (radians)
C      MCRAAP   D        Apparent Right Ascension (radians)
C      MCDCAP   D        Apparent Declination (radians)
C      MCSSLO   D(4)     Signed sum of LOs IFs A-D (GHz)
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCIATI   D        IAT at middle of integration (radians)
C      MCLSTI   D        LST at end of integration (radians)
C      MCIATG   D        IAT of geometry computations
C      MCSR     R        Current surface rafractivity (n-1)
C      MCZAPP   R        Estimated zenith atmos. phase path (nsec)
C      MCSCOS   R(6)     Sin and cos of h, A and eta
C      MCBCOD   I        Bandwidth code
C      MCFEFC   I        Front end control codes
C      MCRCC    I        Recirculator control codes
C      MCZSF    R        Zero spacing flux (IF?)
C      MCUVLM   R(2)     UV limits for online ANTSOL (nsec?)
C      MCACB    I        Array control bits
C      MCWEAT   R(5)     Weather info (???)
C      MCRVEL   D(4)     Radial velocity for IFs A-D (Km/sec)
C      MCREST   D(4)     Line rest Frequency for IFs A-D (MHz)
C      MCVRF    C*8      Velocity rest frame
C                           1) 'G'=geocentric, 'T'=topocentric,
C                              'B'=barycentric, 'L'=LSR
C                           2) 'V'=radio, 'Z'=optical, 'O'=offset
C      MCCORM   C*4      Correlator mode (? char)
C      MCAPOP   C*4      AP options (4 char ?)
C      MCEPOC   I        Epoch year
C      MCCHOF   I(4)     Channel offsets
C      MCANCB   I(28)    Antenna control bits
C      MCANNS   R(4,28)  Antenna nominal sensitivity (unitless)
C      MCANTF   R(4,28)  Front-end derived system temperature
C      MCANTB   R(4,28)  Back-end derived system temperature
C      MCANPD   R(4,28)  Antenna peculiar delay 1/IF (nsec)
C      MCANPP   I(4,28)  Antenna peculiar phase (turns) 1/IF
C      MCIFCB   I(4,28)  Antenna control bits 1/IF
C      MCANTD   D(28)    Antenna total delay at"geometry" epoch (nsec)
C      MCAUVW   R(3,28)  Antenna u,v, and w at center of integration
C                        for specified Epoch (nsec)
C      MCAXYZ   D(3,28)  Antenna x, y, and z (nsec)
C      MCANBA   R(28)    Antenna Ba (k-term)
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      NEWFIL   L        If true then the data in the following record
C                        need to go into a different set of tables
C      MCINIT   L        Set true if NEWFIL
C   Output:
C      NEXTR    L        If true then current average finished
C      IRET     I        Return code, 0=>OK, 3=>Parity error,
C                        4=>EOF, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      LOGICAL   DOINIT, NEXTR
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      LOGICAL   WANTED, DONE, NEWSCN, T, F, WASEOF
      INTEGER   NCONV, INDXX, LOOP, LIM, IERR, SMSGS, ITIME(4)
      CHARACTER ODATE*8, TIMSGN
      SAVE WASEOF
      DATA T, F, WASEOF /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Check previous EOF
      IF (ISEOF) THEN
         IRET = 4
         ISEOF = F
         GO TO 999
         END IF
C                                       Read header record if necessary.
 100  IF (.NOT.GOTHED) THEN
         CALL MCREAD (IRET)
C                                       Too many errors?
         IF (PECNT.GT.PELIM) THEN
            IRET = 4
            GO TO 999
            END IF
C                                       If EOF and data accumulated
C                                       save EOF condition.
         IF (ISEOF.AND.(MCLNF1.GT.0)) THEN
            IRET = 0
            NEXTR = T
            GO TO 999
            END IF
         IF (.NOT. WASEOF) WASEOF = IRET.EQ.4
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Better be block 1
      IF (MCBLK.NE.1) THEN
C                                       Skip to first block of next
C                                       record.
         LIM = MCNBLK - MCBLK
         GO TO 800
         END IF
      GOTHED = T
C                                       Get info to see if data wanted
C                                       logical record length
      NCONV = 1
      INDXX = 2
      CALL ZI32IL (NCONV, INDXX, TAPBUF(TAPIND), MCLRL)
C                                       Format type, revision
      NCONV = 2
      INDXX = 5
      CALL ZI16IL (NCONV, INDXX, TAPBUF(TAPIND), MCFMT)
C                                       Only do format 1
      LIM = MCNBLK - 1
      IF (MCFMT.NE.1) GO TO 800
C                                       Decide if integration wanted.
      CALL MCWANT (DOINIT, WANTED, NEXTR, DONE, NEWSCN)
C                                       Finish tables if done or the
C                                       last integration of a scan.
      IF ((DONE.OR.NEWSCN) .AND. STRTAB) THEN
         SMSGS = MSGSUP
         MSGSUP = 32000
         CALL FLMTAB (F, T, T, T, T, T, IERR)
         MSGSUP = SMSGS
C                                       End of scan time
         EOSTIM = MCIATI
         END IF
C                                       Treat done like EOF
      IF (DONE) THEN
         ISEOF = T
         NEXTR = T
         IF (MCLNF1.LE.0) IRET = 4
         GO TO 999
         END IF
C                                       print out progress if data after
C                                       EOF (for high-density archive)
      IF (WASEOF) THEN
         CALL GREG (TSDATE + 2400000.5D0, ODATE)
         CALL T2DHMS (SNGL (TSIAT0/TWOPI), TIMSGN, ITIME)
         WRITE (MSGTXT, 1000) NOFILE+NFILES, ODATE, ITIME(2),
     *                        ITIME(3), ITIME(4)
         CALL MSGWRT (5)
         WASEOF = F
         END IF
      IF (.NOT.WANTED) GO TO 800
      IF (NEXTR) GO TO 999
      GOTHED = F
C                                       Call appropriate header
C                                       translation routine.
      CALL MCH1
      LOOP = INDEX (MCAPOP, 'Z')
      ZSPEC = LOOP.GT.0
C                                       Get antenna info
      CALL MCANT
C                                       Initialize file crunching info
      IF (MCINIT) THEN
         CALL MCINI (IRET)
         IF (IRET.NE.0) GO TO 999
         MCINIT = F
         END IF
C                                       OK - done with header
      GO TO 999
C                                       Skip rest of integration
 800  GOTHED = F
      DO 820 LOOP = 1,LIM
         CALL MCREAD (IRET)
         IF (IRET.NE.0) GO TO 999
 820     CONTINUE
      IF (NEWFIL) GO TO 999
      GO TO 100
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('tape file # ', I2, ', start date/time = ', A8, '/',
     *        I2.2,':',I2.2,':',I2.2)
      END
      SUBROUTINE MCH1
C-----------------------------------------------------------------------
C   Interprets the contents of the file header.  This version of
C   the routine is for the initial version of the tape format.
C   NOTE: This routine works by copying the all of the words of each
C   data type into an array and then converting all of the data to the
C   local form in a single call to the appropriate routine.  This works
C   because the output of the conversion is to a common and the order
C   of the variables in the common MUST be the same as they are filled
C   in the array in this routine.  Future modifications of, or separate
C   versions of this routine should not change the order of variables
C   in this common.
C   Outputs to common:
C      MCDATE   I        Date (MJAD)
C      MCIATC   I        IAT (interupt count since midnight)
C      MCCPID   C*8      Control program ID
C      MCSDA    I        Pointer to subarray data area (in tape buffer,
C                        in 16 bit words)
C      MCADA    I        Pointer to first antenna data area
C      MCLADA   I        Length (words) of each antenna data area
C      MCNANT   I        Number of antennas in this subarray
C      MCCDA    I(4)     Pointer to correlator data area, 1 per CDA
C      MCLCDA   I(2,4)   Length of CDA header (MC words), baseline entry
C      MCNBPR   I        Number of 2kb blocks per integration
C      MCSAID   I        Subarray ID
C      MCSNAM   C*16     Source name
C      MCQUAL   I        Source qualifier
C      MCARRC   C*2      Array Configuration (1 char?)
C      MCPGID   C*6      Observing program ID
C      MCUSID   I        User ID number
C      MCOMOD   C*2      Observing mode
C      MCALCD   C*2      Calibrator code
C      MCRCV    C*2      Observing band code
C      MCNCPB   I        Number of correlators per baseline. Power of 2
C                        per nibble, 1 per (A,B,C,D)
C      MCINTG   I        Integration time in waveguide cycles
C      MCLSTE   R        LST stop time (radians)
C      MCLSTB   R        LST start time (radians)
C      MCRAEP   D        Right Ascension at standard epoch (radians)
C      MCDCEP   D        Declination at standard epoch (radians)
C      MCRAAP   D        Apparent Right Ascension (radians)
C      MCDCAP   D        Apparent Declination (radians)
C      MCSSLO   D(4)     Signed sum of LOs IFs A-D (GHz)
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCIATI   D        IAT at middle of integration (radians)
C      MCLSTI   D        LST at end of integration (radians)
C      MCIATG   D        IAT of geometry computations.
C      MCSR     R        Current surface rafractivity (n-1)
C      MCZAPP   R        Estimated zenith atmos. phase path (nsec)
C      MCSCOS   R(6)     Sin and cos of h, A and eta
C      MCBCOD   I        Bandwidth code
C      MCFEFC   I        Front end control codes
C      MCRCC    I        Recirculator control codes
C      MCZSF    R        Zero spacing flux (IF?)
C      MCUVLM   R(2)     UV limits for online ANTSOL (nsec?)
C      MCACB    I        Array control bits
C      MCWEAT   R(5)     Weather info (???)
C      MCRVEL   D(4)     Radial velocity for IFs A-D (Km/sec)
C      MCREST   D(4)     Line rest Frequency for IFs A-D (MHz)
C      MCVRF    C*8      Velocity rest frame  2 per IF
C                           1) 'G'=geocentric, 'T'=topocentric,
C                              'B'=barycentric, 'L'=LSR
C                           2) 'V'=radio, 'Z'=optical, 'O'=offset
C      MCCORM   C*4      Correlator mode (? char)
C      MCAPOP   C*4      AP options
C      MCEPOC   I        Epoch year
C      MCCHOF   I(4)     Channel offsets
C      MCHISP   R(4)     Channel separation codes
C
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      MCCHAN   I(4)     Line channels for IFs (A,B,C,D)
C      MCHOLO   L        Holography mode
C-----------------------------------------------------------------------
      INCLUDE 'MC2.INC'
      INCLUDE 'MCB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      INTEGER   NCONV, INDXX, I2BUFF(200), I2CNT, I2PNT, I4CNT, I4PNT,
     *          R4CNT, R4PNT, R8PNT, R8CNT, NMOV, MCSUBA, MCRCA, IBC,
     *          BITS(32)
      INTEGER   I4BUFF(200)
      REAL      R4BUFF(200)
      DOUBLE PRECISION R8BUFF(200)
      EQUIVALENCE (TBUFF, I2BUFF),      (TBUFF(201), I4BUFF),
     *            (TBUFF(401), R4BUFF), (TBUFF(601), R8BUFF)
C
C-----------------------------------------------------------------------
C                                       Copy different data types to
C                                       buffers and then convert all at
C                                       once. Do characters at once.
      I2CNT = 0
      I4CNT = 0
      R4CNT = 0
      R8CNT = 0
      I2PNT = 1
      I4PNT = 1
      R4PNT = 1
      R8PNT = 1
      MCRCA = 5
C                                       MCDATE
      NMOV = 4
      INDXX = MCRCA + 2 * 4
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
C                                       MCIATC
      NMOV = 4
      INDXX = MCRCA + 2 * 6
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
C                                       Control  program ID (MCCPID)
      NCONV = 8
      INDXX = MCRCA + 2 * 8
      MCCPID = '        '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCCPID)
      CALL CHLTOU (NCONV, MCCPID)
C                                       SDA, ADA pointers
      NMOV = 8
      INDXX = MCRCA + 2 * 12
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 2
C                                       Get SDA pointer
      NCONV = 1
      INDXX = 8
      CALL ZI32IL (NCONV, INDXX, TAPBUF(TAPIND), MCSDA)
C                                       Length of ADA, no. ant in sub.
      NMOV = 4
      INDXX =  MCRCA + 2 * 16
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 2
C                                       CDA pointers, sizes
C                                       CDA 1
      NMOV = 4
      INDXX = MCRCA + 2 * 18
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
      INDXX = MCRCA + 2 * 20
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 2
C                                       CDA 2
      NMOV = 4
      INDXX = MCRCA + 2 * 22
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
      INDXX = MCRCA + 2 * 24
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 2
C                                       CDA 3
      NMOV = 4
      INDXX = MCRCA + 2 * 26
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
      INDXX = MCRCA + 2 * 28
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 2
C                                       CDA 4
      NMOV = 4
      INDXX = MCRCA + 2 * 30
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
C                                       Also MCNBPR
      INDXX = MCRCA + 2 * 32
      NMOV = 6
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 3
C                                       Subarray information:
      MCSUBA = 5 + MCSDA * 2
C                                       Subrarray number
      NMOV = 2
      INDXX =  MCSUBA
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 1
C                                       Source name
      NCONV = 16
      INDXX =  MCSUBA + 2 * 1
      MCSNAM = '                '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCSNAM)
      CALL CHLTOU (NCONV, MCSNAM)
C                                       Qualifier
      NMOV = 2
      INDXX =  MCSUBA + 2 * 9
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 1
C                                       Array configuration
      NCONV = 2
      INDXX =  MCSUBA + 2 * 10
      MCARRC = '  '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCARRC)
      CALL CHLTOU (NCONV, MCARRC)
C                                       Observing program
      NCONV = 6
      INDXX =  MCSUBA + 2 * 11
      MCPGID = '      '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCPGID)
      CALL CHLTOU (NCONV, MCPGID)
C                                       Observer's id number
      NMOV = 2
      INDXX =  MCSUBA + 2 * 14
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 1
C                                       Observing mode and calcode
      NCONV = 2
      INDXX =  MCSUBA + 2 * 15
      MCOMOD = '  '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCOMOD)
      CALL CHLTOU (NCONV, MCOMOD)
      NCONV = 2
      INDXX =  MCSUBA + 2 * 16
      MCALCD = '  '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCALCD)
      CALL CHLTOU (NCONV, MCALCD)
C                                       receiver band selected
      MCRCV = '  '
      IF (MCFREV.GE.33) THEN
         NCONV = 2
         INDXX =  MCSUBA + 2 * 17
         CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCRCV)
         CALL CHLTOU (NCONV, MCRCV)
         END IF
      MCRCVX = MCRCV
C                                       MCNCPB, MCINTG
      NMOV = 4
      INDXX =  MCSUBA + 2 * 18
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 2
C                                       Scan start, stop times
      NMOV = 8
      INDXX =  MCSUBA + 2 * 20
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R4PNT, R4BUFF)
      R4PNT = R4PNT + NMOV
      R4CNT = R4CNT + 2
C                                       Positions, Freqs, times
      NMOV = 15 * 8
      INDXX =  MCSUBA + 2 * 24
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R8PNT, R8BUFF)
      R8PNT = R8PNT + NMOV
      R8CNT = R8CNT + 15
C                                       MCSR, MCZAPP, MCSCOS
      NMOV = 8 * 4
      INDXX =  MCSUBA + 2 * 84
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R4PNT, R4BUFF)
      R4PNT = R4PNT + NMOV
      R4CNT = R4CNT + 8
C                                       Bandwidth etc. codes
      NMOV = 6
      INDXX =  MCSUBA + 2 * 100
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 3
C                                       Zero spacing flux, UVLIM
      NMOV = 12
      INDXX =  MCSUBA + 2 * 103
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R4PNT, R4BUFF)
      R4PNT = R4PNT + NMOV
      R4CNT = R4CNT + 3
C                                       Array control bits
      NMOV = 4
      INDXX =  MCSUBA + 2 * 109
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I4PNT, I4BUFF)
      I4PNT = I4PNT + NMOV
      I4CNT = I4CNT + 1
C                                       Weather
      NMOV = 20
      INDXX =  MCSUBA + 2 * 111
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R4PNT, R4BUFF)
      R4PNT = R4PNT + NMOV
      R4CNT = R4CNT + 5
C                                       Velocity, rest freq
      NMOV = 8 * 8
      INDXX =  MCSUBA + 2 * 121
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), R8PNT, R8BUFF)
      R8PNT = R8PNT + NMOV
      R8CNT = R8CNT + 8
C                                       Vel. def
      NCONV = 8
      INDXX =  MCSUBA + 2 * 153
      MCVRF = '        '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCVRF)
      CALL CHLTOU (NCONV, MCVRF)
C                                       MCCORM
      NCONV = 4
      INDXX =  MCSUBA + 2 * 157
      MCCORM = '    '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCCORM)
      CALL CHLTOU (NCONV, MCCORM)
C                                       MCAPOP
      NCONV = 4
      INDXX =  MCSUBA + 2 * 159
      MCAPOP = '    '
      CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MCAPOP)
      CALL CHLTOU (NCONV, MCAPOP)
C                                       Epoch, MCCHOF, ch. sep. code
      NMOV = 18
      INDXX =  MCSUBA + 2 * 161
      CALL ZBYMOV (NMOV, INDXX, TAPBUF(TAPIND), I2PNT, I2BUFF)
      I2PNT = I2PNT + NMOV
      I2CNT = I2CNT + 9
C                                       Convert short integers
      CALL ZI16IL (I2CNT, 1, I2BUFF, MCLADA)
C                                       Convert long integers
      CALL ZI32IL (I4CNT, 1, I4BUFF, MCDATE)
C                                       Convert short reals
      NCONV = R4CNT * 2
      CALL ZBYTFL (NCONV, R4BUFF, R4BUFF)
      CALL ZRM2RL (R4CNT, R4BUFF, MCLSTE)
C                                       Convert long reals
      NCONV = R8CNT * 4
      CALL ZBYTFL (NCONV, R8BUFF, R8BUFF)
      CALL ZDM2DL (R8CNT, R8BUFF, MCRAEP)
C                                       For revisions < 20 (1988) MCIATC
C                                       denoted the end of integration;
C                                       in more recent versions MCIATI
C                                       is used. In FILLM, only variable
C                                       MCIATI is used, so MCIATI=MCIATC
C                                       when the data are older than '88
C                                       GvM, Nov95.
      IF (MCFREV.LT.20) MCIATI = TWOPI * MCIATC/19.2D0/86400.0D0
C                                       adjust to middle of integration
      TCORR = TWOPI * MCINTG / 19.20D0 / 86400.0D0 / 2.0D0
      MCIATI = MCIATI - TCORR
C                                       Convert zeros in MCCDA into
C                                       pointers to next entry
      IF (MCCDA(4).LE.0) MCCDA(4) = MCLRL
      IF (MCCDA(3).LE.0) MCCDA(3) = MCCDA(4)
      IF (MCCDA(2).LE.0) MCCDA(2) = MCCDA(3)
      IF (MCCDA(1).LE.0) MCCDA(1) = MCCDA(2)
C                                       Bandwith codes are 0-9.  The
C                                       recirculator bandwidths are
C                                       50/2**bwc MHz, except for bwc=9,
C                                       which is the same as bwc=8.
C                                       Corrected EWG 30 Oct 2005
      CALL ZGTBIT (32, MCBCOD, BITS)
      IBC = 8*BITS(16) + 4*BITS(15) + 2*BITS(14) + BITS(13)
      IF (IBC.EQ.9) IBC = 8
      MCBANW(1) = 50.0E6 / 2**IBC
      IBC = 8*BITS(12) + 4*BITS(11) + 2*BITS(10) + BITS(9)
      IF (IBC.EQ.9) IBC = 8
      MCBANW(2) = 50.0E6 / 2**IBC
      IBC = 8*BITS(8) + 4*BITS(7) + 2*BITS(6) + BITS(5)
      IF (IBC.EQ.9) IBC = 8
      MCBANW(3) = 50.0E6 / 2**IBC
      IBC = 8*BITS(4) + 4*BITS(3) + 2*BITS(2) + BITS(1)
      IF (IBC .EQ.9) IBC = 8
      MCBANW(4) = 50.0E6 / 2**IBC
C                                       for revisions < 20, the
C                                       front-end filter codes weren't
C                                       written to tape...
      IF (MCFREV.GE.20) THEN
C                                       front-end filter widths
         CALL ZGTBIT (32, MCFEFC, BITS)
         IBC = 8*BITS(16) + 4*BITS(15) + 2*BITS(14) + BITS(13)
         IF (IBC .LE. 2) THEN
            MCFEFW(1) = 50.0E6 / 2**IBC
            MCBANW(1) = MIN (MCBANW(1), MCFEFW(1))
         END IF
         IBC = 8*BITS(12) + 4*BITS(11) + 2*BITS(10) + BITS(9)
         IF (IBC .LE. 2) THEN
            MCFEFW(2) = 50.0E6 / 2**IBC
            MCBANW(2) = MIN (MCBANW(2), MCFEFW(2))
         END IF
         IBC = 8*BITS(8) + 4*BITS(7) + 2*BITS(6) + BITS(5)
         IF (IBC .LE. 2) THEN
            MCFEFW(3) = 50.0E6 / 2**IBC
            MCBANW(3) = MIN (MCBANW(3), MCFEFW(3))
         END IF
         IBC = 8*BITS(4) + 4*BITS(3) + 2*BITS(2) + BITS(1)
         IF (IBC .LE. 2) THEN
            MCFEFW(4) = 50.0E6 / 2**IBC
            MCBANW(4) = MIN (MCBANW(4), MCFEFW(4))
         END IF
      ELSE
         MCFEFW(1) = 0.0
         MCFEFW(2) = 0.0
         MCFEFW(3) = 0.0
         MCFEFW(4) = 0.0
      END IF
C                                       channel separations
      MCHSEP(1) = 50.0E6 / 2**MCHISP(1)
      MCHSEP(2) = 50.0E6 / 2**MCHISP(2)
      MCHSEP(3) = 50.0E6 / 2**MCHISP(3)
      MCHSEP(4) = 50.0E6 / 2**MCHISP(4)
C                                       correlators per baseline
      CALL ZGTBIT (32, MCNCPB, BITS)
      IBC = 8*BITS(16) + 4*BITS(15) + 2*BITS(14) + BITS(13)
      MCCHAN(1) = 2**IBC
      IBC = 8*BITS(12) + 4*BITS(11) + 2*BITS(10) + BITS(9)
      MCCHAN(2) = 2**IBC
      IBC = 8*BITS(8) + 4*BITS(7) + 2*BITS(6) + BITS(5)
      MCCHAN(3) = 2**IBC
      IBC = 8*BITS(4) + 4*BITS(3) + 2*BITS(2) + BITS(1)
      MCCHAN(4) = 2**IBC
C                                       Holography stuff
      MCHOLO = MCOMOD(1:1).EQ.'H'
      IF (MCHOLO) THEN
C                                       Encode Az. & El. position
C                                       into qualifier.
         MCQUAL = 40000 + 256 * MCQUAL + ICHAR(MCALCD(2:2))
         END IF
C                                       Single-dish or interferometer
C                                       pointing
      MCPTNG = MCOMOD(1:1).EQ.'P'.OR.MCOMOD(1:1).EQ.'I'
C                                       TIP mode
      MCTIP = MCOMOD(1:1).EQ.'T'
C                                       Ancient tape stuff:
C                                       If integration time is reported
C                                       as 0, set it to 192 (=10sec)
      IF (MCINTG.EQ.0) MCINTG = 192
C
 999  RETURN
      END
      SUBROUTINE MCANT
C-----------------------------------------------------------------------
C   Extracts antenna info from the Modcomp tape buffer; corrects for
C   center shift when using Pie Town at Q, finds station IDs.
C   Inputs from common:
C      TAPBUF   I(*)     Tape buffer
C      TAPIND   I        Tape buffer pointer
C      MCADA    I        Pointer to first antenna data area
C      MCLADA   I        Length (words) of each antenna data area
C      MCNANT   I        Number of antennas in this subarray
C      IFSLIM   I        Maximum allowed IF status
C    Output in common:
C      MCANCB   I(4,28)  Antenna control bits
C      MCANNS   R(4,28)  Antenna nominal sensitivity (unitless)
C      MCANTF   R(4,28)  Front-end derived system temperature
C      MCANTB   R(4,28)  Back-end derived system temperature
C      MCANPD   R(4,28)  Antenna peculiar delay 1/IF (nsec)
C      MCANPP   I(4,28)  Antenna peculiar phase (turns) 1/IF
C      MCIFCB   I(4,28)  Antenna control bits 1/IF
C      MCANTD   D(28)    Antenna total delay at"geometry" epoch (nsec)
C      MCAUVW   R(3,28)  Antenna u,v, and w at center of integration
C                        for specified Epoch (nsec)
C      MCAXYZ   D(3,28)  Antenna x, y, and z (nsec)
C      MCANBA   R(28)    Antenna Ba (k-term)
C      IFFLAG   L(4,28)  If true IF is flagged (A,B,C,D)
C      MCAORD   I(28)    List of antennas in order
C      MCANTS   L(28)    If true, antenna in subarray
C-----------------------------------------------------------------------
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      INTEGER   IANT, INDXX, IPOINT, I2BUFF(10), I2CONV(10), ANTNO,
     *   NCONV, ISTAT, JSTAT, KSTAT, SGNMSK, IPNT, IPNT2, ZAND,
     *   ANNTST(4,MXANT), ANNTFT(4,MXANT), ANNTBT(4,MXANT), ITST,
     *   TIT(3), BIT(32), I, J, K, IA1, IA2, NOSTA, NOUT, NODD, NGOOD,
     *   MAXSTN(3)
      REAL      TTIME, TITSEC, R4BUFF(10), PHI(4), PHJ(4,MXANT), AMP(8),
     *   ALPHA, PHA(MXANT)
      DOUBLE PRECISION R8BUFF(10), FACTOR, ANOFF(3), LTOLER
      CHARACTER TSIGN*1, ARC(3)*1
      LOGICAL   MOVED, DIDMSG
      EQUIVALENCE (MCANNS(1,1), ANNTST(1,1))
      EQUIVALENCE (MCANTF(1,1), ANNTFT(1,1))
      EQUIVALENCE (MCANTB(1,1), ANNTBT(1,1))
      EQUIVALENCE (I2BUFF, R4BUFF, R8BUFF)
      DATA SGNMSK /32767/
      DATA ANOFF /-1.5D4, -4.5D4, 2.2D4/
      DATA DIDMSG /.FALSE./
C-----------------------------------------------------------------------
      FACTOR = 2.0D0 * PI / 32768.0D0
      ALPHA  = 256.0
C                                       Clear antenna info
      DO 10 IANT = 1,MXANT
         IFFLAG(1,IANT) = .TRUE.
         IFFLAG(2,IANT) = .TRUE.
         IFFLAG(3,IANT) = .TRUE.
         IFFLAG(4,IANT) = .TRUE.
         MCAORD(IANT) = 0
         MCANTS(IANT) = .FALSE.
         MCANCB(IANT) = 0
         MCANNS(1,IANT) = 0.0
         MCANNS(2,IANT) = 0.0
         MCANNS(3,IANT) = 0.0
         MCANNS(4,IANT) = 0.0
         MCANTF(1,IANT) = 0.0
         MCANTF(2,IANT) = 0.0
         MCANTF(3,IANT) = 0.0
         MCANTF(4,IANT) = 0.0
         MCANTB(1,IANT) = 0.0
         MCANTB(2,IANT) = 0.0
         MCANTB(3,IANT) = 0.0
         MCANTB(4,IANT) = 0.0
         MCANPD(1,IANT) = 0.0
         MCANPD(2,IANT) = 0.0
         MCANPD(3,IANT) = 0.0
         MCANPD(4,IANT) = 0.0
         MCANPP(1,IANT) = 0
         MCANPP(2,IANT) = 0
         MCANPP(3,IANT) = 0
         MCANPP(4,IANT) = 0
         MCANTD(IANT) = 0.0D0
         MCAUVW(1,IANT) = 0.0
         MCAUVW(2,IANT) = 0.0
         MCAUVW(3,IANT) = 0.0
         MCAXYZ(1,IANT) = 0.0D0
         MCAXYZ(2,IANT) = 0.0D0
         MCAXYZ(3,IANT) = 0.0D0
         MCANBA(IANT) = 0.0
         MCIFCB(1,IANT) = 2 ** 13
         MCIFCB(2,IANT) = 2 ** 13
         MCIFCB(3,IANT) = 2 ** 13
         MCIFCB(4,IANT) = 2 ** 13
 10      CONTINUE
C                                       Loop through antennas
      DO 100 IANT = 1,MCNANT
C                                       Byte pointer
         IPOINT = MCADA * 2 + (IANT-1) * MCLADA * 2 + 5
C                                       Word (16bit) pointer
         IPNT = MCADA + (IANT-1) * MCLADA + 3
C                                       Double word (32bit) pointer
         IPNT2 = ((IPNT-3) / 2) + 2
C                                       Antenna and status
         INDXX = IPNT
         NCONV = 1
         CALL ZI16IL (NCONV, INDXX, TAPBUF(TAPIND), I2CONV)
         ANTNO = I2CONV(1) / 256
         IF ((ANTNO.LT.1) .OR. (ANTNO.GT.MXANT)) GO TO 990
C                                       Antenna order
         MCAORD(IANT) = ANTNO
         IF (OLDORD(IANT).NE.ANTNO) MCINIT = .TRUE.
         OLDORD(IANT) = ANTNO
         MCANTS(ANTNO) = .TRUE.
C                                       IF flags
         INDXX = IPNT + 3
         NCONV = 1
         CALL ZI16IL (NCONV, INDXX, TAPBUF(TAPIND), I2CONV)
         ISTAT = ZAND (I2CONV(1), SGNMSK)
C                                       A IF
         JSTAT = ISTAT / 4096
         KSTAT = JSTAT
         IF (I2CONV(1).LT.0) KSTAT = KSTAT + 8
         IFFLAG(1,ANTNO) = KSTAT.GT.ABS(IFSLIM)
         MCSTAT(ANTNO) = 4096 * KSTAT
C                                       B IF
         ISTAT = ISTAT - JSTAT * 4096
         JSTAT = ISTAT / 256
         IFFLAG(2,ANTNO) = JSTAT.GT.ABS(IFSLIM)
         MCSTAT(ANTNO) = MCSTAT(ANTNO) + 256 * JSTAT
C                                       C IF
         ISTAT = ISTAT - JSTAT * 256
         JSTAT = ISTAT / 16
         IFFLAG(3,ANTNO) = JSTAT.GT.ABS(IFSLIM)
         MCSTAT(ANTNO) = MCSTAT(ANTNO) + 16 * JSTAT
C                                       D IF
         ISTAT = ISTAT - JSTAT * 16
         IFFLAG(4,ANTNO) = ISTAT.GT.ABS(IFSLIM)
         MCSTAT(ANTNO) = MCSTAT(ANTNO) + ISTAT
C                                       Flag all IFs?
         IF ((IFSLIM.LT.0) .AND. (IFFLAG(1,ANTNO) .OR. IFFLAG(2,ANTNO)
     *                  .OR. IFFLAG(3,ANTNO) .OR. IFFLAG(4,ANTNO))) THEN
            IFFLAG(1,ANTNO) = .TRUE.
            IFFLAG(2,ANTNO) = .TRUE.
            IFFLAG(3,ANTNO) = .TRUE.
            IFFLAG(4,ANTNO) = .TRUE.
            END IF
C                                       Antenna control bits
         INDXX = (IPNT + 1) * 2 - 1
         NCONV = 4
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 1
         CALL ZI32IL (NCONV, 1, I2BUFF, MCANCB(ANTNO))
         CALL ZGTBIT (32, MCANCB(ANTNO), BIT)
C                                       Nominal sensitivity
         INDXX = (IPNT + 4) * 2 - 1
         NCONV = 16
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 8
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 4
         CALL ZRM2RL (NCONV, R4BUFF, MCANNS(1,ANTNO))
C                                       Check for Nan's always
         DO 20 ITST = 1, 4
            IF (ANNTST(ITST,ANTNO).EQ.-1) THEN
               MCANNS(ITST,ANTNO) = FBLANK
               BADNMS = BADNMS + 1
               BADNOM(ITST,ANTNO) = BADNOM(ITST,ANTNO) + 1
               IF (BADNMS.LE.10) THEN
                  TTIME = SNGL(MCIATG) / TWOPI
                  CALL TFDHMS (TTIME, 1, TSIGN, TIT, TITSEC)
                  WRITE (MSGTXT,1000) ANTNO, ITST, TIT, TITSEC
                  IF (MSGTXT(53:53).EQ.' ') MSGTXT(53:53) = '0'
                  IF (MSGTXT(54:54).EQ.' ') MSGTXT(54:54) = '0'
                  CALL MSGWRT (6)
                  END IF
            ELSE
               MCANNS(ITST,ANTNO) = ABS (MCANNS(ITST,ANTNO))
               END IF
 20         CONTINUE
C                                       Peculiar delay
         INDXX = (IPNT + 12) * 2 - 1
         NCONV = 16
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 8
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 4
         CALL ZRM2RL (NCONV, R4BUFF, MCANPD(1,ANTNO))
C                                       Peculiar phase
         INDXX = IPNT + 20
         NCONV = 4
         CALL ZI16IL (NCONV, INDXX, TAPBUF(TAPIND), MCANPP(1,ANTNO))
C                                       Van Vleck correction
C                                       PHI in order A,B,C,D
C                                       just like MCANNS
         DO 30 I = 1, 4
            PHI(I) = MCANPP(I,ANTNO) * FACTOR
 30         CONTINUE
C                                       PHJ in order A,B,C,D
         PHJ(1,ANTNO) = 0.0
         PHJ(2,ANTNO) = PHI(4) - PHI(2)
         PHJ(3,ANTNO) = PHI(1) - PHI(3)
         PHJ(4,ANTNO) = 0.0
C                                       MCANNS and PHJ describe complex
C                                       gain used to get the flux from
C                                       RHAT.  In MCSUM, this may be
C                                       used for Van Vleck correction
C                                       CGN = ALPHA * SQRT (gain_i *
C                                              conjugate of gain_j)
C         IF (ANTNO.EQ.4.OR.ANTNO.EQ.14) THEN
C            WRITE(MSGTXT,1545) ANTNO, (MCANNS(I,ANTNO), I = 1, 4)
C 1545       FORMAT(I3,4(1X,F6.3))
C            CALL MSGWRT(4)
C            WRITE(MSGTXT,1546) ANTNO, (MCANPP(I,ANTNO), I = 1, 4)
C 1546       FORMAT(I3,4(1X,I6))
C            CALL MSGWRT(4)
C            WRITE(MSGTXT,1547) ANTNO, (PHI(I), I = 1, 4)
C 1547       FORMAT(I3,4(1X,F6.3))
C            CALL MSGWRT(4)
C            WRITE(MSGTXT,1548) ANTNO, (PHJ(I,ANTNO), I = 1, 4)
C 1548       FORMAT(I3,4(1X,F6.3))
C            CALL MSGWRT(4)
C            END IF
C                                       End Van Vleck part
C                                       Total delay
         INDXX = (IPNT + 24) * 2 - 1
         NCONV = 8
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 4
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 1
         CALL ZDM2DL (NCONV, R8BUFF, MCANTD(ANTNO))
C                                       Check for delay out of range
         IF ((MCANTD(ANTNO).LT.0.0D0) .OR.
     *       (MCANTD(ANTNO).GT.1.0D6)) THEN
            MCANTD(ANTNO) = 0.0D0
            IFFLAG(1,ANTNO) = .TRUE.
            IFFLAG(2,ANTNO) = .TRUE.
            IFFLAG(3,ANTNO) = .TRUE.
            IFFLAG(4,ANTNO) = .TRUE.
            TTIME = MCIATG / TWOPI
            CALL TFDHMS (TTIME, 1, TSIGN, TIT, TITSEC)
            WRITE (MSGTXT,1010) ANTNO, TIT, TITSEC
            IF (MSGTXT(52:52).EQ.' ') MSGTXT(52:52) = '0'
            IF (MSGTXT(53:53).EQ.' ') MSGTXT(53:53) = '0'
            CALL MSGWRT (6)
            END IF
C                                       u,v,w
         INDXX = (IPNT + 28) * 2 - 1
         NCONV = 12
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 6
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 3
         CALL ZRM2RL (NCONV, R4BUFF, MCAUVW(1,ANTNO))
C                                       Kludge for Holography
         IF (MCHOLO) THEN
            CALL ZGTBIT (32, MCANCB(ANTNO), BIT)
            IF (BIT(31).EQ.0) THEN
               MCAUVW(1,ANTNO) = MCAUVW(1,ANTNO) * 1.0
               MCAUVW(2,ANTNO) = MCAUVW(2,ANTNO) * 1.0
C                                 leave as is (correct holography u,v).
C                 MCAUVW(1,ANTNO) = MCAUVW(1,ANTNO) * 360.0E0
C                 MCAUVW(2,ANTNO) = MCAUVW(2,ANTNO) * 360.0E0
               END IF
C                                       Save holo qualifier
            MCAUVW(3,ANTNO) = MCQUAL
            END IF
C                                       Bx, By, Bz
         INDXX = (IPNT + 34) * 2 - 1
         NCONV = 24
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 12
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 3
         CALL ZDM2DL (NCONV, R8BUFF, MCAXYZ(1,ANTNO))
C                                       Ba
         INDXX = (IPNT + 46) * 2 - 1
         NCONV = 4
         CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
         NCONV = 2
         CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
         NCONV = 1
         CALL ZRM2RL (NCONV, R4BUFF, MCANBA(ANTNO))
C                                       Tsys only for revisions >= 25
         IF (MCFREV.GE.25) THEN
C                                       Front-end system temperature
            INDXX = (IPNT + 48) * 2 - 1
            NCONV = 16
            CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
            NCONV = 8
            CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
            NCONV = 4
            CALL ZRM2RL (NCONV, R4BUFF, MCANTF(1,ANTNO))
C                                       Check for Nan's.
            DO 40 ITST = 1,4
               IF (ANNTFT(ITST,ANTNO).EQ.-1) MCANTF(ITST,ANTNO) = 0.0
 40            CONTINUE
C                                       Back-end system temperature
            INDXX = (IPNT + 56) * 2 - 1
            NCONV = 16
            CALL ZBYMOV (NCONV, INDXX, TAPBUF(TAPIND), 1, I2BUFF)
            NCONV = 8
            CALL ZBYTFL (NCONV, I2BUFF, I2BUFF)
            NCONV = 4
            CALL ZRM2RL (NCONV, R4BUFF, MCANTB(1,ANTNO))
C                                       Check for Nan's.
            DO 50 ITST = 1,4
               IF (ANNTBT(ITST,ANTNO).EQ.-1) MCANTB(ITST,ANTNO) = 0.0
 50            CONTINUE
C                                       if control bits
            INDXX = IPNT + 64
            NCONV = 4
            CALL ZI16IL (NCONV, INDXX, TAPBUF(TAPIND), MCIFCB(1,ANTNO))
C                                       end IF (MCFREV.GE.25)
            END IF
C                                       So much for the ADA
 100     CONTINUE
C
C                                       Now fix antenna location
C                                       find station IDs
C     Band test fails since MCINI sets STFREQ and not called yet
C      DTEMP = STFREQ(1) * 1.0D9
C      CALL FLMBC (DTEMP, BBAND)
C      I = MXANT
C      MOVED = (BBAND.EQ.'Q') .AND. (MCAXYZ(1,I).NE.0.0D0) .AND.
C     *   (MCAXYZ(2,I).NE.0.0D0) .AND. (MCAXYZ(3,I).NE.0.0D0)
      MOVED = .FALSE.
 110  NOUT = 0
      NODD = 0
      NGOOD = 0
      CALL FILL (3, 0, MAXSTN)
      DO 120 I = 1,MXANT
C                                       offset used with Pie Town Q band
C                                       since March 11, 2002
         IF (MOVED) THEN
            IF ((MCAXYZ(1,I).NE.0.0D0) .OR. (MCAXYZ(2,I).NE.0.0D0) .OR.
     *         (MCAXYZ(3,I).NE.0.0D0)) THEN
               MCAXYZ(1,I) = (MCAXYZ(1,I) + ANOFF(1))
               MCAXYZ(2,I) = (MCAXYZ(2,I) + ANOFF(2))
               MCAXYZ(3,I) = (MCAXYZ(3,I) + ANOFF(3))
               END IF
            END IF
C                                       Get pad name
         NOSTA = I
         CALL VLANAM (MCAXYZ(1,I), NOSTA, MCANTN(I), MCANCB(I), MAXSTN)
         EVLA(I) = MCANTN(I)(:5).EQ.'EVLA'
         IF (MCANTN(I)(6:8).EQ.'OUT') THEN
            NOUT = NOUT + 1
            IF ((MCAXYZ(1,I).NE.0.0D0) .OR. (MCAXYZ(2,I).NE.0.0D0) .OR.
     *         (MCAXYZ(3,I).NE.0.0D0)) NODD = NODD + 1
         ELSE
            NGOOD = NGOOD + 1
            END IF
 120     CONTINUE
C                                       When Q is being used some of
C                                       the time then may be at other
C                                       bands too
      IF ((NODD.GT.0) .AND. (.NOT.MOVED)) THEN
         I = MXANT
         MOVED = (MCAXYZ(1,I).NE.0.0D0) .AND. (MCAXYZ(2,I).NE.0.0D0)
     *      .AND. (MCAXYZ(3,I).NE.0.0D0) .AND. (NGOOD.EQ.0)
         IF (MOVED) GO TO 110
         IF ((NGOOD.GT.0) .AND. (.NOT.DIDMSG)) THEN
            MSGTXT = '**********************************************'
            CALL MSGWRT (8)
            MSGTXT = 'SOME NON-0 STATIONS FOUND, SOME NOT - VERY ODD'
            CALL MSGWRT (8)
            MSGTXT = '**********************************************'
            CALL MSGWRT (8)
            DIDMSG = .TRUE.
            END IF
         END IF
C                                       figure out the array
      DO 125 I = 1,3
         IF (MAXSTN(I).LE.9) THEN
            ARC(I) = 'D'
         ELSE IF (MAXSTN(I).LE.14) THEN
            ARC(I) = 'C'
         ELSE IF (MAXSTN(I).LE.18) THEN
            ARC(I) = 'B'
         ELSE IF (MAXSTN(I).LE.24) THEN
            ARC(I) = 'A'
            END IF
 125     CONTINUE
      MCARRC = ARC(1)
      IF (ARC(2).NE.ARC(1)) THEN
         MCARRC = '?'
      ELSE IF (ARC(3).NE.ARC(1)) THEN
         MCARRC(2:2) = ARC(3)
         END IF

C                                       check antenna position for move
      MOVED = .FALSE.
      LTOLER = TOLERB
      DO 130 I = 1,MXANT
         IF ((STRXYZ(1,I).NE.0.0D0) .OR. (STRXYZ(2,I).NE.0.0) .OR.
     *      (STRXYZ(3,I).NE.0.0D0)) THEN
            IF (I.EQ.MXANT) LTOLER = 2 * TOLERB
            IF ((ABS(STRXYZ(1,I)-MCAXYZ(1,I)).GT.LTOLER) .OR.
     *         (ABS(STRXYZ(2,I)-MCAXYZ(2,I)).GT.LTOLER) .OR.
     *         (ABS(STRXYZ(3,I)-MCAXYZ(3,I)).GT.LTOLER)) THEN
               IF ((MCAXYZ(1,I).NE.0.0D0) .OR. (MCAXYZ(1,I).NE.0.0D0)
     *            .OR. (MCAXYZ(1,I).NE.0.0D0)) MOVED = .TRUE.
               END IF
         ELSE
            STRXYZ(1,I) = MCAXYZ(1,I)
            STRXYZ(2,I) = MCAXYZ(2,I)
            STRXYZ(3,I) = MCAXYZ(3,I)
            END IF
 130     CONTINUE
      IF (MOVED) THEN
         MSGTXT = 'ANTENNAS APPEAR TO HAVE MOVED'
         CALL MSGWRT (5)
C                                       force it to close down, decide
C                                       where the new antennas go
         MCINIT = .TRUE.
         NEWFIL = .TRUE.
         BAIL = .TRUE.
         END IF
C                                       Corr coeff or scaled?
      CALL CHKORR (MCANTN, MCIFCB, MCANTS, ISCORC)
C                                       Van Vleck stuff
      IF (VLECK) THEN
         DO 800 I = 1, MCNANT
            IA1 = MCAORD(I)
            DO 700 J = 1, MCNANT
               IA2 = MCAORD(J)
C                                       order: AA,CC,AC,CA,BB,DD,BD,DB
C                                       amplitudes
               AMP(1) = ALPHA * SQRT (MCANNS(1,IA1) * MCANNS(1,IA2))
               AMP(2) = ALPHA * SQRT (MCANNS(3,IA1) * MCANNS(3,IA2))
               AMP(3) = ALPHA * SQRT (MCANNS(1,IA1) * MCANNS(3,IA2))
               AMP(4) = ALPHA * SQRT (MCANNS(3,IA1) * MCANNS(1,IA2))
               AMP(5) = ALPHA * SQRT (MCANNS(2,IA1) * MCANNS(2,IA2))
               AMP(6) = ALPHA * SQRT (MCANNS(4,IA1) * MCANNS(4,IA2))
               AMP(7) = ALPHA * SQRT (MCANNS(2,IA1) * MCANNS(4,IA2))
               AMP(8) = ALPHA * SQRT (MCANNS(4,IA1) * MCANNS(2,IA2))
C                                       phases
               PHA(1) = (PHJ(1,IA1) - PHJ(1,IA2)) / 2.0
               PHA(2) = (PHJ(3,IA1) - PHJ(3,IA2)) / 2.0
               PHA(3) = (PHJ(1,IA1) - PHJ(3,IA2)) / 2.0
               PHA(4) = (PHJ(3,IA1) - PHJ(1,IA2)) / 2.0
               PHA(5) = (PHJ(2,IA1) - PHJ(2,IA2)) / 2.0
               PHA(6) = (PHJ(4,IA1) - PHJ(4,IA2)) / 2.0
               PHA(7) = (PHJ(2,IA1) - PHJ(4,IA2)) / 2.0
               PHA(8) = (PHJ(4,IA1) - PHJ(2,IA2)) / 2.0
C                                       CGN is complex conversion factor
C                                       between flux and correl. coeff.
C                                       fill in real and imaginary parts
C                                       of CGN for 8 correlations and
C                                       all baselines.  To be used by
C                                       MCVLK to get to the correlation
C                                       coefficient and back.
               DO 600 K = 1, 8
                  CGN(K,1,IA1,IA2) = AMP(K) * COS (PHA(K))
                  CGN(K,2,IA1,IA2) = AMP(K) * SIN (PHA(K))
 600              CONTINUE
C
C               IF (IA1.EQ.1.AND.IA2.EQ.2) THEN
C                  WRITE(MSGTXT,1560) IA1,IA2,(AMP(K), K = 1, 8)
C                  CALL MSGWRT(4)
C                  END IF
C               IF (IA1.EQ.15.AND.IA2.EQ.24) THEN
C                  WRITE(MSGTXT,1560) IA1,IA2,(AMP(K), K = 1, 8)
C                  CALL MSGWRT(4)
C                  END IF
 700           CONTINUE
 800        CONTINUE
C
C         WRITE(MSGTXT,1560) (CGN(I,1,4,14), I = 1, 8)
C         CALL MSGWRT(4)
C         WRITE(MSGTXT,1560) (CGN(I,2,4,14), I = 1, 8)
C         CALL MSGWRT(4)
C         MSGTXT = ''
C         CALL MSGWRT(4)
C 1560    FORMAT(2I3,8(F6.1,1X))
         END IF
C                                       End Van Vleck part
      GO TO 999
C                                       Error decoding record.
 990  WRITE (MSGTXT,1600)
      CALL MSGWRT (6)
C                                       Done
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCANT: Bad nom. sens. for ant ',I2,' IF ',I2,' at',
     *   I4,'/',2(I2.2,':'),F4.1)
 1010 FORMAT ('MCANT: Bad total delay for antenna ',I2,' at',
     *   I4,'/',2(I2.2,':'),F4.1)
 1600 FORMAT ('MCANT: Error decoding antenna information.')
      END
      SUBROUTINE CHKORR (MCANTN, MCIFCB, MCANTS, ISCORC)
C-----------------------------------------------------------------------
C   Compare the IF control block setting on whether data are scaled
C   and set ISCORC
C   Inputs:
C      MCANTN   C*8(28)   Antenna names (ignore OUT)
C      MCIFCB   I(4,28)   IF control block
C      MCANTS   L(28)     In the current subarray?
C   Output
C      ISCORC   L         Data are correlation coefficients
C-----------------------------------------------------------------------
      CHARACTER MCANTN(*)*8
      INTEGER   MCIFCB(4,*)
      LOGICAL   MCANTS(*), ISCORC
C
      INTEGER   I, J, BITS(32), L, IE, LE
      LOGICAL   DONE
      INCLUDE 'INCS:DMSG.INC'
      SAVE DONE
      DATA DONE /.FALSE./
C-----------------------------------------------------------------------
      L = -1
      IE = 0
      LE = 0
      DO 100 I = 1,28
         IF ((MCANTS(I)) .AND. (MCANTN(I)(6:8).NE.'OUT')) THEN
            DO 90 J = 1,4
               CALL ZGTBIT (32, MCIFCB(J,I), BITS)
               IF (L.EQ.-1) L = BITS(14)
               IF (BITS(14).NE.L) THEN
                  IE = IE + 1
                  WRITE (MSGTXT,1000) I, J
                  IF (.NOT.DONE) CALL MSGWRT (7)
                  END IF
               LE = LE + 1
 90            CONTINUE
            END IF
 100     CONTINUE
      ISCORC = L.EQ.0
      IF ((IE.GT.0) .AND. (.NOT.DONE)) THEN
         MSGTXT = '*************************************************'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1100) IE, LE
         CALL MSGWRT (8)
         MSGTXT = '*************************************************'
         CALL MSGWRT (8)
         DONE = .TRUE.
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKORR: ANT',I3,' CORR',I2,' DISAGREES WITH FIRST')
 1100 FORMAT ('CHKORR:',I3,' CORR OF',I4,' DISAGREE ON SCALING OF DATA')
      END
      SUBROUTINE MCWANT (DOINIT, WANTED, NEXTR, DONE, NEWSCN)
C-----------------------------------------------------------------------
C   Determines if the current FORMAT 1 record meets the selection
C   criteria; and, if it does, should it be included in the current
C   accumulation.  Uses /MODCOM/ heavily.
C   Inputs:
C      DOINIT   L        True if initializing rather than reading
C   Inputs from common:
C      SELECT   R(11)    Selection parameters.
C                         1 => source qualifier, -1 => all
C                         5 => Averaging time (rad.)
C                         7 => subarray, 0 => any
C                         8 => No. channels wanted
C                         9 => Start time (rad)
C                        10 => End time (rad)
C                        11 => Cal averaging time (rad.)
C      SELBAN   C*1      Band code 1st IF (blank => any)
C      SELBA2   C*1      Band code 2nd IF (blank => any)
C      SELPGM   C*6      Observing program name
C      SELMOD   C*2      Observing mode (blank => any)
C      FRESEL   D(2,4)   (1,n), (2,n) = Selected freq, tolerance for
C                        IF n; 1=A, 2=B, 3=C, 4=D
C      STIAT    D        Start IAT of integration. (=-1.0E10 until valid
C                        data are found)
C      REFMJD   D        Reference MJD (mod. Julian date).
C      DAYOFF   I        Offset of current day from reference day (days)
C      MCSKYF   D(4)     Sky frequencies of current scan
C      MCAORD   I(28)    List of antennas in order of current scan
C      MCLNF1   I        Number of data records accumulated
C   Output to common:
C      BANDAC   C*2      Band code for AC continuum data (e.g. 'C')
C      BANDBD   C*2      Band code for BD continuum data
C      NEWFIL   L        If true then the data in the following record
C                        need to go into a different set of tables
C      OLDOFF   I        Previous value of DAYOFF
C      OLDORD   I(28)    Previous values of MCAORD
C   Output:
C      WANTED   L        If true, the data meets the selection criteria
C      NEXTR    L        If false, the current record goes into the
C                        current accumulation
C      DONE     L        If true, all desired data has been read
C      NEWSCN   L        If true, the next record is in a new scan
C-----------------------------------------------------------------------
      LOGICAL   DOINIT, WANTED, NEXTR, DONE, NEWSCN
C
      CHARACTER TSPGID*6, TSOMOD*2, TCALCD*2, TSCORM*4, TSBNAC*1,
     *   TSBNBD*1, TCOMOD*2, MBND*2, SBND*2
      INTEGER   IQUAL, INCH, I, TSSAID, TSNCPB, TSQUAL, IPOINT, IPNTB,
     *   TSSDA, TSNANT, TSOFF, INTB(2), OUTB(2), MCRCA, INDXX, LTIME,
     *   CTIME, ITD, ITH, ITT(4), WMSG, NCONV
      LOGICAL   T, F, FIRST, EQUAL, MIDNIT, OKSUB, DEFSUB
      REAL      TSLSTB, AVGTIM, XTIME
      DOUBLE PRECISION    OLDSKF(4), TSSKYF(4), TSIAT, T8BUFF(20), TDIFF
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FILLM.INC'
      INCLUDE 'MCB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (TBUFF, T8BUFF)
      SAVE FIRST, LTIME, OLDSKF, WMSG
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
      DATA LTIME /-100/
      DATA WMSG /0/
C-----------------------------------------------------------------------
C                                       Initialize results
      WANTED = F
      NEXTR  = F
      DONE   = F
      NEWFIL = F
      NEWSCN = F
      MCRCA  = 5
C                                       Set pointer to SDA
      CALL ZI32IL (1, 8, TAPBUF(TAPIND), TSSDA)
      IPOINT = TSSDA + 3
      IPNTB = TSSDA * 2 + 5
C                                       Get date
      CALL ZI32IL (1, 4, TAPBUF(TAPIND), TSDATE)
C                                       Set TSOFF from reference date
      IF (REFMJD.GT.0.0D0) THEN
         TSOFF = TSDATE - IDNINT(REFMJD)
      ELSE
         TSOFF = DAYOFF
         OLDOFF = DAYOFF
         END IF
C                                       Get IAT time, depends on version
C                                       of online system software
      IF (MCFREV.GE.20) THEN
C
C For newer revisions of the online system (versions > 19), end-of-scan
C time is taken from the Subarray Data Area (SDA).
C
         CALL ZBYMOV (8, (IPOINT + 72)*2 - 1, TAPBUF(TAPIND), 1, TBUFF)
         CALL ZBYTFL (4, TBUFF, TBUFF)
         CALL ZDM2DL (1, T8BUFF, TSIAT0)
      ELSE
C
C For older revisions of the online system (versions < 20), end-of-scan
C time is taken from the Record Control Area (RCA), words 6 and 7 (hence
C the offset of 2 X 6 = 12 bytes. Using the code above for versions > 19
C led to problems whenever the integration time was not a multiple of
C 10 seconds. This fix introduces calls to the RCA from MCWANT; until
C this fix the RCA was accessed only in MCH1.                GvM, Nov95.
C
         CALL ZBYMOV (4, MCRCA+12, TAPBUF(TAPIND), 1, INTB)
         CALL ZI32IL (1, 1, INTB, OUTB)
         TSIAT0 = TWOPI * FLOAT(OUTB(1))/19.2/86400.0
         ENDIF
C
      TSIAT = TSIAT0 + TSOFF * TWOPI
C                                       Manage day jump
      MIDNIT = F
      IF (TSOFF.GT.OLDOFF) THEN
         MIDNIT = T
         OLDOFF = TSOFF
         END IF
C                                       Current subarray
      CALL ZI16IL (1, IPOINT, TAPBUF(TAPIND), TSSAID)
C                                       Check timerange
      CTIME = TSIAT * 24.0 / TWOPI
      IF ((TSIAT.GT.SELECT(10)+TCORR) .AND. (STIAT.GT.-1.0E-5))
     *   GO TO 990
      IF ((TSIAT.LT.SELECT(9)) .OR. (TSIAT.GT.SELECT(10)+TCORR))
     *   GO TO 980
C                                       Check program name.
      TSPGID = '      '
      CALL ZC8CL (6, IPNTB + 2*11, TAPBUF(TAPIND), TSPGID)
      CALL CHLTOU (6, TSPGID)
C                                       Observing mode pointing?
      INDXX =  IPNTB + 2 * 15
      CALL ZC8CL (2, INDXX, TAPBUF(TAPIND), TCOMOD)
      CALL CHLTOU (2, TCOMOD)
      IF ((TCOMOD(1:1).EQ.'P') .OR. (TCOMOD(1:1).EQ.'I')) THEN
         IF ((WMSG.EQ.0) .OR. (WMSG.EQ.2)) THEN
            XTIME = TSIAT / TWOPI
            CALL TODHMS (XTIME, ITT)
            IF (DOPTNG) THEN
               WRITE (MSGTXT,1010) ITT
            ELSE
               WRITE (MSGTXT,1011) ITT
               END IF
            CALL MSGWRT (7)
            WMSG = WMSG + 1
            END IF
         IF (.NOT.DOPTNG) GO TO 999
      ELSE IF (TCOMOD(1:1).EQ.'T') THEN
         IF ((WMSG.EQ.0) .OR. (WMSG.EQ.1)) THEN
            XTIME = TSIAT / TWOPI
            CALL TODHMS (XTIME, ITT)
            IF (DOTIP) THEN
               WRITE (MSGTXT,1012) ITT
            ELSE
               WRITE (MSGTXT,1013) ITT
               END IF
            CALL MSGWRT (7)
            WMSG = WMSG + 2
            END IF
         IF (.NOT.DOTIP) GO TO 999
      ELSE
         WMSG = 0
         END IF
C                                       Specified subarray
      DEFSUB = NINT(SELECT(7)) .LE. 0
      IF ((VLAOBS.EQ.' ') .AND. DEFSUB) THEN
         SELECT(7) = 1.0
         MSGTXT = '*** caution: CPARM(6) is 0 and VLAOBS is blank ***'
         CALL MSGWRT (2)
         MSGTXT = '** subarray defaults to 1. It is recommended   ***'
         CALL MSGWRT (2)
         MSGTXT = '*** to run FILLM with non-blank VLAOBS.        ***'
         CALL MSGWRT (2)
         END IF
      OKSUB = NINT (SELECT(7)) .EQ. TSSAID
C                                       compare to current subarray
C
C Summary of program names (PID). There are three: VLAOBS, SELPGM,
C and TSPGID. VLAOBS is given by the user; it either selects a PID
C explicitly, or is blank, in which case ALL PID's are filled. SELPGM
C is the PID currently being loaded (SELPGM=VLAOBS when VLAOBS is
C nonblank, otherwise it is the current PID).  TSPGID is the current
C PID on tape, whether being loaded or not.
C
C                                       Stop only when online, already
C                                       received data, nonblank VLAOBS,
C                                       and correct SUBARRAY.
C
C                                       For given VLAOBS and CPARM(6)=0
C                                       SELECT(7) is set to that of the
C                                       first occurrence of VLAOBS.
C
      IF (VLAOBS.EQ.TSPGID) THEN
         IF (OKSUB.OR.DEFSUB) THEN
            GOTCHA = .TRUE.
            SELECT(7) = TSSAID
            IF (NOWARN) THEN
               MSGTXT = ' '
               CALL MSGWRT (2)
               WRITE (MSGTXT,1000) TSSAID
               CALL MSGWRT (2)
               MSGTXT = ' '
               CALL MSGWRT (2)
               NOWARN = .FALSE.
               ENDIF
            ENDIF
      ELSE
C         DOSTOP = GOTCHA .AND. ONLINE .AND. (NWRITN.GT.0) .AND.
C     *      (VLAOBS.NE.' ') .AND. OKSUB
         ENDIF
C                                       correct VLAOBS?
C                                       If DOQUIT issued while LAST
C                                       scan, things hung up. So if
C                                       DOQUIT given, let it, and NOT
C                                       VLAOBS, do the job. GvM, 9/93.
      IF ((VLAOBS.NE.' ') .AND. (TSPGID.NE.VLAOBS) .AND. (.NOT.DOQUIT))
     *   GO TO 980
      IF ((SELPGM.NE.TSPGID) .AND. (SELPGM.NE.' ')) THEN
         BAIL = .TRUE.
         MSGTXT = 'PROGRAM ID HAS CHANGED FROM ''' // SELPGM //
     *      ''' TO ''' // TSPGID // ''''
         CALL MSGWRT (5)
         END IF
      SELPGM = TSPGID
C                                       Check subarray
      IF (SELECT(7).NE.TSSAID) GO TO 999
C                                       Get frequencies
      CALL ZBYMOV (32, (IPOINT + 56)*2 - 1, TAPBUF(TAPIND), 1, TBUFF)
      CALL ZBYTFL (16, TBUFF, TBUFF)
      CALL ZDM2DL (4, T8BUFF, TSSKYF)
C                                       receiver band selected
      MBND = '  '
      IF (MCFREV.GE.33) THEN
         NCONV = 2
         INDXX =  IPNTB + 2 * 17
         CALL ZC8CL (NCONV, INDXX, TAPBUF(TAPIND), MBND)
         CALL CHLTOU (NCONV, MBND)
         END IF
      SBND = MCRCVX
      MCRCVX = MBND
C                                       Get bands
      CALL FLMBC (TSSKYF(1)*1.0D9, TSBNAC)
      CALL FLMBC (TSSKYF(2)*1.0D9, TSBNBD)
      MCRCVX = SBND
C                                       Get observing mode
      TSOMOD = '  '
      CALL ZC8CL (2, IPNTB + 2*15, TAPBUF(TAPIND), TSOMOD)
      CALL CHLTOU (2, TSOMOD)
C                                       Get number of channels
      CALL ZI16IL (1, IPOINT + 18, TAPBUF(TAPIND), TSNCPB)
C                                       Get qualifier
      CALL ZI16IL (1, IPOINT + 9, TAPBUF(TAPIND), TSQUAL)
C                                       Get calibrator code
      TCALCD = '  '
      CALL ZC8CL  (2, IPNTB + 2*16, TAPBUF(TAPIND), TCALCD)
      CALL CHLTOU (2, TCALCD)
C                                       Check mode
      IF (SELMOD.NE.' ') THEN
         EQUAL = TSOMOD.EQ.SELMOD
C                                       Solar mode?
         IF (SOLAR) EQUAL = EQUAL .OR. (TSOMOD.EQ.'  ')
         IF (.NOT. EQUAL) GO TO 999
         END IF
C                                       Check no. channels
      INCH = NINT(SELECT(8))
      IF ((INCH.NE.0).AND. (TSNCPB.NE.INCH)) GO TO 999
C                                       Check qualifier
      IQUAL = NINT(SELECT(1))
      IF ((IQUAL.NE.-1) .AND. (IQUAL.NE.TSQUAL)) GO TO 999
C                                       Check Calcode ?
      IF (SELCOD(1:2).NE.'  ') THEN
C                                       Calibrators only?
         IF ((SELCOD(1:1).EQ.'*' .AND.TCALCD(1:1).EQ.' ') .OR.
C                                       Non Calibrators only?
     *       (SELCOD(1:2).EQ.'-C'.AND.TCALCD(1:1).NE.' ') .OR.
C                                       Only One Calibrator code?
     *       (SELCOD(1:2).NE.'-C'.AND.SELCOD(1:1).NE.'*'.AND.
     *        TCALCD(1:1).NE.SELCOD(1:1))) THEN
            GO TO 999
            END IF
C                                       End if calcode not blank
         END IF
C                                       Frequency test
      IF ((DABS (TSSKYF(1)-FRESEL(1,1)).GT.FRESEL(2,1)) .OR.
     *    (DABS (TSSKYF(2)-FRESEL(1,2)).GT.FRESEL(2,2)) .OR.
     *    (DABS (TSSKYF(3)-FRESEL(1,3)).GT.FRESEL(2,3)) .OR.
     *    (DABS (TSSKYF(4)-FRESEL(1,4)).GT.FRESEL(2,4))) GO TO 980
C                                       Check bands
      IF ((SELBAN.NE.' ') .AND. (SELBAN.NE.TSBNAC)) GO TO 980
      IF ((SELBA2.NE.' ') .AND. (SELBA2.NE.TSBNBD)) GO TO 980
C                                       Passed all tests
      WANTED = T
C                                       Now get some more scan info.
C                                       Number of antennas
      CALL ZI16IL (1, 20, TAPBUF(TAPIND), TSNANT)
C                                       Get correlator mode
      TSCORM = '    '
      CALL ZC8CL (4, IPNTB + 2*157, TAPBUF(TAPIND), TSCORM)
      CALL CHLTOU (4, TSCORM)
C                                       Get start LST
      CALL ZBYMOV (4, IPNTB + 2*22, TAPBUF(TAPIND), 1, TBUFF)
      CALL ZBYTFL (2, TBUFF, TBUFF)
      CALL ZRM2RL (1, TBUFF, TSLSTB)
C                                       First time initialization
      IF (FIRST) THEN
         BANDAC = TSBNAC
         BANDBD = TSBNBD
         CALL COPY (MXANT, MCAORD, OLDORD)
         CALL DPCOPY (4, TSSKYF, OLDSKF)
         MCNANT = TSNANT
         MCNCPB = TSNCPB
         MCCORM = TSCORM
         MCLSTB = TSLSTB
         MCALCD = TCALCD
         MCINIT = T
         FIRST = F
         END IF
C                                       If MCLNF1=0, then this is the
C                                       first call to MCWANT for the
C                                       integration to which this data
C                                       belongs, i.e. the second call to
C                                       MCWANT for this data record.
C                                       It
C
C                                       Not really
C
C                                       is not necessary to set all the
C                                       re-configuration flags again.
C                                       Check number of antennas and
C                                       antenna order is same
C                                       as previous integration
      IF (MCNANT.NE.TSNANT) MCINIT = T
      MCNANT = TSNANT
      IF (MCLNF1.EQ.0) THEN
         STIAT = TSIAT
         GO TO 999
         END IF
C                                       Check bands
      IF (BANDAC.NE.TSBNAC) NEWFIL = T
      IF (BANDBD.NE.TSBNBD) NEWFIL = T
      BANDAC = TSBNAC
      BANDBD = TSBNBD
      DO 600 I = 1,MXANT
         IF (OLDORD(I).NE.MCAORD(I)) THEN
            MCINIT = T
            CALL COPY (MXANT, MCAORD, OLDORD)
            END IF
 600     CONTINUE
C                                       Force a new file check if any
C                                       of the sky frequencies changes.
C                                       This is needed to route data
C                                       to the correct files when
C                                       changing between setups with
C                                       the same AC and BD frequencies
C                                       and different AC and BD
C                                       frequencies using the same
C                                       correlator mode.
      DO 610 I = 1, 4
         IF (ABS(OLDSKF(I)-TSSKYF(I)).GT.1.0E-6*TSSKYF(I)) NEWFIL =
     *      .TRUE.
C                                       No data will be emitted if
C                                       DOINIT is true so OLDSKF
C                                       should not be updated
         IF (.NOT.DOINIT) OLDSKF(I) = TSSKYF(I)
 610     CONTINUE
C                                       Check number of channels
      IF (MCNCPB.NE.TSNCPB) NEWFIL = T
      IF (HINCPB.NE.TSNCPB) NEWFIL = T
      HINCPB = TSNCPB
C                                       Check correlator mode
      IF (MCCORM.NE.TSCORM) NEWFIL = T
      IF (TSCORM.NE.HICORM) NEWFIL = T
      HICORM = TSCORM
C                                       Force NEWFIL true in four
C                                       cases, including DOQUIT;
C                                       code in MCTELL requires
C                                       NEWFIL TRUE for quitting.
      IF (DOMANY.OR.DOBREK.OR.DOQUIT.OR.DOSTOP) THEN
         IF (MIDNIT) NEWFIL = T
         IF (MCLSTB.NE.TSLSTB) NEWFIL = T
         IF ((VLAOBS.NE.' ') .AND. (TSPGID.NE.VLAOBS)) NEWFIL = T
         END IF
C                                       New file?
      IF (NEWFIL) THEN
         MCINIT = T
         NEXTR = T
         NEWSCN = T
         CALL MCTELL (DONE)
         IF ((DONE) .OR. (SELECT(10).LT.-1000.)) GO TO 990
         GO TO 999
         END IF
C                                       New scan at midnight, change in
C                                       LST, or change in SUBMODE; last
C                                       only in non-holography mode.
      SUBMCH = MCALCD(2:2).NE.TCALCD(2:2).AND..NOT.MCHOLO
      NEWSCN = MIDNIT.OR.MCLSTB.NE.TSLSTB.OR.SUBMCH
C                                       New scan means new integration
      NEXTR = NEWSCN
      IF (NEWSCN) GO TO 999
C                                       Check if time for new integrn.
C
C                                       Force solar mode if the tape
C                                       indicates that solar mode was
C                                       used provided that solar mode
C                                       was not suppressed using the
C                                       flags in CPARM(2); if solar
C                                       mode was suppressed then force
C                                       it off.  Unless solar mode is
C                                       suppressed, it should never be
C                                       turned off after being turned on.
C                                       back to non-solar mode
      SOLAR = ((SOLAR) .OR. (TSOMOD(1:1).EQ.'S')) .AND. DEFSOL
C-----------------------------------------------------------------------
C                                       EWG remark - this is not quite
C                                       right.  If DEFSOL false (option
C                                       selected in APARM), will not
C                                       stay solar though started solar
C-----------------------------------------------------------------------
C                                       Get averaging time
      IF (TCALCD(1:1).EQ.' ') THEN
         AVGTIM = SELECT(5)
      ELSE
         AVGTIM = SELECT(11)
         END IF
C                                       New integration? Compare time
C                                       difference with averaging time
      TDIFF = ABS(STIAT-TSIAT)
      NEXTR = TDIFF.GT.AVGTIM
      AVGTIM = AVGTIM * 86400.0 / TWOPI
      NEXTR = NEXTR .OR. (AVGTIM.LE.SUMINT)
C                                       skipping and hour has changed
 980  IF ((TSSAID.EQ.SELECT(7)) .OR. (SELECT(7).LE.0.0)) THEN
         IF ((.NOT.WANTED) .AND. (CTIME.NE.LTIME)) THEN
            ITD = CTIME / 24
            ITH = CTIME - 24*ITD
            WRITE (MSGTXT,1980) ITD, ITH
            CALL MSGWRT (2)
            END IF
         LTIME = CTIME
         END IF
      GO TO 999
C                                       All desired data read
 990  DONE = T
      STIAT = TSIAT
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('*** ATTENTION - default: loading subarray ',I1,' ***')
 1010 FORMAT ('Warning: including pointing data starting',I3,'/',I2.2,
     *   2(':', I2.2))
 1011 FORMAT ('Warning: omitting pointing data starting',I3,'/',I2.2,
     *   2(':', I2.2))
 1012 FORMAT ('Warning: including tipping data starting',I3,'/',I2.2,
     *   2(':', I2.2))
 1013 FORMAT ('Warning: omitting tipping data starting',I3,'/',I2.2,
     *   2(':', I2.2))
 1980 FORMAT ('Skipping data at',I3,'/',I2.2,':00:00')
      END
      SUBROUTINE MCINI (IRET)
C-----------------------------------------------------------------------
C   Sets up parameters for processing data from tape.
C   If there is data that is incompatible with other data in the file
C   then up to 8 output streams can be specified.
C   Each spectrum is assumed to have a "channel 0" as the first channel.
C   This data must be split off into a continuum data base; each CDA may
C   therefore contribute to one or two streams.
C   Inputs from common:
C      MCCORM   R        Correlator mode
C      MCCHAN   I(4)     Line channels for IFs (A,B,C,D)
C      MCFREV   I        Format revision level
C      BANDAC   C*2      Band code for AC continuum data (e.g. 'C')
C      BANDBD   C*2      Band code for BD continuum data
C      MCBANW   R(4)     Bandwidth (Hz) for IFs (A,B,C,D)
C      MCHSEP   R(4)     Channel separation (Hz) for IFs (A,B,C,D)
C      MCSKYF   D(4)     Sky Frequency at Band center or channel 0 (GHz)
C      MCNANT   I        Number of antennas in this subarray
C      MCAORD   I(28)    Order of antennas
C      TMCBCH   I        User specified BCHAN
C      TMCECH   I        User specified ECHAN
C   Outputs to common:
C      NSTREM   I        Number of output streams required for data
C      LENBAS   I        No. words per baseline, incl. all streams
C      STLEN    I(*)     No. words in vis. data in each stream
C      STPNT    I(*)     Pointer in MCDATA for next vis.  1/stream
C      DOAC     L(*)     If true keep auto correlations 1/stream
C      STFREQ   D(*)     Reference frequency for each stream
C      STMCI1   I(*)     VLA reference IF for 1st IF of stream
C                        (1,2,3,4=>A,B,C,D)
C      STMCI2   I(*)     VLA reference IF for 2nd IF of stream
C      STIFNA   C*4(*)   Names of VLA IFs included in stream
C      STNOIF   I(*)     No. IFs in each stream
C      STNOCH   I(*)     No. Channels in each stream
C      STNOPL   I(*)     No. polarizations in each stream
C      STOFIF   I(*)     0-rel index of IF axis in each stream
C      STOFFF   I(*)     0-rel index of Frequency axis in each stream
C      STOFFS   I(*)     0-rel index of Stokes parameter in each stream
C      STBCH    I(*)     1st channel to select in each stream
C      STECH    I(*)     Highest channel to select in each stream
C      STTYPE   I(*)     Type of data in stream: 0 - Continuum,
C                            1 - Line Channel 0, 2 - Line channels
C      STCDA    I(4)     Stream number for each CDA, 0=> ignore
C      STZMOD   I(*)     Number of zero-lag channels to prepend
C      DOCH0    I(4)     If true, first channel in CDA is "channel 0"
C                        STCDA points to ch 0, +3 to first line channel
C      STCOFF   I(4,2)   0-rel offset of 1st word of each CDA to
C                        baseline data area in MCDATA for each 1st
C                        correlator in baseline and for the first
C                        spectral channel selected or the second
C                        polarization for continuum
C      ANTBAS   I(500,2) Gives first and second antenna for each
C                        baseline index
C      MCMODE   I        Correlator mode code:
C                         1=cont,
C                         2=1A, 3=1B, 4=1C, 5=1D,
C                         6=2AB, 7=2AC, 8=2AD, 9=2BC, 10=2BD, 11=2CD,
C                        12=4, 13=PA, 14=PB,
C                        15=1A/C, 16=1B/C, 17=1C/C, 18=1D/C,
C                        19=2AC/C, 20=2BD/C,
C   Output:
C      IRET     I        Return code, 0=>OK, 4=>EOF, otherwise failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Parameters defining VLA IFs
      INTEGER   A, B, C, D
      PARAMETER (A=1, B=2, C=3, D=4)
C                                       Parameters for correlator modes
      INTEGER   NCOD, NCOLD
      PARAMETER (NCOD=20, NCOLD=5)
C
      CHARACTER MODCOD(NCOD)*4, OLDCOD(NCOLD)*4, NEWCOD(NCOLD)*4,
     *          CORMOD*4
      INTEGER   LOOP, ICODE, I, J, IPOINT, LIM, ISTREM,
     *          BCH(4), ECH(4), CHSEL(4)
C
      DATA MODCOD / '    ',
     *              '1A  ','1B  ','1C  ','1D  ',
     *              '2AB ','2AC ','2AD ','2BC ','2BD ','2CD ',
     *              '4   ','PA  ','PB  ',
     *              '1A/C','1B/C','1C/C','1D/C','2AC/','2BD/'/
C                                       Obsolete modes and their
C                                       correspondences
      DATA OLDCOD / '2A  ','2C  ','2D  ','4A  ','4D  '/
      DATA NEWCOD / '1A  ','1C  ','1D  ','1A  ','1D  '/
C-----------------------------------------------------------------------
C                                       Substitute obsolete correlator
C                                       modes with an acceptable
C                                       equivalent
      CORMOD = MCCORM
      DO 10 LOOP = 1,NCOLD
         IF (CORMOD.EQ.OLDCOD(LOOP)) CORMOD = NEWCOD(LOOP)
 10      CONTINUE
      IF ((CORMOD.EQ.'4   ') .AND. (MCFREV.LT.20)) CORMOD = '1A  '
C                                       Decode correlator mode
      ICODE = -1
      DO 15 LOOP = 1,NCOD
         IF (CORMOD.EQ.MODCOD(LOOP)) ICODE = LOOP
 15      CONTINUE
      IF (ICODE.LE.0) THEN
C                                       Unknown correlator mode
         WRITE (MSGTXT,1000) MCCORM
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
      ELSE
C                                       Tell user observing mode
         WRITE (MSGTXT,1100) MCCORM, MCNANT
         CALL MSGWRT (5)
         END IF
      MCMODE = ICODE
C                                       Check channel range
      DO 40 LOOP=A,D
         BCH(LOOP) = 1
         ECH(LOOP) = 1
         CHSEL(LOOP) = 1
 40      CONTINUE
      IF (CORMOD.NE.'    ') THEN
         DO 41 LOOP=A,D
            IF (MCCHAN(LOOP).LE.1) GO TO 41
C                                       Set default range
            BCH(LOOP) = TMCBCH
            ECH(LOOP) = TMCECH
            IF (BCH(LOOP).LE.1) BCH(LOOP) = 1
            IF (ECH(LOOP).LE.0) ECH(LOOP) = MCCHAN(LOOP) - 1
C                                       Check for validity
            IF (BCH(LOOP).GE.MCCHAN(LOOP)) BCH(LOOP) = MCCHAN(LOOP) - 1
            IF (ECH(LOOP).GE.MCCHAN(LOOP)) ECH(LOOP) = MCCHAN(LOOP) - 1
            IF (BCH(LOOP).GT.ECH(LOOP)) ECH(LOOP) = BCH(LOOP)
            IF (ZSPEC) THEN
               BCH(LOOP) = 0
               ECH(LOOP) = MCCHAN(LOOP) - 1
               END IF
            CHSEL(LOOP) = ECH(LOOP) - BCH(LOOP) + 1
 41         CONTINUE
         END IF
C                                       Set defaults
      NSTREM = 1
      DO 50 ISTREM = 1,MXSTRE
C                                       Autocorr OK at rev 24
         DOAC(ISTREM) = (MCFREV.GE.24) .AND. (CORMOD.NE.'    ') .AND.
     *      (DOACOR)
         STMCI1(ISTREM) = A
         STMCI2(ISTREM) = B
         STNOIF(ISTREM) = 1
         STNOPL(ISTREM) = 1
         STOFIF(ISTREM) = 3
         STOFFF(ISTREM) = 1
         STOFFS(ISTREM) = 2
         STBCH(ISTREM) = 1
         STECH(ISTREM) = 1
 50      CONTINUE
      DO 51 ISTREM = 1,MXSTRE,2
         IF (CORMOD.EQ.' ') THEN
            STTYPE(ISTREM)   = 0
            STTYPE(ISTREM+1) = 0
         ELSE
            STTYPE(ISTREM)   = 1
            STTYPE(ISTREM+1) = 2
            END IF
 51      CONTINUE
C                                       VLA IF-dependent defaults
      DO 60 LOOP=A,D
         STCDA(LOOP) = 0
         DOCH0(LOOP) = CORMOD.NE.'    '
         STCOFF(LOOP,1) = 0
         STCOFF(LOOP,2) = 3
 60      CONTINUE
C                                       Branch on correlator mode
      GO TO (100,120,140,160,180,200,220,240,260,280,300,320,340,
     *   360,380,400,420,440,460,480), ICODE
C                                       (1) Continuum
 100     STOFIF(1) = 2
         STOFIF(2) = 2
         STOFFF(1) = 3
         STOFFF(2) = 3
         STOFFS(1) = 1
         STOFFS(2) = 1
         STNOPL(1) = 4
         STNOPL(2) = 4
         STCOFF(2,1) = 12
         STCOFF(2,2) = 15
C                                       1983 and later: AC and BD IFs
         IF (MCFREV.GE.8) THEN
C                                       IFs must be the same band to
C                                       go into the same file
            IF (BANDAC .EQ. BANDBD) THEN
               NSTREM = 1
               STCDA(1) = 1
               STCDA(2) = 1
               STNOIF(1) = 2
               STIFNA(1) = 'ABCD'
               MSGTXT = 'MCINI - Continuum, 2 compatible IFs'
               CALL MSGWRT (3)
C                                       Split IFs into 2 streams
            ELSE
               NSTREM = 2
C                                       D IF labeled OK always
               STMCI1(2) = D
               STCDA(1) = 1
               STCDA(2) = 2
               STIFNA(1) = 'AC'
               STIFNA(2) = 'BD'
               MSGTXT = 'MCINI - Continuum, 2 incompatible IFs'
               CALL MSGWRT (6)
               END IF
C                                       1983 and earlier
         ELSE
            NSTREM = 1
            STCDA(1) = 1
            STIFNA(1) = 'AC'
            MSGTXT = 'MCINI - Continuum, 1 IF'
            CALL MSGWRT (3)
            END IF
         GO TO 800
C                                       (2) 1A, 2A, or 4A
 120     NSTREM = 2
         STCDA(A) = 1
         STMCI1(1) = A
         STMCI1(2) = A
         STIFNA(1) = 'A'
         STIFNA(2) = 'A'
         STBCH(2) = BCH(A)
         STECH(2) = ECH(A)
         MSGTXT = 'MCINI - Mode 1A'
         CALL MSGWRT (3)
         GO TO 800
C                                       (3) 1B
 140     NSTREM = 2
         STCDA(B) = 1
         STMCI1(1) = B
         STMCI1(2) = B
         STIFNA(1) = 'B'
         STIFNA(2) = 'B'
         STBCH(2) = BCH(B)
         STECH(2) = ECH(B)
         MSGTXT = 'MCINI - Mode 1B'
         CALL MSGWRT (3)
         GO TO 800
C                                       (4) 1C or 2C
 160     NSTREM = 2
         STCDA(C) = 1
         STMCI1(1) = C
         STMCI1(2) = C
         STIFNA(1) = 'C'
         STIFNA(2) = 'C'
         STBCH(2) = BCH(C)
         STECH(2) = ECH(C)
         MSGTXT = 'MCINI - Mode 1C'
         CALL MSGWRT (3)
         GO TO 800
C                                       (5) 1D, 2D, or 4D
 180     NSTREM = 2
         STCDA(D) = 1
         STMCI1(1) = D
         STMCI1(2) = D
         STIFNA(1) = 'D'
         STIFNA(2) = 'D'
         STBCH(2) = BCH(D)
         STECH(2) = ECH(D)
         MSGTXT = 'MCINI - Mode 1D'
         CALL MSGWRT (3)
         GO TO 800
C                                       (6) 2AB
 200     IF((BANDAC.EQ.BANDBD) .AND.
     *      (MCCHAN(A).EQ.MCCHAN(B)) .AND.
     *      (ABS(MCHSEP(A)-MCHSEP(B)).LE.0.01*MCHSEP(A))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOIF(1) = 2
            STNOIF(2) = 2
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI2(1) = B
            STMCI2(2) = B
            STIFNA(1) = 'AB'
            STIFNA(2) = 'AB'
            STCDA(A) = 1
            STCDA(B) = 1
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STCOFF(A,2) = 6
            STCOFF(B,1) = 3
            STCOFF(B,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AB compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI1(3) = B
            STMCI1(4) = B
            STIFNA(1) = 'A'
            STIFNA(2) = 'A'
            STIFNA(3) = 'B'
            STIFNA(4) = 'B'
            STCDA(A) = 1
            STCDA(B) = 3
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STBCH(4) = BCH(B)
            STECH(4) = ECH(B)
            STCOFF(B,1) = 3 + CHSEL(A) * 3
            STCOFF(B,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AB incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (7) 2AC
 220     IF((ABS(MCSKYF(A)-MCSKYF(C)).LE.1.D-6*MCSKYF(A)) .AND.
     *      (MCCHAN(A).EQ.MCCHAN(C)) .AND.
     *      (ABS(MCHSEP(A)-MCHSEP(C)).LE.0.01*MCHSEP(A))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOPL(1) = 2
            STNOPL(2) = 2
            STMCI1(1) = A
            STMCI1(2) = A
            STIFNA(1) = 'AC'
            STIFNA(2) = 'AC'
            STCDA(A) = 1
            STCDA(C) = 1
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STCOFF(A,2) = 6
            STCOFF(C,1) = 3
            STCOFF(C,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AC compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI1(3) = C
            STMCI1(4) = C
            STIFNA(1) = 'A'
            STIFNA(2) = 'A'
            STIFNA(3) = 'C'
            STIFNA(4) = 'C'
            STCDA(A) = 1
            STCDA(C) = 3
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STBCH(4) = BCH(C)
            STECH(4) = ECH(C)
            STCOFF(C,1) = 3 + CHSEL(A) * 3
            STCOFF(C,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AC incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (8) 2AD
 240     IF((BANDAC.EQ.BANDBD) .AND.
     *      (ABS(MCSKYF(A)-MCSKYF(D)).LE.1.D-6*MCSKYF(A)) .AND.
     *      (MCCHAN(A).EQ.MCCHAN(D)) .AND.
     *      (ABS(MCHSEP(A)-MCHSEP(D)).LE.0.01*MCHSEP(A))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOPL(1) = 2
            STNOPL(2) = 2
            STMCI1(1) = A
            STMCI1(2) = A
            STIFNA(1) = 'AD'
            STIFNA(2) = 'AD'
            STCDA(A) = 1
            STCDA(D) = 1
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STCOFF(A,2) = 6
            STCOFF(D,1) = 3
            STCOFF(D,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AD compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI1(3) = D
            STMCI1(4) = D
            STIFNA(1) = 'A'
            STIFNA(2) = 'A'
            STIFNA(3) = 'D'
            STIFNA(4) = 'D'
            STCDA(A) = 1
            STCDA(D) = 3
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STBCH(4) = BCH(D)
            STECH(4) = ECH(D)
            STCOFF(D,1) = 3 + CHSEL(A) * 3
            STCOFF(D,2) = 6 + CHSEL(A) * 3
            MSGTXT = 'MCINI - Mode 2AD incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (9) 2BC
 260     IF((BANDAC.EQ.BANDBD) .AND.
     *      (ABS(MCSKYF(B)-MCSKYF(C)).LE.1.D-6*MCSKYF(B)) .AND.
     *      (MCCHAN(B).EQ.MCCHAN(C)) .AND.
     *      (ABS(MCHSEP(B)-MCHSEP(C)).LE.0.01*MCHSEP(B))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOPL(1) = 2
            STNOPL(2) = 2
            STMCI1(1) = B
            STMCI1(2) = B
            STIFNA(1) = 'BC'
            STIFNA(2) = 'BC'
            STCDA(B) = 1
            STCDA(C) = 1
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STCOFF(B,2) = 6
            STCOFF(C,1) = 3
            STCOFF(C,2) = 6 + CHSEL(B) * 3
            MSGTXT = 'MCINI - Mode 2BC compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = B
            STMCI1(2) = B
            STMCI1(3) = C
            STMCI1(4) = C
            STIFNA(1) = 'B'
            STIFNA(2) = 'B'
            STIFNA(3) = 'C'
            STIFNA(4) = 'C'
            STCDA(B) = 1
            STCDA(C) = 3
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STBCH(4) = BCH(C)
            STECH(4) = ECH(C)
            STCOFF(C,1) = 3 + CHSEL(B) * 3
            STCOFF(C,2) = 6 + CHSEL(B) * 3
            MSGTXT = 'MCINI - Mode 2BC incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (10) 2BD
 280     IF((ABS(MCSKYF(B)-MCSKYF(D)).LE.1.D-6*MCSKYF(B)) .AND.
     *      (MCCHAN(B).EQ.MCCHAN(D)) .AND.
     *      (ABS(MCHSEP(B)-MCHSEP(D)).LE.0.01*MCHSEP(B))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOPL(1) = 2
            STNOPL(2) = 2
            STMCI1(1) = B
            STMCI1(2) = B
            STIFNA(1) = 'BD'
            STIFNA(2) = 'BD'
            STCDA(B) = 1
            STCDA(D) = 1
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STCOFF(B,2) = 6
            STCOFF(D,1) = 3
            STCOFF(D,2) = 6 + CHSEL(B) * 3
            MSGTXT = 'MCINI - Mode 2BD compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = B
            STMCI1(2) = B
            STMCI1(3) = D
            STMCI1(4) = D
            STIFNA(1) = 'B'
            STIFNA(2) = 'B'
            STIFNA(3) = 'D'
            STIFNA(4) = 'D'
            STCDA(B) = 1
            STCDA(D) = 3
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STBCH(4) = BCH(D)
            STECH(4) = ECH(D)
            STCOFF(D,1) = 3 + CHSEL(B) * 3
            STCOFF(D,2) = 6 + CHSEL(B) * 3
            MSGTXT = 'MCINI - Mode 2BD incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (11) 2CD
 300     IF((BANDAC.EQ.BANDBD) .AND.
     *      (MCCHAN(C).EQ.MCCHAN(D)) .AND.
     *      (ABS(MCHSEP(C)-MCHSEP(D)).LE.0.01*MCHSEP(D))) THEN
C                                       IFs are compatible
            NSTREM = 2
            STNOIF(1) = 2
            STNOIF(2) = 2
            STMCI1(1) = C
            STMCI1(2) = C
            STMCI2(1) = D
            STMCI2(2) = D
            STIFNA(1) = 'CD'
            STIFNA(2) = 'CD'
            STCDA(C) = 1
            STCDA(D) = 1
            STBCH(2) = BCH(C)
            STECH(2) = ECH(C)
            STCOFF(C,2) = 6
            STCOFF(D,1) = 3
            STCOFF(D,2) = 6 + CHSEL(C) * 3
            MSGTXT = 'MCINI - Mode 2CD compatible'
            CALL MSGWRT (3)
         ELSE
            NSTREM = 4
            STMCI1(1) = C
            STMCI1(2) = C
            STMCI1(3) = D
            STMCI1(4) = D
            STIFNA(1) = 'C'
            STIFNA(2) = 'C'
            STIFNA(3) = 'D'
            STIFNA(4) = 'D'
            STCDA(C) = 1
            STCDA(D) = 3
            STBCH(2) = BCH(C)
            STECH(2) = ECH(C)
            STBCH(4) = BCH(D)
            STECH(4) = ECH(D)
            STCOFF(D,1) = 3 + CHSEL(C) * 3
            STCOFF(D,2) = 6 + CHSEL(C) * 3
            MSGTXT = 'MCINI - Mode 2CD incompatible'
            CALL MSGWRT (6)
            END IF
         GO TO 800
C                                       (12) 4
 320     IF((ABS(MCSKYF(A)-MCSKYF(C)).LE.1.D-6*MCSKYF(A)) .AND.
     *      (MCCHAN(A).EQ.MCCHAN(C)) .AND.
     *      (ABS(MCHSEP(A)-MCHSEP(C)).LE.0.01*MCHSEP(A))) THEN
            IF((ABS(MCSKYF(B)-MCSKYF(D)).LE.1.D-6*MCSKYF(B)) .AND.
     *         (MCCHAN(B).EQ.MCCHAN(D)) .AND.
     *         (ABS(MCHSEP(B)-MCHSEP(D)).LE.0.01*MCHSEP(B))) THEN
               IF((BANDAC.EQ.BANDBD) .AND.
     *            (MCCHAN(A).EQ.MCCHAN(B)) .AND.
     *            (ABS(MCHSEP(A)-MCHSEP(B)).LE.0.01*MCHSEP(A))) THEN
C                                       All IFs compatible
                  NSTREM = 2
                  STNOIF(1) = 2
                  STNOIF(2) = 2
                  STNOPL(1) = 2
                  STNOPL(2) = 2
                  STMCI1(1) = A
                  STMCI1(2) = A
                  STMCI2(1) = B
                  STMCI2(2) = B
                  STIFNA(1) = 'ABCD'
                  STIFNA(2) = 'ABCD'
                  STCDA(A) = 1
                  STCDA(B) = 1
                  STCDA(C) = 1
                  STCDA(D) = 1
                  STBCH(2) = BCH(A)
                  STECH(2) = ECH(A)
                  STCOFF(A,2) = 12
                  STCOFF(B,1) = 6
                  STCOFF(B,2) = 12 + CHSEL(A) * 3 * 2
                  STCOFF(C,1) = 3
                  STCOFF(C,2) = 12 + CHSEL(A) * 3
                  STCOFF(D,1) = 9
                  STCOFF(D,2) = 12 + CHSEL(A) * 3 * 3
                  MSGTXT = 'MCINI - Mode 4: all compatible'
                  CALL MSGWRT (3)
               ELSE
C                                       AC & BD have different Band,
C                                       frequency, or # channels
                  NSTREM = 4
                  STNOPL(1) = 2
                  STNOPL(2) = 2
                  STNOPL(3) = 2
                  STNOPL(4) = 2
                  STMCI1(1) = A
                  STMCI1(2) = A
                  STMCI1(3) = B
                  STMCI1(4) = B
                  STIFNA(1) = 'AC'
                  STIFNA(2) = 'AC'
                  STIFNA(3) = 'BD'
                  STIFNA(4) = 'BD'
                  STCDA(A) = 1
                  STCDA(B) = 3
                  STCDA(C) = 1
                  STCDA(D) = 3
                  STBCH(2) = BCH(A)
                  STECH(2) = ECH(A)
                  STBCH(4) = BCH(B)
                  STECH(4) = ECH(B)
                  STCOFF(A,2) = 6
                  STCOFF(B,1) = 6  + CHSEL(A) * 3 * 2
                  STCOFF(B,2) = 12 + CHSEL(A) * 3 * 2
                  STCOFF(C,1) = 3
                  STCOFF(C,2) = 6  + CHSEL(A) * 3
                  STCOFF(D,1) = 9  + CHSEL(A) * 3 * 2
                  STCOFF(D,2) = 12 + CHSEL(A) * 3 * 2 + CHSEL(B) * 3
                  MSGTXT = 'MCINI - Mode 4: AC & BD compatible'
                  CALL MSGWRT (3)
                  END IF
            ELSE
C                                       A & C same, B & D different
               NSTREM = 6
               STNOPL(1) = 2
               STNOPL(2) = 2
               STMCI1(1) = A
               STMCI1(2) = A
               STMCI1(3) = B
               STMCI1(4) = B
               STMCI1(5) = D
               STMCI1(6) = D
               STIFNA(1) = 'AC'
               STIFNA(2) = 'AC'
               STIFNA(3) = 'B'
               STIFNA(4) = 'B'
               STIFNA(5) = 'D'
               STIFNA(6) = 'D'
               STCDA(A) = 1
               STCDA(B) = 3
               STCDA(C) = 1
               STCDA(D) = 5
               STBCH(2) = BCH(A)
               STECH(2) = ECH(A)
               STBCH(4) = BCH(B)
               STECH(4) = ECH(B)
               STBCH(6) = BCH(D)
               STECH(6) = ECH(D)
               STCOFF(A,2) = 6
               STCOFF(B,1) = 6  + CHSEL(A) * 3 * 2
               STCOFF(B,2) = 9  + CHSEL(A) * 3 * 2
               STCOFF(C,1) = 3
               STCOFF(C,2) = 6  + CHSEL(A) * 3
               STCOFF(D,1) = 9  + CHSEL(A) * 3 * 2 + CHSEL(B) * 3
               STCOFF(D,2) = 12 + CHSEL(A) * 3 * 2 + CHSEL(B) * 3
               MSGTXT = 'MCINI - Mode 4: AC compatible'
               CALL MSGWRT (3)
               END IF
         ELSE
            IF((ABS(MCSKYF(B)-MCSKYF(D)).LE.1.D-6*MCSKYF(B)) .AND.
     *         (MCCHAN(B).EQ.MCCHAN(D)) .AND.
     *         (ABS(MCHSEP(B)-MCHSEP(D)).LE.0.01*MCHSEP(B))) THEN
C                                       A & C different, B & D same
               NSTREM = 6
               STNOPL(5) = 2
               STNOPL(6) = 2
               STMCI1(1) = A
               STMCI1(2) = A
               STMCI1(3) = C
               STMCI1(4) = C
               STMCI1(5) = B
               STMCI1(6) = B
               STIFNA(1) = 'A'
               STIFNA(2) = 'A'
               STIFNA(3) = 'C'
               STIFNA(4) = 'C'
               STIFNA(5) = 'BD'
               STIFNA(6) = 'BD'
               STCDA(A) = 1
               STCDA(B) = 5
               STCDA(C) = 3
               STCDA(D) = 5
               STBCH(2) = BCH(A)
               STECH(2) = ECH(A)
               STBCH(4) = BCH(C)
               STECH(4) = ECH(C)
               STBCH(6) = BCH(B)
               STECH(6) = ECH(B)
               STCOFF(A,2) = 3
               STCOFF(B,1) = 6  + CHSEL(A) * 3 + CHSEL(C) * 3
               STCOFF(B,2) = 12 + CHSEL(A) * 3 + CHSEL(C) * 3
               STCOFF(C,1) = 3  + CHSEL(A) * 3
               STCOFF(C,2) = 6  + CHSEL(A) * 3
               STCOFF(D,1) = 9  + CHSEL(A) * 3 + CHSEL(C) * 3
               STCOFF(D,2) = 12 + CHSEL(A) * 3 + CHSEL(C) * 3
     *                                         + CHSEL(B) * 3
               MSGTXT = 'MCINI - Mode 4: BD compatible'
               CALL MSGWRT (3)
            ELSE
C                                       All different
               NSTREM = 8
               STMCI1(1) = A
               STMCI1(2) = A
               STMCI1(3) = B
               STMCI1(4) = B
               STMCI1(5) = C
               STMCI1(6) = C
               STMCI1(7) = D
               STMCI1(8) = D
               STIFNA(1) = 'A'
               STIFNA(2) = 'A'
               STIFNA(3) = 'B'
               STIFNA(4) = 'B'
               STIFNA(5) = 'C'
               STIFNA(6) = 'C'
               STIFNA(7) = 'D'
               STIFNA(8) = 'D'
               STCDA(A) = 1
               STCDA(B) = 3
               STCDA(C) = 5
               STCDA(D) = 7
               STBCH(2) = BCH(A)
               STECH(2) = ECH(A)
               STBCH(4) = BCH(B)
               STECH(4) = ECH(B)
               STBCH(6) = BCH(C)
               STECH(6) = ECH(C)
               STBCH(8) = BCH(D)
               STECH(8) = ECH(D)
               STCOFF(A,2) = 3
               STCOFF(B,1) = 3  + CHSEL(A) * 3
               STCOFF(B,2) = 6  + CHSEL(A) * 3
               STCOFF(C,1) = 6  + CHSEL(A) * 3 + CHSEL(B) * 3
               STCOFF(C,2) = 9  + CHSEL(A) * 3 + CHSEL(B) * 3
               STCOFF(D,1) = 9  + CHSEL(A) * 3 + CHSEL(B) * 3
     *                                         + CHSEL(C) * 3
               STCOFF(D,2) = 12 + CHSEL(A) * 3 + CHSEL(B) * 3
     *                                         + CHSEL(C) * 3
               MSGTXT = 'MCINI - Mode 4: all incompatible'
               CALL MSGWRT (6)
               END IF
            END IF
         GO TO 800
C                                       (13) PA
 340     NSTREM = 2
         STNOPL(1) = 4
         STNOPL(2) = 4
         STMCI1(1) = A
         STMCI1(2) = A
         STIFNA(1) = 'AC'
         STIFNA(2) = 'AC'
         STCDA(1) = 1
         STCDA(2) = 1
         STCDA(3) = 1
         STCDA(4) = 1
         STBCH(2) = BCH(A)
         STECH(2) = ECH(A)
         STCOFF(1,2) = 12
         STCOFF(2,1) = 3
         STCOFF(2,2) = 12 + CHSEL(A) * 3
         STCOFF(3,1) = 6
         STCOFF(3,2) = 12 + CHSEL(A) * 3 * 2
         STCOFF(4,1) = 9
         STCOFF(4,2) = 12 + CHSEL(A) * 3 * 3
         MSGTXT = 'MCINI - Mode PA'
         CALL MSGWRT (3)
         GO TO 800
C                                       (14) PB
 360     NSTREM = 2
         STNOPL(1) = 4
         STNOPL(2) = 4
         STMCI1(1) = B
         STMCI1(2) = B
         STIFNA(1) = 'BD'
         STIFNA(2) = 'BD'
         STCDA(1) = 1
         STCDA(2) = 1
         STCDA(3) = 1
         STCDA(4) = 1
         STBCH(2) = BCH(B)
         STECH(2) = ECH(B)
         STCOFF(1,2) = 12
         STCOFF(2,1) = 3
         STCOFF(2,2) = 12 + CHSEL(B) * 3
         STCOFF(3,1) = 6
         STCOFF(3,2) = 12 + CHSEL(B) * 3 * 2
         STCOFF(4,1) = 9
         STCOFF(4,2) = 12 + CHSEL(B) * 3 * 3
         MSGTXT = 'MCINI - Mode PB'
         CALL MSGWRT (3)
         GO TO 800
C                                       (15) 1A/BD
 380     NSTREM = 3
         STOFIF(3) = 2
         STOFFF(3) = 3
         STOFFS(3) = 1
         STNOPL(3) = 4
         STMCI1(1) = A
         STMCI1(2) = A
         STMCI1(3) = B
         STIFNA(1) = 'A'
         STIFNA(2) = 'A'
         STIFNA(3) = 'BD'
         STTYPE(3) = 0
         STCDA(A) = 1
         STCDA(B) = 3
         DOCH0(B) = .FALSE.
         DOAC(3) = .FALSE.
         STBCH(2) = BCH(A)
         STECH(2) = ECH(A)
         STCOFF(B,1) = 3 + CHSEL(A) * 3
         STCOFF(B,2) = 6 + CHSEL(A) * 3
         GO TO 800
C                                       (16) 1B/AC
 400     NSTREM = 3
         STOFIF(3) = 2
         STOFFF(3) = 3
         STOFFS(3) = 1
         STNOPL(3) = 4
         STMCI1(1) = B
         STMCI1(2) = B
         STMCI1(3) = A
         STIFNA(1) = 'B'
         STIFNA(2) = 'B'
         STIFNA(3) = 'AC'
         STTYPE(3) = 0
         STCDA(A) = 3
         STCDA(B) = 1
         DOCH0(A) = .FALSE.
         DOAC(3) = .FALSE.
         STBCH(2) = BCH(B)
         STECH(2) = ECH(B)
         STCOFF(A,1) = 3 + CHSEL(B) * 3
         STCOFF(A,2) = 6 + CHSEL(B) * 3
         GO TO 800
C                                       (17) 1C/BD
 420     NSTREM = 3
         STOFIF(3) = 2
         STOFFF(3) = 3
         STOFFS(3) = 1
         STNOPL(3) = 4
         STMCI1(1) = C
         STMCI1(2) = C
         STMCI1(3) = B
         STIFNA(1) = 'C'
         STIFNA(2) = 'C'
         STIFNA(3) = 'BD'
         STTYPE(3) = 0
         STCDA(B) = 3
         STCDA(C) = 1
         DOCH0(B) = .FALSE.
         DOAC(3) = .FALSE.
         STBCH(2) = BCH(C)
         STECH(2) = ECH(C)
         STCOFF(B,1) = 3 + CHSEL(C) * 3
         STCOFF(B,2) = 6 + CHSEL(C) * 3
         GO TO 800
C                                       (18) 1D/AC
 440     NSTREM = 3
         STOFIF(3) = 2
         STOFFF(3) = 3
         STOFFS(3) = 1
         STNOPL(3) = 4
         STMCI1(1) = D
         STMCI1(2) = D
         STMCI1(3) = A
         STIFNA(1) = 'D'
         STIFNA(2) = 'D'
         STIFNA(3) = 'AC'
         STTYPE(3) = 0
         STCDA(A) = 3
         STCDA(D) = 1
         DOCH0(A) = .FALSE.
         DOAC(3) = .FALSE.
         STBCH(2) = BCH(D)
         STECH(2) = ECH(D)
         STCOFF(A,1) = 3 + CHSEL(D) * 3
         STCOFF(A,2) = 6 + CHSEL(D) * 3
         GO TO 800
C                                       (19) 2AC/BD
 460     IF((ABS(MCSKYF(A)-MCSKYF(C)).LE.1.D-6*MCSKYF(A)) .AND.
     *      (MCCHAN(A).EQ.MCCHAN(C)) .AND.
     *      (ABS(MCHSEP(A)-MCHSEP(C)).LE.0.01*MCHSEP(A))) THEN
C                                       A & C are compatible
            NSTREM = 3
            STOFIF(3) = 2
            STOFFF(3) = 3
            STOFFS(3) = 1
            STNOPL(1) = 2
            STNOPL(2) = 2
            STNOPL(3) = 4
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI1(3) = B
            STIFNA(1) = 'AC'
            STIFNA(2) = 'AC'
            STIFNA(3) = 'BD'
            STTYPE(3) = 0
            STCDA(A) = 1
            STCDA(B) = 3
            STCDA(C) = 1
            DOCH0(B) = .FALSE.
            DOAC(3) = .FALSE.
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STCOFF(A,2) = 6
            STCOFF(B,1) = 6 + CHSEL(A) * 3 * 2
            STCOFF(B,2) = 9 + CHSEL(A) * 3 * 2
            STCOFF(C,1) = 3
            STCOFF(C,2) = 6 + CHSEL(A) * 3
         ELSE
C                                       A & C are not compatible
            NSTREM = 5
            STOFIF(5) = 2
            STOFFF(5) = 3
            STOFFS(5) = 1
            STNOPL(5) = 4
            STMCI1(1) = A
            STMCI1(2) = A
            STMCI1(3) = C
            STMCI1(4) = C
            STMCI1(5) = B
            STIFNA(1) = 'A'
            STIFNA(2) = 'A'
            STIFNA(3) = 'C'
            STIFNA(4) = 'C'
            STIFNA(5) = 'BD'
            STTYPE(5) = 0
            STCDA(A) = 1
            STCDA(B) = 5
            STCDA(C) = 3
            DOCH0(B) = .FALSE.
            DOAC(5) = .FALSE.
            STBCH(2) = BCH(A)
            STECH(2) = ECH(A)
            STBCH(4) = BCH(C)
            STECH(4) = ECH(C)
            STCOFF(B,1) = 6 + CHSEL(A) * 3 + CHSEL(C) * 3
            STCOFF(B,2) = 9 + CHSEL(A) * 3 + CHSEL(C) * 3
            STCOFF(C,1) = 3 + CHSEL(A) * 3
            STCOFF(C,2) = 6 + CHSEL(A) * 3
            END IF
         GO TO 800
C                                       (20) 2BD/AC
 480     IF((ABS(MCSKYF(B)-MCSKYF(D)).LE.1.D-6*MCSKYF(B)) .AND.
     *      (MCCHAN(B).EQ.MCCHAN(D)) .AND.
     *      (ABS(MCHSEP(B)-MCHSEP(D)).LE.0.01*MCHSEP(B))) THEN
C                                       B & D are compatible
            NSTREM = 3
            STOFIF(3) = 2
            STOFFF(3) = 3
            STOFFS(3) = 1
            STNOPL(1) = 2
            STNOPL(2) = 2
            STNOPL(3) = 4
            STMCI1(1) = B
            STMCI1(2) = B
            STMCI1(3) = A
            STIFNA(1) = 'BD'
            STIFNA(2) = 'BD'
            STIFNA(3) = 'AC'
            STTYPE(3) = 0
            STCDA(A) = 3
            STCDA(B) = 1
            STCDA(D) = 1
            DOCH0(A) = .FALSE.
            DOAC(3) = .FALSE.
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STCOFF(A,1) = 6 + CHSEL(B) * 3 * 2
            STCOFF(A,2) = 9 + CHSEL(B) * 3 * 2
            STCOFF(B,2) = 6
            STCOFF(D,1) = 3
            STCOFF(D,2) = 6 + CHSEL(B) * 3
         ELSE
C                                       B & D are not compatible
            NSTREM = 5
            STOFIF(5) = 2
            STOFFF(5) = 3
            STOFFS(5) = 1
            STNOPL(5) = 4
            STMCI1(1) = B
            STMCI1(2) = B
            STMCI1(3) = D
            STMCI1(4) = D
            STMCI1(5) = A
            STIFNA(1) = 'B'
            STIFNA(2) = 'B'
            STIFNA(3) = 'D'
            STIFNA(4) = 'D'
            STIFNA(5) = 'AC'
            STTYPE(5) = 0
            STCDA(A) = 5
            STCDA(B) = 1
            STCDA(D) = 3
            DOCH0(A) = .FALSE.
            DOAC(5) = .FALSE.
            STBCH(2) = BCH(B)
            STECH(2) = ECH(B)
            STBCH(4) = BCH(D)
            STECH(4) = ECH(D)
            STCOFF(A,1) = 6 + CHSEL(B) * 3 + CHSEL(D) * 3
            STCOFF(A,2) = 9 + CHSEL(B) * 3 + CHSEL(D) * 3
            STCOFF(D,1) = 3 + CHSEL(B) * 3
            STCOFF(D,2) = 6 + CHSEL(B) * 3
            END IF
         GO TO 800
C                                       Get length of baseline in MCDATA
 800  LENBAS = 0
      DO 810 I = 1,NSTREM
         STNOCH(I) = STECH(I) - STBCH(I) + 1
         STZMOD(I) = 0
         IF ((STNOCH(I).GT.1) .AND. (ZSPEC)) STZMOD(I) = 1
         STLEN(I) = STNOIF(I) * STNOPL(I) * STNOCH(I) * 3
         LENBAS = LENBAS + STLEN(I)
 810     CONTINUE
C                                       Set pointers
      STPNT(1) = 1
      DO 815 I = 2,NSTREM
         STPNT(I) = STPNT(I-1) + STLEN(I-1)
 815     CONTINUE
C                                       Set reference frequencies
      DO 820 I = 1,NSTREM
         STFREQ(I) = MCSKYF(STMCI1(I))
 820     CONTINUE
C                                       Fill baseline pointer array
      IPOINT = 0
C                                       Auto correlations
      DO 830 I = 1,MCNANT
         IPOINT = IPOINT + 1
         ANTBAS(IPOINT,1) = MCAORD(I)
         ANTBAS(IPOINT,2) = MCAORD(I)
 830     CONTINUE
C                                       Cross correlations
      DO 850 I = 1,MCNANT
         LIM = I + 1
         IF (LIM.LE.MCNANT) THEN
            DO 840 J = LIM,MCNANT
               IPOINT = IPOINT + 1
               ANTBAS(IPOINT,1) = MCAORD(I)
               ANTBAS(IPOINT,2) = MCAORD(J)
 840           CONTINUE
            END IF
 850     CONTINUE
C                                       Done
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCINI: Do not understand Correlator Mode ''',A4,'''')
 1100 FORMAT ('MCINI: Processing Correlator Code ''',A4,'''',
     *        ' with ',I3,' antennas.')
      END
      SUBROUTINE MCSUM (BLKNO)
C-----------------------------------------------------------------------
C   Accumulates MODCOMP VLA archive FORMAT 1 data from TAPBUF into
C   MCDATA.
C   Routine works by collecting the current baseline record in MCLPR
C   and working from there.  A baseline record may be split between
C   tape blocks.
C   Input:
C      BLKNO    I        Physical block number
C   Input from common:
C      MCLNF1   I        Number of Format 1 data records accumulated
C      TAPBUF   R(*)     Tape buffer
C      TAPIND   I        Pointer in TAPBUF
C      MCCDA    I(4)     Pointer to correlator data area, 1 per CDA
C      MCLCDA   I(2,4)   Length of CDA baseline entry, header (MC words)
C      DOCH0    I(4)     If true, first channel in CDA is "channel 0"
C                        STCDA points to ch 0, +3 to first line channel
C   Input/output in common:
C      MCLNF1   I        Number of Format 1 data records accumulated
C      MCLPR    I(*)     Last partial baseline record as local integers
C      MCWLEF   I        Number of (16 bit) words already processed in
C                        the current baseline
C      MCDATA   R(*)     Visibility data accumulation array.  Baseline
C                        pointer in MC data for IA1<IA2 is:
C                        ((IA1-1)*MCNANT) - (((IA1+1)*IA1)/2) + IA2
C      AVGUVW   R(3,28)  Average antenna u,v and w. (nsec)
C      AVGIAT   D        Average IAT end time (Days)
C      SUMINT   R        Sum of integration time (seconds)
C-----------------------------------------------------------------------
      INTEGER   BLKNO
C
      INTEGER   ITEMP
      LOGICAL   FLAGS(528), BADBL, NOMORE, SPL, POR4
      INTEGER   LOOP, LIMIT, IA, IPOINT, IEND, CURCDA, CDAHL, CDANWB,
     *   LIM1, LIM2, LLIM1, LLIM2, ISTART, NWLEFT, BLPNTR, NUMBAS,
     *   IBASE, NDATA, BLNDX, TBINDX, NPBASE, IA1, IA2, NWDREC, MAXBL,
     *   BINC, IR1, IR2, ZOR
      REAL      SCLFAC
      CHARACTER BBAND*1
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       is integration empty of good
C                                       data?
      IF ((MCLNF1.GT.0) .AND. (BLKNO.EQ.1)) THEN
         LIMIT = LENBAS * ((MCNANT * (MCNANT+1)) / 2)
         DO 10 LOOP = 3,LIMIT,3
            IF (MCDATA(LOOP).GT.0.0) GO TO 15
 10         CONTINUE
C                                       empty clear everything
         MCLNF1 = 0
         END IF
C                                       See if first entry.
 15   IF ((MCLNF1.LE.0) .AND. (BLKNO.EQ.1)) THEN
C                                       First entry, initialize.
C                                       Zero fill MCDATA
         LIMIT = LENBAS * ((MCNANT * (MCNANT+1)) / 2)
         DO 20 LOOP = 1,LIMIT
            MCDATA(LOOP) = 0.0
 20         CONTINUE
         DO 30 LOOP = 1,MXANT
            AVGUVW(1,LOOP) = 0.0
            AVGUVW(2,LOOP) = 0.0
            AVGUVW(3,LOOP) = 0.0
            AVTANT(1,1,LOOP) = 0.0
            AVTANT(2,1,LOOP) = 0.0
            AVTANT(1,2,LOOP) = 0.0
            AVTANT(2,2,LOOP) = 0.0
            AVTSYS(1,1,LOOP) = 0.0
            AVTSYS(2,1,LOOP) = 0.0
            AVTSYS(1,2,LOOP) = 0.0
            AVTSYS(2,2,LOOP) = 0.0
            AVSTAT(LOOP) = 0
            AVANCB(LOOP) = 0
            AVANTD(LOOP) = 0.0D0
 30         CONTINUE
         CALL RFILL (5, 0.0, AVWEAT)
         AVSCOS(1) = 0.0
         AVSCOS(2) = 0.0
         AVRAAP = 0.0D0
         AVDEAP = 0.0D0
C                                       Average IAT end time
         AVGIAT = 0.0D0
C                                       Summed integration time:
         SUMINT = 0.0
         END IF
      IF (BLKNO.EQ.1) THEN
C                                       Accumulate header info
C                                       Words left in partial record
         MCWLEF = 0
         MCLNF1 = MCLNF1 + 1
         IF (REFMJD.GT.0.0) DAYOFF = MCDATE - IDNINT(REFMJD)
C                                       Find band
         CALL FLMBC (FREQ, BBAND)
         IR1 = STMCI1(1)
         IR2 = STMCI2(1)
C                                       Point IR1, IR2 to R-hand pol
         IF (IR1.GT.2) IR1 = IR1 - 2
         IF (IR2.GT.2) IR2 = IR2 - 2
C                                       Sum end IAT time
         AVGIAT = DAYOFF + AVGIAT + MCIATI * 0.159154943D0
C                                       Sum integration
         SUMINT = SUMINT + MCINTG / 19.2
C                                       Sum weather
         DO 40 IA = 1,5
            AVWEAT(IA) = AVWEAT(IA) + MCWEAT(IA)
 40         CONTINUE
         AVSCOS(1) = AVSCOS(1) + MCSCOS(1)
         AVSCOS(2) = AVSCOS(2) + 1.0 / (MAX (1.E-10, MCSCOS(1)))
         AVRAAP = AVRAAP + MCRAAP
         AVDEAP = AVDEAP + MCDCAP
C                                       Sum u,v,w, tsys
         DO 50 IA = 1,MXANT
            AVSTAT(IA) = ZOR (AVSTAT(IA), MCSTAT(IA))
            AVANCB(IA) = ZOR (AVANCB(IA), MCANCB(IA))
            AVANTD(IA) = AVANTD(IA) + MCANTD(IA)
            AVGUVW(1,IA) = AVGUVW(1,IA) + MCAUVW(1,IA)
            AVGUVW(2,IA) = AVGUVW(2,IA) + MCAUVW(2,IA)
            AVGUVW(3,IA) = AVGUVW(3,IA) + MCAUVW(3,IA)
            AVTSYS(1,1,IA) = AVTSYS(1,1,IA) + MCANNS(IR1,IA)
            AVTSYS(2,1,IA) = AVTSYS(2,1,IA) + MCANNS(IR1+2,IA)
            AVTSYS(1,2,IA) = AVTSYS(1,2,IA) + MCANNS(IR2,IA)
            AVTSYS(2,2,IA) = AVTSYS(2,2,IA) + MCANNS(IR2+2,IA)
C                                       revisions 25 and up: fill TANT
C                                       with FE or BE TSYS. Depends on
C                                       band, override using CPARM(2)
            IF (MCFREV.GE.25) THEN
               POR4 = (BBAND.EQ.'P') .OR. (BBAND.EQ.'4')
C                                       back-end values
               IF ((POR4.EQV.DEFTY) .OR. (EVLA(IA))) THEN
                  AVTANT(1,1,IA) = AVTANT(1,1,IA) + MCANTB(IR1,IA)
                  AVTANT(2,1,IA) = AVTANT(2,1,IA) + MCANTB(IR1+2,IA)
                  AVTANT(1,2,IA) = AVTANT(1,2,IA) + MCANTB(IR2,IA)
                  AVTANT(2,2,IA) = AVTANT(2,2,IA) + MCANTB(IR2+2,IA)
C                                       front-end values
               ELSE
                  AVTANT(1,1,IA) = AVTANT(1,1,IA) + MCANTF(IR1,IA)
                  AVTANT(2,1,IA) = AVTANT(2,1,IA) + MCANTF(IR1+2,IA)
                  AVTANT(1,2,IA) = AVTANT(1,2,IA) + MCANTF(IR2,IA)
                  AVTANT(2,2,IA) = AVTANT(2,2,IA) + MCANTF(IR2+2,IA)
                  END IF
               END IF
 50         CONTINUE
         END IF
C
      MAXBL = (MCNANT * (MCNANT+1)) / 2
C                                       Accumulate visibility data:
C                                       Word pointer in logical record
C                                       of TAPIND.
      NWDREC = (FDVEC(2) - 4) / 2
      IPOINT = (BLKNO - 1) * NWDREC
      ISTART = IPOINT + 1
      IEND = IPOINT + NWDREC
      IEND = MIN (IEND, MCLRL)
C                                       Ignore data before first CDA
      IPOINT = MAX (IPOINT, MCCDA(1))
C                                       Find current CDA
 100     CURCDA = 1
         IF ((IPOINT.GE.MCCDA(2)) .AND. (STCDA(2).GT.0)) CURCDA = 2
         IF ((IPOINT.GE.MCCDA(3)) .AND. (STCDA(3).GT.0)) CURCDA = 3
         IF ((IPOINT.GE.MCCDA(4)) .AND. (STCDA(4).GT.0)) CURCDA = 4
C                                       See if in dummy data at end of
C                                       CDA
         CDANWB = MAX (1, MCLCDA(2,CURCDA))
         NPBASE = (IPOINT - MCCDA(CURCDA)) / CDANWB
         IF ((NPBASE.GE.MAXBL) .OR. (MCLCDA(2,CURCDA).LE.0))
     *      CURCDA = CURCDA + 1
         IF (CURCDA.GT.4) GO TO 999
C                                       Try several times
         CDANWB = MAX (1, MCLCDA(2,CURCDA))
         NPBASE = (IPOINT - MCCDA(CURCDA)) / CDANWB
         IF ((NPBASE.GE.MAXBL) .OR. (MCLCDA(2,CURCDA).LE.0))
     *      CURCDA = CURCDA + 1
         IF (CURCDA.GT.4) GO TO 999
C                                       Try several times
         CDANWB = MAX (1, MCLCDA(2,CURCDA))
         NPBASE = (IPOINT - MCCDA(CURCDA)) / CDANWB
         IF ((NPBASE.GE.MAXBL) .OR. (MCLCDA(2,CURCDA).LE.0))
     *      CURCDA = CURCDA + 1
         IF (CURCDA.GT.4) GO TO 999
         IPOINT = MAX (IPOINT, MCCDA(CURCDA))
C                                       Done?
         IF (IPOINT.GE.MCLRL) GO TO 999
         IF (IPOINT.GT.IEND) GO TO 999
C                                       CDA header length
         CDAHL = MCLCDA(1,CURCDA)
C                                       DOCH0 .true. => spectrum
         SPL   = DOCH0(CURCDA)
         IF (SPL) THEN
C                                       Length (MC words) of baseline
            CDANWB = MAX (1, MCLCDA(2,CURCDA))
            LIM1 = STBCH(STCDA(CURCDA)+1)
            LIM2 = STECH(STCDA(CURCDA)+1)
            LLIM1 = LIM1
            LLIM2 = LIM2
            NDATA = (LIM2 - LIM1 + 2) * 2
            BINC = 2
C                                       Continuum
         ELSE
            CDANWB = MAX (1, MCLCDA(2,CURCDA))
            LIM1 = 1
            LIM2 = 5
            LLIM1 = 2
            LLIM2 = 4
            NDATA = 12
            BINC = 3
            END IF
C                                       Number of baselines already done
         NPBASE = (IPOINT - MCCDA(CURCDA)) / CDANWB
C                                       Find number of baselines in CDA
C                                       or TAPBUF.
         IF ((IEND.GT.MCCDA(CURCDA+1)) .AND. (CURCDA.LE.3)) THEN
C                                       End of CDA in this buffer
            NUMBAS = (MCCDA(CURCDA+1) - IPOINT + MCWLEF) / CDANWB
            NWLEFT = 0
            NOMORE = .FALSE.
         ELSE
C                                       Buffer ends before CDA
            NUMBAS = (IEND - IPOINT + MCWLEF) / CDANWB
            NWLEFT = (IEND - IPOINT + MCWLEF) - NUMBAS * CDANWB
            NOMORE = .TRUE.
            END IF
         ITEMP = MAXBL - NPBASE
         NUMBAS = MIN (NUMBAS, ITEMP)
C                                       Loop over baselines
         DO 300 IBASE = 1,NUMBAS
C                                       Baseline index
            BLNDX = ((IPOINT - MCCDA(CURCDA) - MCWLEF) / CDANWB) + 1
            IA1 = ANTBAS(BLNDX,1)
            IA2 = ANTBAS(BLNDX,2)
C                                       GET baseline record
            CALL MCGBRC (ISTART, IEND, CDAHL, CDANWB, LIM1, LIM2,
     *         IPOINT)
C                                       Crack header
            CALL MCCHED (CURCDA, CDAHL, IA1, IA2, LIM1, LIM2,
     *         BADBL, FLAGS, SCLFAC)
C                                       See if whole baseline bad
            IF (BADBL) GO TO 300
C                                       Baseline pointer in MCDATA
            BLPNTR = (BLNDX-1) * LENBAS + 1 + STCOFF(CURCDA,1)
C                                       Scale data
            DO 150 LOOP = 1,NDATA
               TBUFF(LOOP) = MCLPR(CDAHL+LOOP) * SCLFAC
 150           CONTINUE
C                                       If wanted: Van Vleck correction
C                                       but for continuum only
            IF (VLECK.AND..NOT.SPL) CALL MCVLK (IA1, IA2, CURCDA)
C                                       Always do first correlator
            IF (.NOT.FLAGS(1)) THEN
               MCDATA(BLPNTR)   = MCDATA(BLPNTR)   + TBUFF(1)
               MCDATA(BLPNTR+1) = MCDATA(BLPNTR+1) + TBUFF(2)
               MCDATA(BLPNTR+2) = MCDATA(BLPNTR+2) + 1.0
               END IF
C                                       Rest of data
            BLPNTR = (BLNDX-1) * LENBAS + 1 + STCOFF(CURCDA,2)
            TBINDX = 1 + BINC
      INCLUDE 'INCS:ZVND.INC'
            DO 200 LOOP = LLIM1,LLIM2
               IF (.NOT.FLAGS(LOOP)) THEN
                  MCDATA(BLPNTR)   = MCDATA(BLPNTR)   + TBUFF(TBINDX)
                  MCDATA(BLPNTR+1) = MCDATA(BLPNTR+1) + TBUFF(TBINDX+1)
                  MCDATA(BLPNTR+2) = MCDATA(BLPNTR+2) + 1.0
                  END IF
               BLPNTR = BLPNTR + 3
               TBINDX = TBINDX + BINC
 200           CONTINUE
 300        CONTINUE
C                                       Deal with partial baseline data
      IF (NWLEFT.GT.0) THEN
         CALL MCGBRC (ISTART, IEND, CDAHL, CDANWB, LIM1, LIM2, IPOINT)
         END IF
      IF (NOMORE) GO TO 999
C                                       In case CDA done
      IF ((NUMBAS+NPBASE).GE.MAXBL) IPOINT = MCCDA(CURCDA+1)
C                                       Trap indexing problem that can
C                                       lead to an infinite loop -
C                                       if this is invoked the last
C                                       baseline of the CDA is lost.
      IF (NUMBAS.LE.0) IPOINT = MCCDA(CURCDA+1)
C                                       If data from another CDA loop
      IF (CURCDA.LT.4) GO TO 100
C
 999  RETURN
      END
      SUBROUTINE MCNORM
C-----------------------------------------------------------------------
C   Normalizes the accumulated MODCOMP VLA archive FORMAT 1
C   record in array RECORD.  Mostly the correlator values  and times
C   are averaged.  Uses /MODCOM/ heavily.
C   Input in common:
C      MCINTG   I        Modcomp integration in waveguide cycles (52
C                        msec)
C   Input/Ouptut in common:
C      MCLNF1   I        Number of data records accumulated
C      MCDATA   R(*)     Visibility data accumulation array.  Baseline
C                        pointer in MC data for IA1<IA2 is:
C                        ((IA1-1)*MCNANT) - (((IA1+1)*IA1)/2) + IA2
C   Output in common:
C      AVGUVW   R(3,28)  Average antenna u,v and w
C      AVGIAT   D        Average IAT end time (Days)
C-----------------------------------------------------------------------
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DDCH.INC'
C
      INTEGER   NWORDS, LOOP
      REAL      WFACT, XFACT, COUNT
C-----------------------------------------------------------------------
C                                       Weighting factor (10sec => 1.0)
      WFACT = MCINTG / 192.
C                                       Make sure there is data;
C                                       Don't bother if only 1.
      IF (MCLNF1.LE.0) GO TO 999
      IF (MCLNF1.EQ.1) GO TO 200
C                                       Average time
      XFACT = 1.0 / MCLNF1
      AVGIAT = AVGIAT * XFACT
C                                       u,v,w, tsys
      DO 50 LOOP = 1,MXANT
         AVGUVW(1,LOOP) = AVGUVW(1,LOOP) * XFACT
         AVGUVW(2,LOOP) = AVGUVW(2,LOOP) * XFACT
         AVGUVW(3,LOOP) = AVGUVW(3,LOOP) * XFACT
         AVTSYS(1,1,LOOP) = AVTSYS(1,1,LOOP) * XFACT
         AVTSYS(2,1,LOOP) = AVTSYS(2,1,LOOP) * XFACT
         AVTSYS(1,2,LOOP) = AVTSYS(1,2,LOOP) * XFACT
         AVTSYS(2,2,LOOP) = AVTSYS(2,2,LOOP) * XFACT
         AVTANT(1,1,LOOP) = AVTANT(1,1,LOOP) * XFACT
         AVTANT(2,1,LOOP) = AVTANT(2,1,LOOP) * XFACT
         AVTANT(1,2,LOOP) = AVTANT(1,2,LOOP) * XFACT
         AVTANT(2,2,LOOP) = AVTANT(2,2,LOOP) * XFACT
         AVANTD(LOOP) = AVANTD(LOOP) / MCLNF1
 50      CONTINUE
      DO 60 LOOP = 1,5
         AVWEAT(LOOP) = AVWEAT(LOOP) * XFACT
 60      CONTINUE
      AVSCOS(1) = AVSCOS(1) * XFACT
      AVSCOS(2) = AVSCOS(2) * XFACT
      AVRAAP = AVRAAP / MCLNF1
      AVDEAP = AVDEAP / MCLNF1
C                                       Average data
      NWORDS = LENBAS * (MCNANT * (MCNANT+1)) / 2
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,NWORDS,3
         IF (MCDATA(LOOP+2).GT.0.0) THEN
            COUNT = MAX (1.0, MCDATA(LOOP+2))
            MCDATA(LOOP) = MCDATA(LOOP) / COUNT
            MCDATA(LOOP+1) = MCDATA(LOOP+1) / COUNT
            MCDATA(LOOP+2) = COUNT * WFACT
            END IF
 100     CONTINUE
C                                       Reset counter
      MCLNF1 = 0
      GO TO 999
C                                       1 point, set weights
 200  NWORDS = LENBAS * (MCNANT * (MCNANT+1)) / 2
      INCLUDE 'INCS:ZVND.INC'
      DO 300 LOOP = 1,NWORDS,3
         MCDATA(LOOP+2) = MCDATA(LOOP+2) * WFACT
 300     CONTINUE
C                                       Reset counter
      MCLNF1 = 0
C
 999  RETURN
      END
      SUBROUTINE MCGBRC (ISTART, IEND, CDAHL, CDANWB, LIM1, LIM2,
     *   IPOINT)
C-----------------------------------------------------------------------
C   Routine to convert baseline record into array MCLPR.  Only the
C   channels desired are extracted.
C   This routine works in three modes:
C      1) some old, valid data already resides in MCLPR
C      2) no old data but only a partial record in in the current tape
C         buffer.
C      3) A complete record is in the tape buffer.
C   Note: ISTART, IEND and IPOINT are relative to the first word of the
C   RCA rather than TAPIND.  Must add 2 to pointer in TAPBUF to allow
C   for the first two words (rec. no., no. rec.) of the tape block.
C   Inputs:
C      ISTART   I        First (MC) word in tape buffer
C      IEND     I        Highest (MC) word in tape buffer
C      CDAHL    I        Number of (MC) words in header
C      CDANWB   I        Number of (MC) words per baseline
C      LIM1     I        Low channel limit to copy
C      LIM2     I        High channel limit to copy
C   Input from common:
C      TAPBUF   R(*)     Tape buffer
C      TAPIND   I        Pointer in TAPBUF
C   Input/output:
C      IPOINT   I        MC word number of first word of next record
C   Input/output in common:
C      MCLPR(*) I        Baseline record all as local integers
C      MCWLEF   I        Number of (16 bit) words already processed in
C                        the current baseline
C-----------------------------------------------------------------------
      INTEGER   ISTART, IEND, CDAHL, CDANWB, LIM1, LIM2, IPOINT
C
      INCLUDE 'MC2.INC'
C
      INTEGER   NDATA, ITEMP, NCONV, BPNT, IPTR, NLEFT
C-----------------------------------------------------------------------
C                                       Partial previous record
      IF (MCWLEF.GT.0) THEN
         NCONV = CDAHL - MCWLEF
         BPNT =  3
         IPTR = MCWLEF + 1
         IF (NCONV.GT.0)
     *      CALL ZI16IL (NCONV, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
C                                       Convert data
         IF (MCWLEF.GT.CDAHL) THEN
C                                       Some old data
            NCONV = CDANWB - MCWLEF
            BPNT = 3
            IPTR = MCWLEF + 1
C                                       Contiguous channels
            IF (LIM1.EQ.1) THEN
               NDATA = (LIM2+1) * 2
               NDATA = MIN (NDATA, NCONV)
               CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
C                                       Non-contiguous
            ELSE
               IF (IPTR.LE.(CDAHL+2)) THEN
                  NDATA = 3 - (IPTR - CDAHL)
                  CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND),
     *               MCLPR(IPTR))
                  IPTR = IPTR + NDATA
                  BPNT = BPNT + NDATA
                  END IF
               ITEMP = MCWLEF - CDAHL - 2
               ITEMP = MAX (0, ITEMP)
               NDATA = (LIM2 - LIM1 + 1) * 2 - ITEMP
               BPNT = BPNT + LIM1 * 2 - ITEMP * 2
               CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
               END IF
         ELSE
C                                       No old data
            NCONV = CDAHL + (LIM2+1) * 2
            BPNT = 3 + CDAHL - MCWLEF
            IPTR = CDAHL + 1
C                                       Contiguous channels
            IF (LIM1.EQ.1) THEN
               NDATA = NCONV
               CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
C                                       Non-contiguous
            ELSE
               NDATA = 2
               CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
               NDATA = (LIM2 - LIM1 + 1) * 2
               BPNT = BPNT + LIM1 * 2
               CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR+2))
               END IF
            END IF
            IPOINT = IPOINT + CDANWB - MCWLEF
            MCWLEF = 0
            GO TO 999
         END IF
C                                       Partial record in buffer
      IF ((IPOINT+CDANWB).GT.IEND) THEN
         NLEFT = IEND - IPOINT
         MCWLEF = NLEFT
C                                       Accumulate record
C                                       Header
         NCONV = CDAHL
         NCONV = MIN (NCONV, NLEFT)
         BPNT = IPOINT - ISTART + 4
         CALL ZI16IL (NCONV, BPNT, TAPBUF(TAPIND), MCLPR)
         NLEFT = NLEFT - NCONV
C                                       See if done
         IF (NLEFT.LE.0) GO TO 999
C                                       Convert data
         NCONV = CDAHL + (LIM2+1) * 2
         NCONV = MIN (NCONV, NLEFT)
         BPNT = IPOINT + CDAHL - ISTART + 4
         IPTR = CDAHL + 1
C                                       Contiguous channels
         IF (LIM1.EQ.1) THEN
            NDATA = NCONV
            CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
C                                       Non-contiguous
         ELSE
            NDATA = 2
            NDATA = MIN (NDATA, NLEFT)
            CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
            NLEFT = NLEFT - LIM1 * 2
C                                       See if done
            IF (NLEFT.LE.0) GO TO 999
            NDATA = (LIM2 - LIM1 + 1) * 2
            NDATA = MIN (NDATA, NLEFT)
            BPNT = BPNT + LIM1 * 2
            CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR+2))
            END IF
C                                       Increment buffer counter
         IPOINT = IPOINT + CDANWB
         GO TO 999
         END IF
C                                       Full record in buffer:
C                                       Header
      BPNT = IPOINT - ISTART + 4
C                                       Contiguous channels
      IF (LIM1.EQ.1) THEN
         NCONV = CDAHL + (LIM2+1) * 2
         CALL ZI16IL (NCONV, BPNT, TAPBUF(TAPIND), MCLPR)
C                                       Non-contiguous
      ELSE
         NCONV = 2 + CDAHL
         CALL ZI16IL (NCONV, BPNT, TAPBUF(TAPIND), MCLPR)
         NDATA = (LIM2 - LIM1 + 1) * 2
         BPNT = BPNT + LIM1 * 2 + CDAHL
         IPTR = CDAHL + 3
         CALL ZI16IL (NDATA, BPNT, TAPBUF(TAPIND), MCLPR(IPTR))
         END IF
C                                       Increment buffer counter
      IPOINT = IPOINT + CDANWB
C
 999  RETURN
      END
      SUBROUTINE MCCHED (CURCDA, CDAHL, IA1, IA2, LIM1, LIM2, BADBL,
     *   FLAGS, SCLFAC)
C-----------------------------------------------------------------------
C   Routine to crack data headers and set flags.
C   Flagging not implemented in initial version.
C      Word  Bits        Description
C      0-31        Bit map for spectral line only; one bit per channel
C                  no. channels/16 will be present.
C      -2    0,1   type of flagging, 0=>whole baseline, 1=>some flagged
C            3-7   Cause of flagging:
C                      5 => Frequency RMS too high
C                      6 => Time RMS too big
C                      7 => Value too big (clip)
C           11-15  5 bit scale factor (exponential)
C      -1    0-3   Bit map for continuum (AA,CC,AC,CA)
C   Inputs:
C      CURCDA   I        Current CDA number
C      CDAHL    I        CDA header length (in 16 bit words)
C      IA1      I        First antenna number
C      IA2      I        Second antenna number
C      LIM1     I        Low channel limit
C      LIM2     I        High channel limit
C   Input from common:
C      MCLPR(*) I        Baseline record as local integers
C      DOCH0    I(4)     If true, first channel in CDA is "channel 0"
C                        STCDA points to ch 0, +3 to first line channel
C      MCMODE   I        Correlator mode code:
C                         1=cont,
C                         2=1A, 3=1B, 4=1C, 5=1D,
C                         6=2AB, 7=2AC, 8=2AD, 9=2BC, 10=2BD, 11=2CD,
C                        12=4, 13=PA, 14=PB,
C                        15=1A/C, 16=1B/C, 17=1C/C, 18=1D/C,
C                        19=2AC/C, 20=2BD/C,
C                        21=2A, 22=4A
C   Outputs:
C      BADBL    L        If true, all data is bad
C      FLAGS(*) L        Flag array (true=bad)
C      SCLFAC   R        Visibility scaling factor
C-----------------------------------------------------------------------
      INTEGER   CURCDA, CDAHL, IA1, IA2, LIM1, LIM2
      LOGICAL   BADBL, FLAGS(*)
      REAL      SCLFAC
C
      INCLUDE 'MC2.INC'
      INTEGER   IEXP, IEXMSK, IR1, IR2, IRPOL1(4,20), IRPOL2(4,20),
     *   FGOFF, ZAND, LOOP, NCORR, BITS(32), CBTMP, I, MASK, ZOR, TIT(3)
      REAL      TTIME, TITSEC
      LOGICAL   T, F, ISLINE, BADIF, OKEXP
      CHARACTER TSIGN*1
      INCLUDE 'INCS:DMSG.INC'
      DATA IEXMSK /31/
C                                       new lines added 7/8/08 EWG
C                     1         2        3        4        5
      DATA IRPOL1 /1,2,0,0, 1,0,0,0, 0,2,0,0, 0,0,3,0, 0,0,0,4,
C             6        7        8        9       10       11
C    *    1,2,0,0, 1,0,3,0, 1,0,0,4, 0,3,2,0, 0,4,0,2, 0,0,3,4,
     *    1,2,0,0, 1,0,3,0, 1,0,0,4, 0,2,3,0, 0,2,0,4, 0,0,3,4,
C            12       13       14       15       16       17
C    *    1,3,2,4, 1,1,3,3, 4,4,2,2, 1,2,0,0, 1,2,0,0, 0,2,3,0,
     *    1,2,3,4, 1,3,1,3, 2,4,2,4, 1,2,0,0, 1,2,0,0, 0,2,3,0,
C            18       19       20
     *    1,0,0,4, 1,2,3,0, 1,2,0,4/
C                     1         2        3        4        5
      DATA IRPOL2 /3,4,0,0, 1,0,0,0, 0,2,0,0, 0,0,3,0, 0,0,0,4,
C             6        7        8        9       10       11
C    *    1,2,0,0, 1,0,3,0, 1,0,0,4, 0,3,2,0, 0,4,0,2, 0,0,3,4,
     *    1,2,0,0, 1,0,3,0, 1,0,0,4, 0,2,3,0, 0,2,0,4, 0,0,3,4,
C            12       13       14       15       16       17
C    *    1,3,2,4, 1,2,2,1, 4,2,2,4, 1,4,0,0, 3,2,0,0, 0,4,3,0,
     *    1,2,3,4, 1,3,3,1, 2,4,4,2, 1,4,0,0, 3,2,0,0, 0,4,3,0,
C            18       19       20
     *    3,0,0,4, 1,4,3,0, 3,2,0,4/
C      DATA MASK /'FF7F'X/
      DATA MASK /65407/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize flags
C                                       Line
      IF (DOCH0(CURCDA)) THEN
         NCORR = LIM2 - LIM1 + 2
         ISLINE = T
         FGOFF = CDAHL - 1
C                                       Continuum
      ELSE
         NCORR = 4
         ISLINE = F
         FGOFF = 1
C                                       No AC data
         BADBL = IA1.EQ.IA2
         IF (BADBL) GO TO 999
         END IF
C                                       Polarization pointers
      IR1 = IRPOL1(CURCDA,MCMODE)
      IR2 = IRPOL2(CURCDA,MCMODE)
      DO 50 LOOP = 1,NCORR
         FLAGS(LOOP) = F
 50      CONTINUE
C                                       Get flags
C                                       NYI
CC                                       See if want MC flags
C      IF (PASFLG .OR. (.NOT.SOMFLG)) GO TO 999
C                                       Line flags
      IF (ISLINE) THEN
         BADBL = (IFFLAG(IR1,IA1)) .OR.( IFFLAG(IR2,IA2))
C                                       NYI
C                                       Need to include LIM1,LIM2
C         FLGPNT = 1
C         NBIT = 16
C         DO 100 LOOP = 3,CDAHL
C            CALL ZGTBIT (NBIT, MCLPR(LOOP), I2ARR(1))
C            DO 90 LOOP2 = 1,16
C               FLAGS(FLGPNT) = I2ARR(17-LOOP2).EQ.1
C               FLGPNT = FLGPNT + 1
C 90            CONTINUE
C 100        CONTINUE
C                                       Continuum
      ELSE
C                                       IF flags
         FLAGS(1) = IFFLAG(IR1,IA1) .OR. IFFLAG(IR1,IA2)
         FLAGS(2) = IFFLAG(IR2,IA1) .OR. IFFLAG(IR2,IA2)
         FLAGS(3) = IFFLAG(IR1,IA1) .OR. IFFLAG(IR2,IA2)
         FLAGS(4) = IFFLAG(IR2,IA1) .OR. IFFLAG(IR1,IA2)
C   Holography fix:  disable the flagging for Tsys fluctuations, if this
C   is the only problem; retain all other flagging *  mjk, 19 March 1993
C
         IF (PASFLG) THEN
            BADIF = FLAGS(1) .OR. FLAGS(2) .OR. FLAGS(3) .OR. FLAGS(4)
            IF (BADIF) THEN
               CBTMP = ZOR (MCANCB(IA1), MCANCB(IA2))
               CALL ZGTBIT (32, CBTMP, BITS)
               IF (BITS(8).NE.0) THEN
                  CBTMP = ZAND (CBTMP, MASK)
                  IF (CBTMP.EQ.0) THEN
                     DO 100 I = 1,4
                        FLAGS(I) = .FALSE.
  100                   CONTINUE
                     END IF
                  END IF
               END IF
            END IF
C                                       NYI
C         IF (.NOT.PASFLG) THEN
C            CALL ZGTBIT (NBIT, MCLPR(2), I2ARR(1))
C            FLAGS(1) = FLAGS(1) .OR. (I2ARR(16).EQ.1)
C            FLAGS(2) = FLAGS(2) .OR. (I2ARR(15).EQ.1)
C            FLAGS(3) = FLAGS(3) .OR. (I2ARR(14).EQ.1)
C            FLAGS(4) = FLAGS(4) .OR. (I2ARR(13).EQ.1)
C            END IF
         BADBL = FLAGS(1) .AND. FLAGS(2) .AND. FLAGS(3) .AND. FLAGS(4)
         END IF
C                                       NYI
C      NBIT = 16
C      CALL ZGTBIT (NBIT, MCLPR(1), I2ARR(1))
CC                                       I2ARR(16)=1 => all bad
CC                                       I2ARR(15)=1 => Some bad
CC                                       See if all bad
C      BADBL = I2ARR(16).EQ.1
C      IF (BADBL) GO TO 999
CC                                       See if any data flagged
C      SOMFLG = I2ARR(15).EQ.1
C                                       Find scale factor. Code change
C                                       13may97: only use ZAND for older
C                                       revisions.  Online change caused
C                                       negative values in MCLPR which
C                                       made the ZAND operation fail.
C                                       Old tapes have a bias of 2
      IF (MCFREV.LT.23) THEN
         IEXP = ZAND (IEXMSK, MCLPR(FGOFF)) - 2
      ELSE
         IEXP = MCLPR(FGOFF)
         END IF
      OKEXP = (IEXP.GE.-10) .AND. (IEXP.LE.22)
      IF (OKEXP) THEN
         SCLFAC = 2.0 ** (-IEXP - 8)
      ELSE
         SCLFAC = 1.0
         TTIME = SNGL (AVGIAT)
         CALL TFDHMS (TTIME, 1, TSIGN, TIT, TITSEC)
         WRITE (MSGTXT,1000) TIT, TITSEC, IA1, IA2, IEXP
         IF (.NOT.BADBL) THEN
            BADEXP(IA1) = BADEXP(IA1) + 1
            BADEXP(IA2) = BADEXP(IA2) + 1
            IF (BADEXS.LT.200) CALL MSGWRT (8)
            BADEXS = BADEXS + 1
            END IF
         END IF
      BADBL = BADBL .OR. (.NOT.OKEXP)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCCHED:',I3,'/',2(I2.2,':'),F4.1,I3,' x',I3,
     *   '  BAD GAIN EXPONENT',I8)
      END
      SUBROUTINE ANTUPD (IS)
C-----------------------------------------------------------------------
C   Updates the antenna file before the uv file is closed, to cope
C   with antennas entering the array after the start of the observation.
C   Input:
C      IS       I        Output stream number
C-----------------------------------------------------------------------
      INTEGER   IS
C
      INCLUDE 'FILLM.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C
      INTEGER   IERR, VER, LUNAN, I, NAT, ITBUFF(512), I4, IRTEMP(2),
     *   IANWR
      DOUBLE PRECISION   FTEMP
      EQUIVALENCE (TBUFF, ITBUFF)
      EQUIVALENCE (IRTEMP, FTEMP)
C
      DATA LUNAN /27/
C-----------------------------------------------------------------------
C                                       init an file
      VER = 1
      NAT = MCNANT
      TIMSYS = 'IAT'
C                                       Open file
      CALL ANTINI ('WRIT', TBUFF, DISKO(IS), CNOOUT(IS), VER, CATBLK,
     *   LUNAN, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
C                                       See if entries exist
      I4 = ITBUFF(5)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over entries,
C                                       rewriting them if
C                                       they need updating.
      DO 20 I = 1,MXANT
         IANRNO = I
         CALL TABAN ('READ', TBUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
         IF ((STAXYZ(1).EQ.0.D0) .AND. (STAXYZ(2).EQ.0.D0)
     *      .AND. (STAXYZ(3).EQ.0.D0)) THEN
            STAXYZ(1) = MCAXYZ(1,I) * 1.0D-9 * VELITE
            STAXYZ(2) = MCAXYZ(2,I) * 1.0D-9 * VELITE
            STAXYZ(3) = MCAXYZ(3,I) * 1.0D-9 * VELITE
            STAXOF = MCANBA(I) * 1.0D-9 * VELITE
            NOSTA = I
            IANWR = IANRNO - 1
            ANNAME = MCANTN(I)
            CALL TABAN ('WRIT', TBUFF, IANWR, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 20      CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 0, IANRNO, TBUFF, TBUFF, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Error ',I3,' occurred updating antenna (AN) table')
      END
      SUBROUTINE MCTELL (DONE)
C-----------------------------------------------------------------------
C   checks the TELL area for a message periodically.  It does this no
C   more often than JDTINC days, or whenever NEWFIL is true.
C   Output:
C      DONE     L   A quit or abort command has been received.
C   Output in common:
C      JDTELL   D   last TELL time in Julian days
C      TSTART, TEND, XTR, SELECT(9), SELECT(10) if new TIMERANGE sent
C      SELECT(10) = -10000 if abort OR QUIT
C-----------------------------------------------------------------------
      LOGICAL   DONE
C
      INTEGER   SCRTCH(256), IERR, ID(6)
      REAL      TELTIM(8), TELAPM(10)
      CHARACTER OPTELL*4
      DOUBLE PRECISION JDNOW
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FILLM.INC'
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /FLMTLL/ TELTIM, TELAPM
C-----------------------------------------------------------------------
C                                       quit already received:
C                                       quit on new scan
      IF (DOQUIT) THEN
         DONE = NEWFIL
         GO TO 999
         END IF
C                                       stop already received:
C                                       stop now.
      IF (DOSTOP) THEN
         DONE = .TRUE.
         GO TO 999
         END IF
C                                       check time
      DONE = .FALSE.
      CALL ZDATE (ID(1))
      CALL ZTIME (ID(4))
      CALL DAT2JD (ID, JDNOW)
      IF ((.NOT.NEWFIL) .AND. (JDNOW-JDTELL.LE.JDTINC)) GO TO 999
C                                       check tell file
      JDTELL = JDNOW
      MSGTXT = ' '
      CALL GTTELL (18, OPTELL, TELTIM, SCRTCH, IERR)
      IF (IERR.EQ.3) THEN
         MSGTXT = 'has been told to ABORT, will STOP now'
         DONE = .TRUE.
      ELSE IF (IERR.EQ.2) THEN
         MSGTXT = 'has been told to QUIT at the end of this scan'
         DOQUIT = .TRUE.
         DONE = NEWFIL
C                                       Start, stop times
      ELSE IF (IERR.EQ.1) THEN
         MSGTXT = 'has been told to change parameters'
         CALL RCOPY (8, TELTIM, XTR)
         TSTART = XTR(1) + (XTR(2) / 24.0) + (XTR(3) / 1440.) +
     *      (XTR(4) / 86400.)
         IF (TSTART.EQ.0.0) THEN
            XTR(1) = -99.0
            TSTART = -99.0
            END IF
         TEND = XTR(5) + (XTR(6) / 24.0) + (XTR(7) / 1440.) +
     *      (XTR(8) / 86400.)
         IF (TEND.EQ.0.0) THEN
            XTR(5) = 999.0
            TEND = 999.0
            END IF
         IF ((TEND-TSTART).LT.1.0E-4) THEN
            XTR(1) = -99.0
            XTR(5) = 999.0
            TSTART = -99.0
            TEND = 999.0
            END IF
         SELECT(9) = TSTART * TWOPI
         SELECT(10) = TEND * TWOPI
C                                       Append?
         DOMANY = ABS (TELAPM(1)-1.0).LT.0.1
         DOBREK = ABS (TELAPM(1)-2.0).LT.0.1
C                                       Controlled stop?
         DOSTOP = ABS (TELAPM(1)-3.0).LT.0.1
         IF (DOMANY) THEN
            CALL MSGWRT (4)
            MSGTXT = 'will do MANY files now'
         ELSE IF (DOBREK) THEN
            CALL MSGWRT (4)
            MSGTXT = 'will BREAK files now'
         ELSE IF (DOSTOP) THEN
            CALL MSGWRT (4)
            MSGTXT = 'will STOP now'
            END IF
         DONE   = DOSTOP
         END IF
C
      IF (DONE) SELECT(10) = -1.E4
C
      IF (MSGTXT.NE.' ') CALL MSGWRT (4)
C
 999  RETURN
      END
      SUBROUTINE MCVLK (IA1, IA2, CDA)
C-----------------------------------------------------------------------
C   Performs Van Vleck correction on certain elements of TBUFF, which is
C   part of MC2.INC.  First, it uses the complex gains CGN to undo the
C   gains.  Then, it applies the correction, and finally, the gains CGN
C   are reapplied.
C
C   The CGN values are in order AA,CC,AC,CA, and TBUFF is in the same
C   order as long as IA1<IA2.  In the opposite case, the TBUFF order is
C   AA,CC,CA,AC, and CGN(3,2) and CGN(4,2) should be reversed. They only
C   differ in the sign of their imaginary parts.  Since the imaginary
C   parts of CGN(1,2) and CGN(2,2) are 0, we can put a negative sign
C   in front of *all* imaginary parts of CGN whenever IA2>IA1.
C
C   For IA2>IA1, the TBUFF phases should also be inverted, but the Van
C   Vleck correction is invariant for phase inversion.
C-----------------------------------------------------------------------
      INCLUDE 'MC2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER IA1, IA2, CDA, TOF, CDOF, I, J
      REAL RE, IM, RHATR, RHATI, RHATCR, RHATCI, REC, IMC,
     *     DENOM, SIGN, CGNR, CGNI, SENR, SENI, V, W, F, FN, A
C
C                                       V,W are sampler levels of the
C                                       samplers of the two antennas.
C                                       Here the nominal threshold
C                                       0.6120 is used.
C
      DATA V, W, F /0.6120, 0.6120, 0.54054/
C-----------------------------------------------------------------------
C                                       normalizing factor
      A  = SQRT ((V * V - 1.0) * (W * W - 1.0))
      FN = F * A * 1.570796327 * EXP (0.5 * (V * V + W * W))
C                                       use minus sign in front of
C                                       imaginary part when IA2 > IA1
      IF (IA1.LT.IA2) THEN
         SIGN =  1.0
      ELSE
         SIGN = -1.0
         END IF
C                                       CDA dependent offset in CGN
      CDOF = 4 * (CDA - 1)
C                                       loop over 4 polarizations. CDOF
C                                       determines A/C or B/D.
      DO 100 I = 1, 4
         J = I + CDOF
C                                       offset in TBUFF
         TOF = 3 * (I - 1) + 1
         RE = TBUFF(TOF)
         IM = TBUFF(TOF+1)
C                                       temp names to avoid clutter
         CGNR = CGN(J,1,IA1,IA2)
         CGNI = CGN(J,2,IA1,IA2)
         SENR = CGNR / 256.0
         SENI = CGNI / 256.0
C                                       undo gains, complex division
         DENOM = CGNR * CGNR + CGNI * CGNI
         RHATR = (RE * CGNR + SIGN * IM * CGNI) / DENOM
         RHATI = (IM * CGNR - SIGN * RE * CGNI) / DENOM
C
C                                       apply Van Vleck correction
         IF (INVRT) THEN
            RHATCR = ASIN (A * RHATR) / FN
            RHATCI = ASIN (A * RHATI) / FN
         ELSE
            RHATCR =  SIN (FN * RHATR) / A
            RHATCI =  SIN (FN * RHATI) / A
            END IF
C                                       test output
         IF (J.EQ.1.AND.TVLK) THEN
            WRITE(MSGTXT,1000) ABS(RHATR), RHATCR, RHATCR/RHATR
            CALL MSGWRT (4)
            END IF
C                                       reapply gains
         REC    = RHATCR * CGNR - SIGN * RHATCI * CGNI
         IMC    = RHATCI * CGNR + SIGN * RHATCR * CGNI
C                                       reinsert in TBUFF
         TBUFF(TOF)   = REC
         TBUFF(TOF+1) = IMC
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (3F9.5)
      END
      SUBROUTINE FLDKIO (OP, FDVEC, BUFF, IBIND, IERR)
C-----------------------------------------------------------------------
C   FLDKIO is for reading disk files for FILLM
C   NOTE: FLDKIO WORKS IN REAL (8-BIT) BYTES, NOT THE AIPS HALF-INTEGER
C   "BYTES".
C   Usage notes:
C     1) Zero fill FDVEC before filling in relevant values.
C     2) Opening the file.  If FLDKIO determines that the file is not
C        open it will do so.  Once the file is open the file descriptor
C        vector FDVEC must be used in each call.
C     3) Initialization.  FLDKIO initializes the I/O using the values in
C        FDVEC when it opens the file.
C        If OP='OPRD' the file is opened but I/O is not initialized;
C        this allows positioning tapes before the actual I/O starts.
C     5) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.
C   Disks: any other LUN, for VLA blocking (n * 2048)
C          FLDKIO requires that FDVEC(2) = 2048, FDVEC(31) = 0 on input
C          FLDKIO sets FDVEC(6) = 1
C          The desired file name must be in FDVEC(7-30) packed string
C          WITHOUT THE FILE NUMBER which will be appended
C   Inputs:
C      OP     C*4     Operation code: 'READ','CLOS', 'OPRD'
C   Input/Output:
C      FDVEC  I(50)   File descriptor vector.
C                     1 = LUN to use, set before first call.
C                         129-NTAPED to 128 => tape => error
C                     2 = Logical record length in bytes (8-bit)
C                     3 = Buffer size in 8-bit bytes I
C                     4 =
C                     5 =
C                     6 =
C                  7-30 = File name for disk files (24 char. packed)
C                    31 =
C                    32 =
C                    33 = Desired file number
C                 34-39   Reserved for future use
C                The following are used by FLDKIO:
C                    40 = FTAB pointer
C                    41 =
C                    42 = LBYTES - number of bytes read
C                    43 = Number of 2048's left to read
C                 44-50 = reserved for future use
C      BUFF   R(*)   Buffer for I/O must be large enough for the largest
C                    transfer rounded up to the next larger number of
C                    disk blocks.
C   Outputs:
C      IBIND  I      The location in BUFF of the start of the next
C                    record. Note: IBIND points to the address in the
C                    I   array irregardless of the actual data type.
C      IERR   I      Error return: 0 => ok
C                             2 => input error
C                             3 => i/o error on initialize
C                             4 => end of file
C                             5 => beginning of medium
C                             6 => end of medium
C                             7 => Buffer too small
C                             8 => error opening file.
C                            10 => data record shorter than 1 logical
C   Usage notes: For map i/o the first 16 words in each FTAB entry
C   contain a user table to handle double buffer i/o,  the rest
C   contain system-dependent I/O tables.
C   FTAB user table   entries, with offsets from the FIND pointer are:
C   129-ntaped <= LUN <= 128 will generate an error - tapes not allowed
C      FTAB + 0 =>  LUN using this entry
C             1 =>
C             2 =>  Number of 8-bit bytes in a logical record
C             3 =>  Number of disk logical records in each transfer (1)
C             4 =>
C             5 =>  Block offset on disk file for next operation I
C             6 =>
C           7-8 =>
C             9 =>  I/O opcode 0=read, 1=write
C            10 =>  1 => tape, 2 => disk
C            11 =>
C            12 =>
C            13 =>  1 => I/O active, else inactive (not initialized).
C            14 =>  number bytes last read/write to buffer 1
C            15 =>  number bytes last read/write to buffer 2
C   To suppress messages about logical and physical record lengths
C   being inconsistent, set MSGSUP to 1000 or greater.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), BUFF(*), IBIND, IERR
      CHARACTER OP*4
C
      CHARACTER PHNAME*48, STRNG*8
      LOGICAL   DISK, DOREAD
      INTEGER   JERR, IND, JBUF, MBYTE, I, J, NUMMSG, LBYTES, I4TEMP,
     *   BUFSZ4, RECNO, BUFPNT, NBYPWD, ITRIM, IFILE, FCBOFF, MCB(2),
     *   LRW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C
      SAVE NUMMSG
C-----------------------------------------------------------------------
      IERR = 2
      IBIND = 1
C                                       Initial parameters
      IND = FDVEC(40)
      NBYPWD = NBITWD / 8
C                                       See if tape or disk.
C??                                     BETTER TEST HERE
      DISK = (FDVEC(1).LT.129-NTAPED) .OR. (FDVEC(1).GT.128)
      IF (DISK) THEN
         IF (FDVEC(2).NE.13*2048) GO TO 985
         IF (FDVEC(31).NE.0) GO TO 985
         FDVEC(6) = 1
      ELSE
         MSGTXT = 'FLDKIO: ONLY DOES FILLM DISK I/O'
         GO TO 990
         END IF
      BUFSZ4 = FDVEC(3)
C??                                     CHECK HERE ON DEVTAB VALUE
      IF (MOD (DEVTAB(FDVEC(1)),2).EQ.1) GO TO 999
C                                       Test OPcode
      DOREAD = (OP.EQ.'OPRD') .OR. (OP.EQ.'READ')
      IF ((OP.NE.'READ') .AND. (OP.NE.'CLOS') .AND. (OP.NE.'OPRD')) THEN
         WRITE (MSGTXT,1000) OP
         GO TO 990
         END IF
C                                       Check if CLOSE
      IERR = 0
      IF (OP.EQ.'CLOS') GO TO 900
C                                       Check if Open but I/O inactive
      IF (((IND.GT.0) .AND. (FDVEC(1).EQ.FTAB(IND))) .AND.
     *   (FTAB(IND+13).EQ.1)) GO TO 500
C                                       Open
      IF ((IND.LE.0) .OR. (FDVEC(1).NE.FTAB(IND))) THEN
         PHNAME = ' '
         CALL H2CHR (48, 1, FDVEC(7), PHNAME)
         I = ITRIM (PHNAME)
         IFILE = MAX (1, FDVEC(33))
         WRITE (STRNG,1005) IFILE
         CALL CHTRIM (STRNG, 6, STRNG, J)
         PHNAME(I+1:) = STRNG(:J)
         MSGTXT = 'Opening ' // PHNAME
         CALL MSGWRT (3)
         CALL ZTPOPN (FDVEC(1), IND, FDVEC(5), PHNAME, 'READ', JERR)
C                                       Save number of blocks in file
         IF (JERR.NE.0) THEN
            IERR = 8
            WRITE (MSGTXT,1010) JERR
            FDVEC(40) = 0
            GO TO 990
            END IF
         FDVEC(40) = IND
         END IF
C                                       Init previously inactive I/O
      FDVEC(42) = 0
      FDVEC(43) = 0
      NUMMSG = -2
C                                       Fill values in FTAB
C                                       No. logical rec. done
      FTAB(IND+12) = 0
C                                       LUN
      FTAB(IND) = FDVEC(1)
C                                       Number of bytes in a logical
C                                       record.
      FTAB(IND+2) = FDVEC(2)
C                                       Number of disk blocks per op.
      FTAB(IND+3) = 1
      FTAB(IND+13) = 0
C                                       Buffer size check
      FTAB(IND+4) = -1
C                                       Buffer too small
      IF (BUFSZ4.LT.13*2048) THEN
         IERR = 7
         I4TEMP = 13 * 2048
         WRITE (MSGTXT,1100) BUFSZ4, I4TEMP
         GO TO 990
         END IF
C                                       Next disk block
      RECNO = 1
      FTAB(IND+5) = RECNO
C                                       Opcode
      FTAB(IND+9) = 0
C                                       Medium type
      FTAB(IND+10) = 2
C                                       Set buffer pointer
      BUFPNT = 1
C                                       Check limited no. records
      FDVEC(41) = -1
C                                       Set I/O active flag
      FTAB(IND+13) = 1
C                                       Done if OP='OPxx'
      IF (OP(1:2).EQ.'OP') GO TO 999
C                                       READ
 500  JBUF = 1
      J = MIN (13, FDVEC(43))
      IF (J.LE.0) J = 13
      MBYTE = 2048 * J
      FTAB(IND+14) = MBYTE
      RECNO = FTAB(IND+5)
      FCBOFF = IND + MOFF
      CALL ZDKMID ('READ', RECNO, FTAB(FCBOFF), BUFF, MBYTE, IERR)
      IF (IERR.EQ.4) GO TO 998
      IF (IERR.NE.0) GO TO 980
C                                       Update block counter
      FTAB(IND+5) = RECNO + J
      FDVEC(43) = FDVEC(43) - J
C                                       Set pointers etc.
      IBIND = 1
      JBUF = 1
      CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, IERR)
      IF (IERR.EQ.4) GO TO 998
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1500) IERR, OP
         GO TO 990
         END IF
C                                       Check bytes read
      FDVEC(42) = LBYTES
      IF (LBYTES.NE.MBYTE) THEN
         I = LBYTES / 2048
         IF (I*2048.NE.LBYTES) GO TO 970
         CALL ZI16IL (2, 1, BUFF, MCB)
         CALL ZI32IL (1, 2, BUFF, LRW)
         LRW = 2 * LRW + 4 * MCB(2)
         LRW = (LRW - 1) / 2048 + 1
         IF ((MCB(1).NE.1) .OR. (LRW.GT.I)) GO TO 970
         IERR = 10
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close
 900  IERR = 8
      FDVEC(42) = 0
      IF ((IND.LE.0) .OR. (FTAB(IND).NE.FDVEC(1))) GO TO 999
      IERR = 0
C                                       Turn off things
      FTAB(IND+4) = 0
      FTAB(IND+12) = 0
      FTAB(IND+11) = 0
      FDVEC(40) = 0
C                                       Close file
      CALL ZTPCLS (FTAB(IND), IND, JERR)
      IF (IERR.EQ.0) IERR = JERR
      IF (IERR.NE.0) GO TO 980
      GO TO 998
C                                       I/O error
 970  WRITE (MSGTXT,1970) LBYTES
      IERR = 4
      GO TO 990
 980  WRITE (MSGTXT,1980) IERR, OP
      GO TO 990
C                                       disk IO error
 985  WRITE (MSGTXT,1985) FDVEC(2), FDVEC(31)
C                                       error messages
 990  CALL MSGWRT (6)
C                                       Mark I/O inactive
 998  FTAB(IND+13) = 0
C                                       Don't wait after error
      FTAB(IND+4) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLDKIO: OPCODE ',A4,' INVALID')
 1005 FORMAT (I6)
 1010 FORMAT ('FLDKIO: ERROR ',I3,' OPENING FILE')
 1100 FORMAT ('FLDKIO: BUFFER SIZE=',I6,' TOO SMALL, NEED',I6)
 1500 FORMAT ('FLDKIO: ERROR ',I3,2X,A4,'ING FILE')
 1970 FORMAT ('FLDKIO: LBYTES=',I8,' BUT IERR = 0')
 1980 FORMAT ('FLDKIO: ERROR',I3,' DOING I/O WITH OPCODE = ',A4)
 1985 FORMAT ('FLDKIO: INPUT BYTES, LENGTH TYPE=',I7,I3,
     *     ' SHOULD BE  2048, 0')
      END
