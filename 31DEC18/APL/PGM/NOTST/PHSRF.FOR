LOCAL INCLUDE 'PHSRF.INC'
C                                       Local include for PHSRF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XOPCOD
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XCHNS(4,20), BADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, NEWCNO, OLDCNO, CHNSEL(3,20,MAXIF)
      LOGICAL   ISCOMP, DOBLT, DOBOTH
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4
      DOUBLE PRECISION CATOD(128)
      EQUIVALENCE (CATOLD, CATOD)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC, XNAMOU,
     *   XCLAOU, XSOUT, XDISO, XOPCOD, XCHNS, BADD
      COMMON /PHSRFP/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, CHNSEL, OLDCNO, ISCOMP, DOBLT, DOBOTH,
     *   NEWCNO
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM PHSRF
C-----------------------------------------------------------------------
C! Compares RR and LL by dividing RR by LL.
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2008-2011, 2015-2016, 2018
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
C   PHSRF Compares RR and LL by dividing RR by LL.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'PHSRF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'PHSRF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL PHSRIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Process.
      CALL PHSUV (IRET)
      IF (IRET.NE.0) GO TO 990
      IF (.NOT.DOBLT) CALL PHSRHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE PHSRIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   PHSRIN gets input parameters for PHSRF and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in PHSRF for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'PHSRF.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, J, NW(MAXIF), K, K1, K2,
     *   LUN
      HOLLERITH CATH(256)
      LOGICAL   T, F, MATCH
      REAL      RPARM(20)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA BLANK  /' '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 251
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Decide on function
      DOBLT = F
      DOBOTH = F
      IF (OPCODE(1:4).EQ.'PHBL') DOBLT = T
      IF (OPCODE(1:3).EQ.'A&P') DOBOTH = T
      IF (OPCODE(1:4).EQ.'APBL') THEN
         DOBOTH = T
         DOBLT = T
         END IF
C                                       Average channel number
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Set up CHNSEL array
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 60 I = 1,20
         K = IROUND (XCHNS(2,I))
         IF (K.LE.0) GO TO 65
         K = IROUND (XCHNS(4,I))
         IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
            K1 = 1
            K2 = MAXIF
         ELSE
            K1 = K
            K2 = K
            END IF
         DO 55 K = K1,K2
            NW(K) = NW(K) + 1
            DO 50 J = 1,3
               CHNSEL(J,NW(K),K) = IROUND (XCHNS(J,I))
               IF (CHNSEL(J,NW(K),K).LT.0) CHNSEL(J,NW(K),K) = 0
 50            CONTINUE
            IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 55         CONTINUE
 60      CONTINUE
 65   J = CATBLK(KINAX+JLOCF)
      DO 75 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            IF ((JLOCIF.GE.0) .AND. (K.LE.CATBLK(KINAX+JLOCIF))) THEN
               WRITE (MSGTXT,1070)
               JERR = 1
               GO TO 990
               END IF
         ELSE
            DO 70 I = 1,NW(K)
               CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), J))
               IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = J
               CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), J))
 70            CONTINUE
            END IF
 75      CONTINUE
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Create output file.
      IF (.NOT.DOBLT) THEN
         CCNO = 1
         FRW(NCFILE+1) = 3
         JERR = 4
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 990
               END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
            IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = FRW(NCFILE) - 1
         NEWCNO = CCNO
C                                       copy keywords
         CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
         END IF
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PHSRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('PHSRIN: ERROR',I3,' UPDATING NEW CATBLK')
 1070 FORMAT ('PHSRIN: CHNSEL IS ZERO FOR SOME IF')
      END
      SUBROUTINE PHSUV (IRET)
C-----------------------------------------------------------------------
C   PHSUV sends uv data one point at a time to the division
C   routine and then writes the modified data if requested.
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, VO, KBIND, NIOUT, NIOLIM,
     *   IA1, IA2, BO, NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET
      LOGICAL   T, F
      INCLUDE 'PHSRF.INC'
      REAL      DUM, BASEN, VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NCORO = (LRECO - NRPRMO)
      IF (.NOT.ISCOMP) NCORO = NCORO / 3
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       Open vis file for write
      IF (.NOT.DOBLT) THEN
         CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
         CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         ILENBU = 0
         CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU,
     *      JBUFSZ, BUFF2, BO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         IPTRO = KBIND
         NIOUT = 0
         NIOLIM = ILENBU
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      IF (.NOT.DOBLT) THEN
         CALL RNXGET (DISKIN, OLDCNO, CATOLD)
         CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
         END IF
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         CALL PHREF (NUMVIS, RPARM(1+ILOCT), IA1, IA2, VIS, RPARM,
     *      RESULT, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            IF (.NOT.DOBLT) THEN
               CALL RNXUPD (RPARM, RNXRET)
               CALL RCOPY (NRPRMO, RPARM, BUFF2(IPTRO))
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1150) IRET
                     GO TO 990
                     END IF
                  IPTRO = KBIND
                  NIOUT = 0
                  END IF
               END IF
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Final call to DIVRL.
      NUMVIS = -1
      CALL PHREF (NUMVIS, DUM, IA1, IA2, BUFF1, BUFF1, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      IF (.NOT.DOBLT) THEN
         NIOUT = - NIOUT
         CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            GO TO 990
            END IF
C                                       Compress output file.
         NVIS = XCOUNT
         CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       close NX table, output
         CALL RNXCLS (RNXRET)
         CALL ZCLOSE (LUNO, INDO, IRET)
         IF (RNXRET.NE.0) THEN
            MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Close input
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PHSUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('PHSUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('PHSUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('PHSUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('PHSUV: PHREF ERROR',I3)
 1150 FORMAT ('PHSUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE PHSRHI
C-----------------------------------------------------------------------
C   PHSRHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, J, K, NIF
      LOGICAL   T
      INCLUDE 'PHSRF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF (JLOCIF.GT.0) THEN
         NIF = CATOLD(KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration adverbs
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Channel selection
      DO 65 K = 1,NIF
         DO 60 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,3050) TSKNAM, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 60         CONTINUE
 65      CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'PHSRHI: ERROR COPYING TABLES TO OUTPUT'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PHSRHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 3050 FORMAT (A6,'/ Ref Ch Avgd: Start, Stop, Inc ',2I5,I4,'  IF=',I3)
      END
      SUBROUTINE PHREF (NUMVIS, T, IA1, IA2, VIS, RPARM, RESULT, IRET)
C-----------------------------------------------------------------------
C   Phase-reference
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days since 0 IAT on the reference day.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      LRECI      I    Input file record length
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   Output:
C      T          R    Time in same units as input.
C      RPARM      R    Modified random parameter array.
C      RESULT  R(3,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMVIS, IA1, IA2, IRET
      REAL      T, VIS(3,*), RPARM(*), RESULT(3,*)
C
      INTEGER  NUMPOL, NUMIF, NUMF, LOOPIF, LOOPF, INDEX, ONDEX, JNDEX,
     *   JNCS, JNCIF, LOOPS, LLINCF, IDUM, WORK(512), NUMANS(513),
     *   IROUND
      REAL     VISREF(12*MAXIF), T1, T2, TW, PHS, AMP
      CHARACTER CTEMP*12, UTYPE*2, STAT*4
      LOGICAL   CHSTAT, BADTAN
C                                       BL table information
      INTEGER BLKOLS(MAXBLC), BLNUMV(MAXBLC), IBLRNO, BLVER, BLLUN,
     *   BLBUFF(1024), ISRC, ISUBA, IFQID, NN, NUMTEL
      REAL    FACADD(2,2,MAXIF), FACMUL(2,2,MAXIF), T3, T4
C
      INCLUDE 'PHSRF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE NUMPOL, NUMIF, NUMF, JNCS, JNCIF, BLBUFF, IBLRNO, BLNUMV,
     *   BLKOLS, CHSTAT
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LE.0) GO TO 800
C                                       Setup on first call
      IF (NUMVIS.EQ.1) THEN
         NUMPOL = CATOLD(KINAX+JLOCS)
         NUMF = CATOLD(KINAX+JLOCF)
         IF (JLOCIF.GT.0) THEN
            NUMIF = CATOLD(KINAX+JLOCIF)
         ELSE
            NUMIF = 1
            END IF
C                                       Set output increments
C                                       (averaging)
         JNCIF = INCIF
         IF (JLOCF.LT.JLOCIF) JNCIF = INCIF / NUMF
         JNCS = INCS
         IF (JLOCF.LT.JLOCS) JNCS = INCS / NUMF
C                                       Check that data OK - must have
C                                       > 1 freq.
         IF (NUMF.LT.2) THEN
            IRET = 9
            MSGTXT = 'YOUR DATA IS INCOMPATIBLE WITH MY MISSION'
            CALL MSGWRT (9)
            GO TO 999
            END IF
C                                       Set up for Sn table entry
         IF (DOBLT) THEN
C                                       Change status to 'writ'
C                                       Determine status of file
            UTYPE = 'UV'
            CHSTAT = .FALSE.
            CALL CATDIR ('INFO', DISKIN, OLDCNO, CTEMP, CTEMP, IDUM,
     *         UTYPE, IDUM, STAT, WORK, IRET)
            IF (STAT.EQ.'READ') THEN
C                                       Change status
               CALL STATCH ('READ', 'WRIT', DISKIN, OLDCNO, UTYPE,
     *            WORK, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT'
                  GO TO 990
                  END IF
               CHSTAT = .TRUE.
               END IF
C
            BLLUN=27
            CALL GETNAN (DISKIN, OLDCNO, CATOLD, BLLUN, VISREF,
     *         NUMANS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET
               GO TO 990
               END IF
            NUMTEL = NUMANS(2)
C                                       Create BL table
            BLLUN = 48
            BLVER = 0
            CALL BLINI ('WRIT', BLBUFF, DISKIN, OLDCNO, BLVER, CATOLD,
     *         BLLUN, IBLRNO, BLKOLS, BLNUMV, NUMTEL, NUMPOL, NUMIF,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            WRITE (MSGTXT,1030) BLVER
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Determine phase of
C                                       reference channel
      LLINCF = INCF
      IF (INCF.EQ.1) INCF = INCF * 3
      CALL AVGCHN (VIS, NUMPOL, 1, NUMF, 1, NUMIF, CHNSEL, JNCS, JNCIF,
     *   VISREF)
      INCF = LLINCF
C                                       If write BL table then
      IF (DOBLT) THEN
         ISRC = 0
         IF (ILOCSU.GE.0) ISRC = IROUND(RPARM(1+ILOCSU))
         IFQID = -1
         IF (ILOCFQ.GE.0) IFQID = IROUND(RPARM(1+ILOCFQ))
         IF (ILOCB.GE.0) THEN
            ISUBA = RPARM(ILOCB+1) + 0.1
            ISUBA = 1.5 + 100.0 * (RPARM(ILOCB+1) - ISUBA)
         ELSE
            ISUBA = RPARM(ILOCSA+1) + 0.1
            END IF
         NN = NUMIF * 4
         CALL RCOPY (NN, 0.0, FACADD)
         CALL RCOPY (NN, 0.0, FACMUL)
         END IF
C                                       Divide
      DO 700 LOOPS = 1,NUMPOL
         DO 500 LOOPIF = 1,NUMIF
            JNDEX = (LOOPS-1) * JNCS + (LOOPIF-1) * JNCIF
            T1 = VISREF(1+JNDEX)
            T2 = VISREF(2+JNDEX)
            TW = VISREF(3+JNDEX)
            IF ((ABS(T1).LT.1.0E-25) .AND. (ABS(T2).LT.1.0E-25)) THEN
               BADTAN = .TRUE.
               PHS =0.
            ELSE IF (TW.LE.0.0) THEN
               BADTAN = .TRUE.
               PHS =0.
            ELSE
               BADTAN = .FALSE.
               PHS = ATAN2 (T2, T1)
               END IF
            IF (DOBOTH) THEN
               AMP = SQRT (T1*T1 + T2*T2)
            ELSE
               AMP = 1.0
               END IF
            T1 = 1.0 * COS (PHS)
            T2 = 1.0 * SIN (PHS)
            IF (AMP.GT.1.0E-25) THEN
               T3 = 1.0/AMP * COS (-PHS)
               T4 = 1.0/AMP * SIN (-PHS)
            ELSE
               T3 = FBLANK
               T4 = FBLANK
               END IF
            FACMUL(1,LOOPS,LOOPIF) = T3
            FACMUL(2,LOOPS,LOOPIF) = T4
            IF (.NOT.DOBLT) THEN
               DO 300 LOOPF = 1,NUMF
                  INDEX = 1 + (LOOPS-1) * INCSI + (LOOPIF-1) * INCIFI +
     *               (LOOPF-1) * INCFI
                  ONDEX = 1 + (LOOPS-1) * INCSO + (LOOPIF-1) * INCIFO +
     *               (LOOPF-1) * INCFO
                  IF ((VIS(3,INDEX).GT.1.0E-25) .AND. (AMP.GT.1.0E-25)
     *               .AND. ( .NOT. BADTAN)) THEN
                     RESULT(1,ONDEX) = (VIS(1,INDEX) * T1 +
     *                  VIS(2,INDEX) * T2)/AMP
                     RESULT(2,ONDEX) = (VIS(2,INDEX) * T1 -
     *                  VIS(1,INDEX) * T2)/AMP
                     RESULT(3,ONDEX) = VIS(3,INDEX) * AMP * AMP
                  ELSE
C                                       Bad vis - flag
                     RESULT(1,ONDEX) = 0.0
                     RESULT(2,ONDEX) = 0.0
                     RESULT(3,ONDEX) = 0.0
                     END IF
 300              CONTINUE
               END IF
 500        CONTINUE
 700     CONTINUE
C                                       Write BL entry
      IF (DOBLT) THEN
         CALL TABBL ('WRIT', BLBUFF, IBLRNO, BLKOLS, BLNUMV, NUMPOL,
     *      T, ISRC, ISUBA, IA1, IA2, IFQID, FACMUL, FACADD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Clear up
 800  CONTINUE
      IF (DOBLT) THEN
         CALL TABBL ('CLOS', BLBUFF, IBLRNO, BLKOLS, BLNUMV, NUMPOL,
     *      T, ISRC, ISUBA, IA1, IA2, IFQID, FACMUL, FACADD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
         CALL CATIO ('UPDT', DISKIN, OLDCNO, CATOLD, 'REST', WORK,
     *      IRET)
C                                       Check if changed status
         IF (CHSTAT) THEN
            UTYPE = 'UV'
            CALL STATCH ('WRIT', 'READ', DISKIN, OLDCNO, UTYPE, WORK,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ'
               GO TO 990
               END IF
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PHREF: ERROR ',I3,' CHANGING ',A4,' STATUS')
 1010 FORMAT ('PHREF: ERROR ',I3,' FINDING # ANTENNAS')
 1020 FORMAT ('PHREF: ERROR ',I3,' OPENING BL TABLE')
 1030 FORMAT ('Writing to BL table ',I3)
 1040 FORMAT ('PHREF: ERROR ',I3,' WRITING TO BL TABLE')
 1050 FORMAT ('PHREF: ERROR ',I3,' CLOSING BL TABLE')
      END

