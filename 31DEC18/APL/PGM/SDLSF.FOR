LOCAL INCLUDE 'SDLSF.INC'
C                                       Local include for SDLSF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, DOOUT, XCHSEL(3,10),
     *   XORD, DOALL, XCHANF, MFLUX, RFLUX, XPRT, BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), BUFF3(UVBFSS), AVGB(3,MAXCIF)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, CHNSEL(3,10), NCHSEL,
     *   CHFIT0, DISK2, SEQO2, CAT2(256), INCS2, INCF2, INCIF2, LREC2,
     *   NRPRM2, CCN2, CHMASK(MAXCHA), NCORI, NCORO, NCOPY, NCOR2,
     *   NCOP2, NAVG, PRTLEV, NORDER
      LOGICAL   ISCOMP, DOFLUX
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XCHSEL, XORD, DOALL, DOOUT, XCHANF, MFLUX,
     *   RFLUX, XPRT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, AVGB, JBUFSZ
      COMMON /INFO/ CATOLD, CAT2, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, CHNSEL, NCHSEL, CHFIT0,
     *   DISK2, SEQO2, CCN2, INCS2, INCF2, INCIF2, LREC2, NRPRM2,
     *   CHMASK, DOFLUX, NCORI, NCORO, NCOPY, NCOR2, NCOP2, NAVG,
     *   PRTLEV, NORDER
LOCAL END
      PROGRAM SDLSF
C-----------------------------------------------------------------------
C! Averages several channels and "antennas" and subtracts from uv data.
C# Utility UV UV-util SINGLEDISH SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 1999-2000, 2008, 2015
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
C   SDLSF averages a set of channels and subtracts them from another
C   range of channels.  It can average all antennas at a given time, fit
C   that average, and subtract that fit from each.
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
C      CHANSEL(3,10)  CHNSEL        Channels to select for baseline
C      DOOUTPUT       DOOUT         > 0 => write fit uv data also
C      CHANNEL        CHFIT0        first fit channel out
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'SDLSF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'SDLSF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SDLSFI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Filter data.
      CALL SDLSFU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL SDLSFH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SDLSFI (PRGN, JERR)
C-----------------------------------------------------------------------
C   SDLSFI gets input parameters for SDLSF and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      ISCOMP  L  If true data is compressed
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
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, NFREQ, I, CATMP(256),
     *   J
      LOGICAL   T
      INCLUDE 'SDLSF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 51
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      DISK2 = DISKO
      SEQO2 = SEQOUT
      DOFLUX = (MFLUX.GT.0.0) .OR. (RFLUX.GT.0.0)
      IF (MFLUX.LE.0.0) MFLUX = 1.E10
      IF (RFLUX.LE.0.0) RFLUX = 1.E10
      PRTLEV = IROUND (XPRT)
      NORDER = XORD + 0.02
      NORDER = MAX (0, MIN (1, NORDER))
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
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Channel selection?
      NFREQ = CATBLK(KINAX+JLOCF)
      CHFIT0 = IROUND (XCHANF)
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 = IROUND
     *   (CATR(KRCRP+JLOCF))
      IF ((CHFIT0.LE.0) .OR. (CHFIT0.GT.NFREQ)) CHFIT0 = MAX (1,
     *   NFREQ/2)
      IF (DOOUT.GT.0.0) THEN
         WRITE (MSGTXT,1005) CHFIT0
         CALL MSGWRT (3)
         END IF
C
      CALL FILL (30, 0, CHNSEL)
      NCHSEL = 0
C
      DO 10 I = 1,10
         CHNSEL(1,I) = XCHSEL(1,I) + 0.5
         CHNSEL(2,I) = XCHSEL(2,I) + 0.5
         CHNSEL(3,I) = XCHSEL(3,I) + 0.5
C
         IF ((CHNSEL(1,I).GT.0) .OR. (CHNSEL(2,I).GT.0)) THEN
            NCHSEL = NCHSEL + 1
            IF ((CHNSEL(1,I).GT.NFREQ) .OR. (CHNSEL(1,I).LT.1))
     *         CHNSEL(1,I) = 1
            IF ((CHNSEL(2,I).LT.CHNSEL(1,I)) .OR.
     *         (CHNSEL(2,I).GT.NFREQ)) CHNSEL(2,I) = NFREQ
C
            IF ((CHNSEL(3,I).LE.0) .OR.
     *         (CHNSEL(3,I).GT.(CHNSEL(2,I)-CHNSEL(1,I)+1)))
     *         CHNSEL(3,I) = 1
         ELSE
            IF (I.EQ.1) THEN
               NCHSEL = 1
               CHNSEL(1,I) = 1
               CHNSEL(2,I) = NFREQ
               CHNSEL(3,I) = 1
               END IF
            GO TO 20
            END IF
 10      CONTINUE
C                                       Make mask
 20   CALL FILL (MAXCHA, 0, CHMASK)
      DO 25 J = 1,NCHSEL
         DO 24 I = CHNSEL(1,J),CHNSEL(2,J),CHNSEL(3,J)
            CHMASK(I) = 1
 24         CONTINUE
 25      CONTINUE

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
C                                       Create output file.
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
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
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                       second output file
      IF (DOOUT.GT.0.0) THEN
         CALL COPY (256, CATBLK, CATMP)
         CALL COPY (256, CATOLD, CATBLK)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, 'BASFIT', KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO2
C                                       Frequency axis
         CATBLK(KINAX+JLOCF) = 1
         CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - CHFIT0 + 1
         CATR(KRARP) = CATR(KRARP) - CHFIT0 + 1
C                                       Create output file.
         CCN2 = 1
         FRW(NCFILE+1) = 3
         JERR = 4
         CALL UVCREA (DISK2, CCN2, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK2
         FCNO(NCFILE) = CCN2
         FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
         INCX = CATBLK(KINAX)
         LREC2 = LREC
         NRPRM2 = NRPARM
         INCS2 = INCS / INCX
         INCF2 = INCF / INCX
         INCIF2 = INCIF / INCX
         JERR = 0
C                                       copy keywords
         CALL KEYCOP (DISKIN, OLDCNO, DISK2, CCN2, IERR)
C                                       save catblk
         SEQO2 = CATBLK(KIIMS)
         CALL COPY (256, CATBLK, CAT2)
         CALL COPY (256, CATMP, CATBLK)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDLSFI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1005 FORMAT ('Continuum written at channel =',I6)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('SDLSFI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SDLSFU (IRET)
C-----------------------------------------------------------------------
C   SDLSFU sends uv data one point at a time to the filtering
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
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LUN2, IND2,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, VCOUNT, IPTR2, NOUT, II, IR, NIOU2, NIOLI2
      LOGICAL   T, F, LAST
      INCLUDE 'SDLSF.INC'
      INTEGER   MXA
      PARAMETER (MXA = 10)
      REAL      BASEN, CBUFF(UVBFSS), RESULT(3*MXA*MAXCIF),
     *   RESUL2(3*4*MAXIF*MXA), RRPARM(14*MXA)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO, LUN2 /16, 17, 18/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = NCORO * INCX
      NCOR2 = (LREC2 - NRPRM2) / CATBLK(KINAX)
      NCOP2 = NCOR2 * INCX
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(2), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Open vis file for write
      IF (DOOUT.GT.0.0) THEN
         CALL ZPHFIL ('UV', DISK2, CCN2, 1, OFILE, IRET)
         CALL ZOPEN (LUN2, IND2, DISK2, OFILE, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
         ILENBU = 0
         CALL UVINIT ('WRIT', LUN2, IND2, NVIS, VO, LREC2, ILENBU,
     *      JBUFSZ, BUFF3, BO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         IPTR2 = KBIND
         NIOU2 = 0
         NIOLI2 = ILENBU
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      VCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         LAST = INIO.LE.0
         INIO = MAX (INIO, 1)
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (LAST) THEN
               NUMVIS = -1
            ELSE
               IF (ILOCB.GE.0) THEN
                  BASEN = BUFF1(IPTRI+ILOCB)
                  IA1 = BASEN / 256. + 0.1
                  IA2 = BASEN - IA1*256. + 0.1
               ELSE
                  IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
                  IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
                  END IF
               NUMVIS = NUMVIS + 1
               END IF
C                                       Call filtering routine.
C                                       Compressed data.
            IF ((ISCOMP) .AND. (.NOT.LAST)) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL SDLSFF (NUMVIS, BUFF1(IPTRI+ILOCT), CBUFF,
     *            BUFF1(IPTRI), INCX, RRPARM, RESULT, RESUL2, IRET)
C                                       Un compressed data
            ELSE
               CALL SDLSFF (NUMVIS, BUFF1(IPTRI+ILOCT),
     *            BUFF1(IPTRI+NRPRMI), BUFF1(IPTRI), INCX, RRPARM,
     *            RESULT, RESUL2, IRET)
               END IF
C                                       Branch on his return
            IF (IRET) 130, 140, 120
C                                       Error (fatal)
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
 130        NOUT = -IRET
            DO 135 II = 1,NOUT
               VCOUNT = VCOUNT + 1
               IR = (II - 1) * NRPRMO + 1
               CALL RCOPY (NRPRMO, RRPARM(IR), BUFF2(IPTRO))
               IR = (II - 1) * NCOPY + 1
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT(IR), BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT(IR), BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               IF (DOOUT.GT.0.0) THEN
                  IR = (II - 1) * NRPRMO + 1
                  CALL RCOPY (NRPRM2, RRPARM(IR), BUFF3(IPTR2))
                  IR = (II - 1) * NCOP2 + 1
                  IF (ISCOMP) THEN
                     CALL ZUVPAK (NCOR2, RESUL2(IR),
     *                  BUFF3(IPTR2+ILOCWT), BUFF3(IPTR2+NRPRM2))
                  ELSE
                     CALL RCOPY (NCOP2, RESUL2(IR), BUFF3(IPTR2+NRPRM2))
                     END IF
                  IPTR2 = IPTR2 + LREC2
                  NIOU2 = NIOU2 + 1
                  END IF
C                                       Write vis record.
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1150) IRET, VCOUNT
                     GO TO 990
                     END IF
                  IPTRO = KBIND
                  NIOUT = 0
                  END IF
C                                       Write vis record.
               IF ((DOOUT.GT.0.0) .AND. (NIOU2.GE.NIOLI2)) THEN
                  CALL UVDISK ('WRIT', LUN2, IND2, BUFF3, NIOLI2, KBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1150) IRET, VCOUNT
                     GO TO 990
                     END IF
                  IPTR2 = KBIND
                  NIOU2 = 0
                  END IF
 135           CONTINUE
C                                       OK, but no output please
 140        IPTRI = IPTRI + LRECI
 190        CONTINUE
C                                       Read next buffer.
         IF (.NOT.LAST) GO TO 100
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET, VCOUNT
         GO TO 990
         END IF
C                                       Finish write
      IF (DOOUT.GT.0.0) THEN
         NIOU2 = - NIOU2
         CALL UVDISK ('FLSH', LUN2, IND2, BUFF3, NIOU2, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET, VCOUNT
            GO TO 990
            END IF
         END IF
C                                       Compress output file.
      NVIS = VCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
      IF (DOOUT.GT.0.0) CALL UCMPRS (NVIS, DISK2, CCN2, LUN2, CAT2,
     *   IRET)
C                                      Put vis. count in CATBLK
C      CAT2(KIGCN) = NVIS
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (DOOUT.GT.0.0) CALL ZCLOSE (LUN2, IND2, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDLSFU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('SDLSFU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SDLSFU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SDLSFU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SDLSFU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SDLSFU: SDLSFF ERROR',I3)
 1150 FORMAT ('SDLSFU: ERROR',I3,' WRITING VIS FILE AT VIS',I9)
      END
      SUBROUTINE SDLSFH
C-----------------------------------------------------------------------
C   SDLSFH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'SDLSF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(2), FCNO(1), CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                      CHANSEL
      DO 20 I = 1,NCHSEL
         WRITE (HILINE,2020) TSKNAM, I, CHNSEL(1,I), CHNSEL(2,I),
     *      CHNSEL(3,I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
 20      CONTINUE
C                                       ORDER
      WRITE (HILINE,2025) TSKNAM, NORDER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
      IF (DOOUT.GT.0.0) THEN
         CALL COPY (256, CAT2, CATBLK)
C                                       Copy/open history file.
         CALL HISCOP (LUN1, LUN2, DISKIN, DISK2, FCNO(2), FCNO(3),
     *      CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 200
            END IF
C                                       New history
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 200
         CALL HENCOO (TSKNAM, NAMOUT, 'BASFIT', SEQO2, DISK2, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       FIT CHANNEL
         WRITE (HILINE,2010) TSKNAM, CHFIT0
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                      CHANSEL
         DO 120 I = 1,NCHSEL
            WRITE (HILINE,2020) TSKNAM, I, CHNSEL(1,I), CHNSEL(2,I),
     *         CHNSEL(3,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 120        CONTINUE
C                                       ORDER
         WRITE (HILINE,2025) TSKNAM, NORDER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 200     CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISK2,
     *      FCNO(2), FCNO(3), CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1200)
            CALL MSGWRT (6)
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISK2, FCNO(3), CATBLK, 'REST',
     *      BUFF1, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDLSFH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1200 FORMAT ('SDLSFH: ERROR COPYING TABLES')
 2010 FORMAT (A6,'CHANNEL =',I5,5X,'/ Channel at which fit evaluated')
 2020 FORMAT (A6,'CHANSEL(',I2,')=',I5,',',I5,',',I2,5X,
     *   '/ channels fit')
 2025 FORMAT (A6,'ORDER =',I3,10X,'/ order of polynomial fit')
      END
      SUBROUTINE SDLSFF (NUMVIS, T, VIS, RPARM, INCX, RRPARM, RESULT,
     *   RESUL2, IRET)
C-----------------------------------------------------------------------
C   Routine to fit baseline to channels, subtract and perhaps flag
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days.
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      CHNSEL(3,10) Gives channel selection
C      LRECI   I    Input file record length
C      NRPRMI  I    Input number of random parameters.
C      INCSI   I    Input Stokes' increment in vis.
C      INCFI   I    Input frequency increment in vis.
C      INCIFI  I    Input IF increment in vis.
C      LRECO   I    Output file record length
C      NRPRMO  I    Output number of random parameters.
C      INCSO   I    Output Stokes' increment in vis.
C      INCFO   I    Output frequency increment in vis.
C      INCIFO  I    Output IF increment in vis.
C   Output:
C      RRPARM  R(*)   Output random parameters
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      RESUL2  R(INCX,*) Baseline fit value at CHFIT0
C      IRET    I  Return code   0 => don't write
C                              -N => OK, write N output records
C                              >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      T, VIS(INCX,*), RPARM(*), RESULT(INCX,*),
     *   RESUL2(INCX,*), RRPARM(*)
C
      INTEGER   NFREQ, NIF, NPOLN, INDEX, OFF, IS, IIF, INDEX2, NFIT,
     *   ICHAN, NSX, NFLUXF, IR, NFLUXR, NFLUXP, MNAVG
      REAL      SX, SXX, RSY, RSXY, RM, RB, SSX, MSX, PTIME, DELTAT,
     *   HOLDP(14), MFLUX2
      LOGICAL   FLGWT, PRTHDR
      INCLUDE 'SDLSF.INC'
      INTEGER   FLAGD(10*MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE NPOLN, NFREQ, NIF, NFLUXF, PTIME, DELTAT, HOLDP, FLAGD,
     *   NFLUXR, NFLUXP, MNAVG
C-----------------------------------------------------------------------
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         NAVG = 0
         MNAVG = 0
         PTIME = -999.
         DELTAT = 0.03 / (24.0 * 3600.0)
         IF (DOALL.LE.0.0) DELTAT = -DELTAT
         NFLUXF = 0
         NFLUXR = 0
         NFLUXP = 0
         NPOLN = CATBLK(KINAX+JLOCS)
         NFREQ = CATBLK(KINAX+JLOCF)
         IF (JLOCIF.GT.0) THEN
            NIF = CATBLK(KINAX+JLOCIF)
         ELSE
            NIF = 1
            END IF
         CALL RFILL (3*MAXCIF, 0.0, AVGB)
         CALL FILL (10*MAXCIF, 0, FLAGD)
         END IF
C                                       Average data out
      IRET = 0
      MFLUX2 = MFLUX * MFLUX
      IF (((NUMVIS.LE.0) .OR. (ABS(T-PTIME).GT.DELTAT)) .AND.
     *   (NAVG.GT.0)) THEN
         IF (NAVG.EQ.1) THEN
            CALL RCOPY (NRPARM, HOLDP, RRPARM)
            CALL RCOPY (NCOPY, AVGB, RESULT)
            END IF
         IRET = -NAVG
         MNAVG = MAX (MNAVG, NAVG)
C                                       Loop over IF
         DO 90 IIF = 1,NIF
C                                       Loop over Stokes
            DO 80 IS = 1,NPOLN
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
C                                       Average up the data
               INDEX = OFF
               DO 20 ICHAN = 1,NFREQ
                  AVGB(1,INDEX) = 0.0
                  AVGB(2,INDEX) = 0.0
                  AVGB(3,INDEX) = 0.0
                  DO 10 IR = 1,NAVG
                     INDEX2 = INDEX + (IR-1) * NCORO
                     IF (RESULT(3,INDEX2).GT.0.0) THEN
                        AVGB(1,INDEX) = AVGB(1,INDEX) + RESULT(1,INDEX2)
     *                     * RESULT(3,INDEX2)
                        AVGB(3,INDEX) = AVGB(3,INDEX) + RESULT(3,INDEX2)
                        END IF
 10                  CONTINUE
                  IF (AVGB(3,INDEX).GT.0.0) AVGB(1,INDEX) =
     *               AVGB(1,INDEX) / AVGB(3,INDEX)
                  INDEX = INDEX + INCFO
 20               CONTINUE
C                                       Least squares fit to real and
C                                       imaginary parts of selected
C                                       channels
               SX = 0.0
               RSY = 0.0
               RSXY = 0.0
               SXX = 0.0
               NFIT = 0
C
               DO 30 ICHAN = 1,NFREQ
                  IF (CHMASK(ICHAN).GT.0) THEN
                     INDEX = OFF + (ICHAN-1) * INCFI
C                                       Compute sums
                     IF (AVGB(3,INDEX).GT.0.0) THEN
                        RSY = RSY + AVGB(1,INDEX)
                        NFIT = NFIT + 1
                        IF (NORDER.GT.0) THEN
                           RSXY = RSXY + ICHAN*AVGB(1,INDEX)
                           SX = SX + ICHAN
                           SXX = SXX + ICHAN*ICHAN
                           END IF
                        END IF
                     END IF
 30               CONTINUE
C                                       Compute m & b; y = mx + b
C                                       Real and imaginary separately
               RM = 0.0
               IF (NFIT.GT.1) THEN
                  IF (NORDER.GT.0) THEN
                     RM = (NFIT * RSXY) - (SX * RSY)
                     RM = RM / (NFIT * SXX - SX**2)
                     END IF
                  RB = (RSY / NFIT) - (RM * SX / NFIT)
                  FLGWT = .FALSE.
               ELSE
                  RB = 0.0
                  FLGWT = .TRUE.
                  END IF
C                                       Subtract baseline from input
C                                       loop over outputs
               DO 70 IR = 1,NAVG
                  INDEX =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
                  INDEX2 = INDEX + (IR - 1) * NCORO
                  SSX = 0.0
                  MSX = 0.0
C
      INCLUDE 'INCS:ZVND.INC'
                  DO 50 ICHAN = 1,NFREQ
C                                       Subtract from vis.
                     RESULT(1,INDEX2) = RESULT(1,INDEX2) -
     *                  (RB + RM*ICHAN)
                     RESULT(2,INDEX2) = RESULT(2,INDEX2) +
     *                  (RB + RM*ICHAN)
                     IF (FLGWT) RESULT(3,INDEX2) =
     *                  - ABS (RESULT(3,INDEX2))
                     IF ((DOFLUX) .AND. (RESULT(3,INDEX2).GT.0.0) .AND.
     *                  (CHMASK(ICHAN).GT.0)) THEN
                        SX = RESULT(1,INDEX2) * RESULT(1,INDEX2)
                        SSX = SSX + SX
                        MSX = MAX (MSX, SX)
                        IF (SX.GT.MFLUX2) FLAGD(INDEX2) = FLAGD(INDEX2)
     *                     + 1
                        NSX = NSX + 1
                        END IF
                     INDEX2 = INDEX2 + INCFO
 50                  CONTINUE
                  IF (NSX.GT.0) THEN
                     SSX = SQRT (SSX / NSX)
                     MSX = SQRT (MSX)
                     IF ((SSX.GT.RFLUX) .OR. (MSX.GT.MFLUX)) THEN
                        NFLUXF = NFLUXF + 1
                        IF (SSX.GT.RFLUX) NFLUXR = NFLUXR + 1
                        IF (MSX.GT.MFLUX) NFLUXP = NFLUXP + 1
                        INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO
     *                     + 1 + (IR - 1) * NCORO
                        DO 60 ICHAN = 1,NFREQ
                           RESULT(3,INDEX2) = - ABS (RESULT(3,INDEX2))
                           INDEX2 = INDEX2 + INCFO
 60                        CONTINUE
                        FLGWT = .TRUE.
                        END IF
                     END IF
                  IF (DOOUT.GT.0.0) THEN
                     INDEX = OFF + (CHFIT0 - 1) * INCFI + (IR-1)*NCORO
                     INDEX2 =  (IS - 1) * INCS2 + (IIF - 1) * INCIF2 + 1
     *                  + (IR - 1) * NCOR2
                     RESUL2(1,INDEX2) = (RB + RM*CHFIT0)
                     RESUL2(2,INDEX2) = RESUL2(1,INDEX2)
                     RESUL2(3,INDEX2) = RESULT(3,INDEX)
                     IF (FLGWT) RESUL2(3,INDEX2) =
     *                  - ABS (RESUL2(3,INDEX2))
                     END IF
 70               CONTINUE
 80            CONTINUE
 90         CONTINUE
C                                       Zero sum count
         NAVG = 0
         END IF
C                                       last call
      IF (NUMVIS.LE.0) THEN
         IF (DOFLUX) THEN
            IF (NFLUXF.LE.0) THEN
               MSGTXT = 'No points flagged for excess residuals'
               CALL MSGWRT (5)
            ELSE
               WRITE (MSGTXT,1100) NFLUXF
               CALL MSGWRT (5)
               WRITE (MSGTXT,1101) NFLUXR, RFLUX
               CALL MSGWRT (5)
               WRITE (MSGTXT,1102) NFLUXP, MFLUX
               CALL MSGWRT (5)
               IF (PRTLEV.GT.0) THEN
                  DO 140 IR = 1,MNAVG
                     PRTHDR = .TRUE.
                     DO 130 IIF = 1,NIF
                        DO 120 IS = 1,NPOLN
                           OFF = (IS - 1) * INCSO + (IIF - 1) * INCIFO +
     *                        1 + (IR - 1) * NCORO
                           INDEX = OFF
                           IF ((NIF.GT.1) .OR. (NPOLN.GT.1)) THEN
                              DO 100 ICHAN = 1,NFREQ
                                 IF (FLAGD(INDEX).GE.PRTLEV) THEN
                                    IF (PRTHDR) THEN
                                       WRITE (MSGTXT,1104) IR
                                       CALL MSGWRT (4)
                                       PRTHDR = .FALSE.
                                       END IF
                                    WRITE (MSGTXT,1105) ICHAN, IIF, IS,
     *                                 FLAGD(INDEX)
                                    CALL MSGWRT (4)
                                    END IF
                                 INDEX = INDEX + 1
 100                             CONTINUE
                           ELSE
                              DO 110 ICHAN = 1,NFREQ
                                 IF (FLAGD(INDEX).GE.PRTLEV) THEN
                                    IF (PRTHDR) THEN
                                       WRITE (MSGTXT,1104) IR
                                       CALL MSGWRT (4)
                                       PRTHDR = .FALSE.
                                       END IF
                                    WRITE (MSGTXT,1106) ICHAN,
     *                                 FLAGD(INDEX)
                                    CALL MSGWRT (4)
                                    END IF
                                 INDEX = INDEX + 1
 110                             CONTINUE
                              END IF
 120                       CONTINUE
 130                    CONTINUE
 140                 CONTINUE
                  END IF
               END IF
            END IF
C                                       move data
      ELSE
         IF (NAVG.EQ.0) THEN
            CALL RCOPY (NRPARM, RPARM, HOLDP)
            CALL RCOPY (NCOPY, VIS, AVGB)
            PTIME = T
         ELSE
            IF (NAVG.EQ.1) THEN
               CALL RCOPY (NRPARM, HOLDP, RRPARM)
               CALL RCOPY (NCOPY, AVGB, RESULT)
               END IF
            IR = NAVG * NCORO + 1
            CALL RCOPY (NCOPY, VIS, RESULT(1,IR))
            IR = NAVG * NRPARM + 1
            CALL RCOPY (NRPARM, RPARM, RRPARM(IR))
            END IF
         NAVG = NAVG + 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Flagged',I9,' TOTAL time/IF/Stokes samples')
 1101 FORMAT (7X,I9,' for excessive RMS >',F6.1)
 1102 FORMAT (7X,I9,' for excessive abs(residual) >',F6.1)
 1104 FORMAT ('******* BEAM',I3,' *******')
 1105 FORMAT ('Channel',I5,' IF',I3,' Stokes',I2,' had',I9,
     *   ' excess residuals')
 1106 FORMAT ('Channel',I5,' had',I9,' excess residuals')
      END
