LOCAL INCLUDE 'UJOIN.INC'
C                                       Local include for UJOIN
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCOD
      REAL      XSIN, XDISIN, XSOUT, XDISO, XCHSEL(3,10), DOWT,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, CHNSEL(3,10)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XCHSEL, XOPCOD, DOWT,
     *   SEQIN, SEQOUT, DISKIN, DISKO, ILOCWT, CATOLD, INCSI,
     *   INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO, NRPRMI,
     *   NRPRMO, ISCOMP, CHNSEL
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for UJOIN
LOCAL END
      PROGRAM UJOIN
C-----------------------------------------------------------------------
C! Joins 2 IFs into one spectrum
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1999-2000, 2004, 2007-2013, 2017
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
C   UJOIN joines 2 IFs into 1 spectrum
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
C      CHANSEL        CHNSEL        1st, last input, 1st output channel
C                                   for each IF
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UJOIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UJOIN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file
      CALL UJOINI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL UJOINU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UJOINH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UJOINI (PRGN, IRET)
C-----------------------------------------------------------------------
C   UJOINI gets input parameters for UJOIN and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
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
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'UJOIN.INC'
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
      IRET = 0
C                                       Get input parameters.
      NPARM = 46
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
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
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Plan overlap, modify header
      CALL FIXHDR (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      IRET = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UJOINI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE FIXHDR (IERR)
C-----------------------------------------------------------------------
C   Gets frequencies, plans overlap, alters CATBLK for output
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   NCH, NIF, NOUT, I, IROUND, NP, IP, J, L, JIF
      CHARACTER STRING*12
      INCLUDE 'INCS:PUVD.INC'
      REAL      FINC(MAXIF), REFP, ERROR1, ERROR2
      INTEGER   ISBAND(MAXIF), VER, FRQSEL, LUN1
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION FOFF(MAXIF), UVFREQ, FIN, FOUT
      INCLUDE 'UJOIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN1 /27/
C-----------------------------------------------------------------------
      IERR = 4
      IF (JLOCIF.LT.0) THEN
         MSGTXT = 'NO IF AXIS - I CANNOT DO MY THING'
         GO TO 980
         END IF
      IF (JLOCF.LT.0) THEN
         MSGTXT = 'NO FREQ AXIS - I CANNOT DO MY THING'
         GO TO 980
         END IF
      NCH = CATBLK(KINAX+JLOCF)
      REFP = CATR(KRCRP+JLOCF)
      UVFREQ = CATD(KDCRV+JLOCF)
      NIF = CATBLK(KINAX+JLOCIF)
      IF (NIF.LT.2) THEN
         MSGTXT = 'ONLY 1 IF - I CANNOT DO MY THING'
         GO TO 980
         END IF
      IF (NCH.LE.2) THEN
         MSGTXT = 'FEW SPECTRAL CHANNELS - WHY DO MY THING'
         GO TO 980
         END IF
      NOUT = 0
      NCH = NCH
      DO 10 I = 1,NIF
         CHNSEL(1,I) = IROUND (XCHSEL(1,I))
         IF (CHNSEL(1,I).LT.1) CHNSEL(1,I) = 1
         CHNSEL(2,I) = IROUND (XCHSEL(2,I))
         IF (CHNSEL(2,I).LT.CHNSEL(1,I)) CHNSEL(2,I) = NCH
         CHNSEL(3,I) = IROUND (XCHSEL(3,I))
         IF (CHNSEL(3,I).LT.1) CHNSEL(3,I) = 1
         IF ((CHNSEL(1,I).GT.NCH) .OR. (CHNSEL(2,I).GT.NCH)) THEN
            MSGTXT = 'INCORRECT CHANNEL SPECIFICATIONS'
            GO TO 980
            END IF
         NOUT = MAX (NOUT, CHNSEL(3,I)+CHNSEL(2,I)-CHNSEL(1,I))
 10      CONTINUE
      CATBLK(KINAX+JLOCF) = NOUT
      CATBLK(KINAX+JLOCIF) = 1
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) + CHNSEL(3,1) - CHNSEL(1,1)
      CATR(KRARP) = CATR(KRARP) + CHNSEL(3,1) - CHNSEL(1,1)
      IERR = 0
C                                       tell user channel selection
      DO 50 I = 1,NOUT
         WRITE (MSGTXT,1010) I
         NP = 34
         DO 40 JIF = 1,NIF
            J = I - CHNSEL(3,JIF) + CHNSEL(1,JIF)
C                                       this IF involved
            IF ((J.GE.CHNSEL(1,JIF)) .AND. (J.LE.CHNSEL(2,JIF))) THEN
               WRITE (STRING,1015) JIF, J
               IP = 0
C                                       squeeze all blanks
               DO 30 L = 1,9
                  IF (STRING(L:L).NE.' ') THEN
                     IP = IP + 1
                     STRING(IP:IP) = STRING(L:L)
                     END IF
 30               CONTINUE
               IF (NP+IP-1.LE.80) MSGTXT(NP:) = STRING(:IP)
               NP = NP + IP + 1
               END IF
 40         CONTINUE
         CALL MSGWRT (2)
 50      CONTINUE
C                                       get FQ info
      VER = 1
      FRQSEL = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, FCNO(1), VER, CATOLD, LUN1,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Test frequencies
      DO 60 JIF = 1,NIF
         FOUT = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) * (CHNSEL(3,JIF) -
     *      CATR(KRCRP+JLOCF))
         FIN = UVFREQ + FOFF(JIF) + FINC(JIF) * (CHNSEL(1,JIF) - REFP)
         ERROR1 = (FOUT - FIN) / CATR(KRCIC+JLOCF)
         FOUT = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) * (CHNSEL(3,JIF) -
     *      CATR(KRCRP+JLOCF) + CHNSEL(2,JIF) - CHNSEL(1,JIF))
         FIN = UVFREQ + FOFF(JIF) + FINC(JIF) * (CHNSEL(2,JIF) - REFP)
         ERROR2 = (FOUT - FIN) / CATR(KRCIC+JLOCF)
         IF ((ABS(ERROR1).GT.0.1) .OR. (ABS(ERROR2).GT.0.1)) THEN
            WRITE (MSGTXT,1050) JIF, ERROR1, ERROR2
            CALL MSGWRT (6)
            END IF
 60      CONTINUE
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Output channel',I5,' input IF(ch)')
 1015 FORMAT (I2,'(',I5,')')
 1050 FORMAT ('WARNING: IF',I3,' END FREQS DIFFER BY',2F9.2,' CHANNELS')
      END
      SUBROUTINE UJOINU (IRET)
C-----------------------------------------------------------------------
C   UJOINU sends uv data one point at a time to the user supplied
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
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'UJOIN.INC'
      REAL      CBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
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
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(1), 1, IFILE, IRET)
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
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
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
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
C                                      Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL JOINUV (NUMVIS, CBUFF, INCX, RESULT, IRET)
C                                       Un compressed data
            ELSE
               CALL JOINUV (NUMVIS, BUFF1(IPTRI+NRPRMI), INCX, RESULT,
     *            IRET)
               END IF
C                                       Branch on his return
            IF (IRET) 140, 130, 120
C                                       Error (fatal)
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
 130        CONTINUE
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                       OK, but no output please
 140        IPTRI = IPTRI + LRECI
C                                       Write vis record.
         IF (NIOUT.LT.NIOLIM) GO TO 190
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Final call to JOINUV.
 200  NUMVIS = -1
      CALL JOINUV (NUMVIS, BUFF1, INCX, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UJOINU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UJOINU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UJOINU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UJOINU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UJOINU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UJOINU: JOINUV ERROR',I3)
 1150 FORMAT ('UJOINU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE UJOINH
C-----------------------------------------------------------------------
C   UJOINH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(20)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT, NIF, J
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF), VER, FRQSEL, FREQID
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION FOFF(MAXIF)
      INCLUDE 'UJOIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /20, 'FQ','CH','CL','SN','SU','FG','BP',
     *   'IM', 'CQ', 'PC', 'TY', 'GC', 'MC', 'WX', 'BL', 'AN', 'CP',
     *   'PD', 'CD', 'SY'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(1), FCNO(2), CATBLK,
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
C                                      Add any other history.
      NIF = CATOLD(KINAX+JLOCIF)
      WRITE (HILINE,1010) TSKNAM, NIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      DO 20 I = 1,NIF
         WRITE (HILINE,1011) TSKNAM, I, (CHNSEL(J,I), J = 1,3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
 20      CONTINUE
      IF (OPCODE.EQ.'DIFF') THEN
         HILINE = TSKNAM // '/ IFs weighted by (-1)**(IFnumber-1)'
      ELSE
         HILINE = TSKNAM // '/ IFs weighted by data weights'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       flagging
      IF (ABS(DOWT).LT.0.5) THEN
         HILINE = TSKNAM // '/ Spectrum flagged if any input flagged'
      ELSE IF (DOWT.GT.0.0) THEN
         HILINE = TSKNAM // '/ Channel flagged if any input channel ' //
     *      'flagged'
      ELSE
         HILINE = TSKNAM // '/ Data flagged only if all inputs flagged'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables except FQ
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(1),
     *   FCNO(2), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       copy FQ info
      VER = 1
      FRQSEL = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, FCNO(1), VER, CATOLD, LUN1,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fixup
      NIF = 1
C                                       Rewrite new
      VER = 1
      FREQID = 1
      CALL CHNDAT ('WRIT', BUFF1, DISKO, FCNO(2), VER, CATBLK, LUN1,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       copy tables with IFs
      CALL COPTAB (IERR)
C                                       Update CATBLK.
 990  CALL CATIO ('UPDT', DISKO, FCNO(2), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UJOINH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'NIF =',I5,19X,'/ Input IFs -> 1')
 1011 FORMAT (A6,'CHANSEL(',I2.2,')=',3I5,'  / IF ',I2.2,
     *   '1st last input, 1st output chan')
 1200 FORMAT ('UJOINH: ERROR COPYING TABLES')
      END
      SUBROUTINE JOINUV (NUMVIS, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Actually joins up the IFs to make one spectrum
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
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
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      HISCRD    C(NUMHIS) History records
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, KF, JS, NIF, NS, INDEXO, INDEXI, WTS, JF1, JF2,
     *   WASF(4)
      REAL      WT
      INCLUDE 'UJOIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = CATOLD(KINAX+JLOCS)
         NIF = CATOLD(KINAX+JLOCIF)
         CALL FILL (4, 0, WASF)
         KF = INCX * CATBLK(KINAX+JLOCF) * NS
         CALL RFILL (KF, 0.0, RESULT)
         DO 40 JIF = 1,NIF
            WTS = (-1) ** (JIF - 1)
            JF1 = CHNSEL(1,JIF)
            JF2 = CHNSEL(2,JIF)
            DO 30 JS = 1,NS
               DO 20 JF = JF1,JF2
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  KF = CHNSEL(3,JIF) + JF - JF1
                  INDEXO = (KF-1) * INCFO + (JS-1) * INCSO + 1
                  WT = VIS(3,INDEXI)
                  IF (WT.GT.0.0) THEN
                     IF (OPCODE.EQ.'DIFF') WT = WTS
                     RESULT(1,INDEXO) = RESULT(1,INDEXO) + WT *
     *                  VIS(1,INDEXI)
                     RESULT(2,INDEXO) = RESULT(2,INDEXO) + WT *
     *                  VIS(2,INDEXI)
                     RESULT(3,INDEXO) = RESULT(3,INDEXO) + VIS(3,INDEXI)
                  ELSE IF (DOWT.GT.-0.5) THEN
                     RESULT(3,INDEXO) = -1.0E10
                     WASF(JS) = WASF(JS) + 1
                     END IF
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         IF ((OPCODE.NE.'DIFF') .AND. (ABS(DOWT).GE.0.5)) THEN
            KF = CATBLK(KINAX+JLOCF) * NS
            DO 50 JF = 1,KF
               WT = RESULT(3,JF)
               IF (WT.GT.0.0) THEN
                  RESULT(1,JF) = RESULT(1,JF) / WT
                  RESULT(2,JF) = RESULT(2,JF) / WT
                  END IF
 50            CONTINUE
         ELSE IF (ABS(DOWT).LT.0.5) THEN
            KF = CATBLK(KINAX+JLOCF)
            DO 80 JS = 1,NS
               IF (WASF(JS).GT.0) THEN
                  DO 60 JF = 1,KF
                     INDEXO = (JF-1) * INCFO + (JS-1) * INCSO + 1
                     RESULT(3,INDEXO) = -ABS (RESULT(3,INDEXO))
 60                  CONTINUE
               ELSE IF (OPCODE.NE.'DIFF') THEN
                  DO 70 JF = 1,KF
                     INDEXO = (JF-1) * INCFO + (JS-1) * INCSO + 1
                     WT = RESULT(3,INDEXO)
                     IF (WT.GT.0.0) THEN
                        RESULT(1,INDEXO) = RESULT(1,INDEXO) / WT
                        RESULT(2,INDEXO) = RESULT(2,INDEXO) / WT
                        END IF
 70                  CONTINUE
                  END IF
 80            CONTINUE
            END IF
C                                       last call - no vis
      ELSE
         END IF
C
 999  RETURN
      END
      SUBROUTINE COPTAB (IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, AN(50),
     *   NA, BIF, EIF, SOUWAN, NSOUWD, BCHAN, ECHAN, DOPOL, BPOL, EPOL
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION TIME1, TIME2, FQOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'UJOIN.INC'
      DATA LUN1, LUN2 /28,29/
C-----------------------------------------------------------------------
      MSGTXT = 'Updating tables for IF selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
      BIF = 1
      EIF = 1
      NA = 0
      CALL FILL (50, 0, AN)
      TIME1 = -1.D9
      TIME2 = 1.D9
      SOUWAN = 0
      NSOUWD = 0
      BCHAN = 1
      ECHAN = MAXCHA
      FQOFF = 0.0D0
      DOPOL = 0
C                                       no STOKES selection so okay
      BPOL = 1
      EPOL = 2
C                                       Reference frequency in AN table
C                                       IF selection
      CALL FNDEXT ('AN', CATOLD, NVER)
      DO 100 VER = 1,NVER
         CALL ISTAB ('AN', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, VER, CATOLD, CATBLK, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
      OFQID = 0
C                                       BP tables
      CALL FNDEXT ('BP', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'BANDPASS TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
C                                       CL tables
      CALL FNDEXT ('CL', CATOLD, NVER)
      DO 140 VER = 1,NVER
         CALL ISTAB ('CL', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do NOT select on sources
         IF (EXIST .AND. (IERR.EQ.0)) CALL CLSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, 0, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 140     CONTINUE
C                                       CD tables
      CALL FNDEXT ('CD', CATOLD, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST .AND. (IERR.EQ.0)) CALL CDSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, AN, 0, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'CAL SOURCE POLARIZATION TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
C                                       CQ tables
      CALL FNDEXT ('CQ', CATOLD, NVER)
      DO 160 VER = 1, NVER
         CALL ISTAB ('CQ', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, FCNO(1), DISKO,
     *      FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       FG tables
      CALL FNDEXT ('FG', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'WARNING: FG TABLES MAY CONTAIN BAD IF/CHANNEL' //
     *      ' REFERENCES'
         CALL MSGWRT (6)
         END IF
      DO 180 VER = 1,NVER
         CALL ISTAB ('FG', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL FGSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      BCHAN, ECHAN,  TIME1, TIME2, OFQID, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATOLD, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATOLD, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATOLD, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATOLD, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       PD tables
      CALL FNDEXT ('PD', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         MSGTXT = 'ANTENNA SPECTRAL POLARIZATION TABLES ARE DISCARDED'
         CALL MSGWRT (7)
         END IF
C                                       SN tables
      CALL FNDEXT ('SN', CATOLD, NVER)
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SNSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, ISUB, JSUB, BUFF1, BUFF2,
     *      IRET)
         IF (IRET.GT.0) GO TO 999
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATOLD, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SYSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 290     CONTINUE
C                                       SU tables
      IF (ILOCSU.GE.0) THEN
         CALL FNDEXT ('SU', CATOLD, NVER)
         DO 300 VER = 1,NVER
            CALL ISTAB ('SU', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, FCNO(1),
     *         DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BIF,
     *         EIF, OFQID, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 300        CONTINUE
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATOLD, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, FCNO(1), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL TYSEL (DISKIN, FCNO(1),
     *      DISKO, FCNO(2), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATOLD, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, TIME1,
     *      TIME2, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATOLD, NVER)
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, EIF, OFQID, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C
 999  RETURN
      END
