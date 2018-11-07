LOCAL INCLUDE 'BLOAT.INC'
C                                       Local include for BLOAT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ,
     *   CHAN1, NCHAN, BCHAN, ECHAN, JNCS, JNCF, JNCIF, KNCS, KNCF,
     *   KNCIF, ILOCWT, LRECI, LRECO
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*12
      REAL      XSIN, XDISIN,  XSOUT, XDISO, APARM(10), BUFF1(UVBFSS),
     *   BUFF2(UVBFSS)
      DOUBLE PRECISION FREQI
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, APARM
      COMMON /BLPARM/ FREQI, SEQIN, SEQOUT, DISKIN, DISKO, CHAN1, NCHAN,
     *   BCHAN, ECHAN, JNCS, JNCF, JNCIF, KNCS, KNCF, KNCIF, ILOCWT,
     *   LRECI, LRECO
      COMMON /BLOCHR/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM BLOAT
C-----------------------------------------------------------------------
C! Converts pseudo continuum uv data to line format.
C# UV-util Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002-2003, 2008, 2015, 2017
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
C   BLOAT takes an pseudo-continuum data base and converts it to
C   spectral line data base with 1 or two channels filled in and the
C   rest flagged.  This data set sorted 'BT' can then be merged
C   with other similar data sets with task 'VBMRG'.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         Control information.
C                                   1=> No. channels in output file.
C                                   2=> output file channel of first
C                                       input channel.
C                                   3=> no. channels to copy (1 or 2)
C                                   4+5=>Reference frequency.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'BLOAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'BLOAT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BLOGIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Convert data
      CALL BLOGUV (IRET)
      IF (IRET.EQ.0) CALL BLOHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BLOGIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BLOGIN gets input parameters for BLOAT and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in BLOAT for more details.
C-----------------------------------------------------------------------
      CHARACTER UV*2, STAT*4, PRGN*6, BLANK*6
      INTEGER   JERR, ITEMP, IROUND, NPARM, IERR, OLDCNO, INCX
      DOUBLE PRECISION XTEMP
      LOGICAL   T
      INCLUDE 'BLOAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA UV, BLANK /'UV', ' '/
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
      NPARM = 24
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
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UV,
     *   NLUSER, STAT, BUFF1, IERR)
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
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECI = LREC
      JNCS = INCS / INCX
      JNCF = INCF / INCX
      JNCIF = INCIF / INCX
      FREQI = FREQ
C                                       Find weight and scale.
      IF (INCX.LE.1) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Finish input parms.
      CHAN1 = MAX (1, IROUND (APARM(2)))
      NCHAN = MAX (1, IROUND (APARM(1)))
      BCHAN = MAX (1, IROUND (APARM(3)))
      ECHAN = IROUND (APARM(4))
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MIN (ECHAN, CATBLK(KINAX+JLOCF))
      ITEMP = NCHAN - ECHAN + BCHAN
      CHAN1 = MIN (ITEMP, CHAN1)
      IF (CHAN1.LE.0) THEN
         WRITE (MSGTXT,1045) BCHAN, ECHAN, NCHAN
         IERR = 2
         GO TO 990
         END IF
C                                       Set frequency axis size
      CATBLK(KINAX+JLOCF) = NCHAN
C                                       Set ref. frequency.
      XTEMP = APARM(5)
      XTEMP = XTEMP + APARM(6)
      IF (XTEMP.GT.1.0D0) CATD(KDCRV+JLOCF) = XTEMP
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) + CHAN1 - BCHAN
      CATR(KRARP) = CATR(KRARP) + CHAN1 - BCHAN
C                                       Create output file.
      CCNO = 1
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1050) IERR
         GO TO 990
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                        Put input file in READ
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UV,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       output setup
      CALL UVPGET (JERR)
      LRECO = LREC
      KNCS = INCS / INCX
      KNCF = INCF / INCX
      KNCIF = INCIF / INCX
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLOGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('CANNOT WRITE BCHAN, ECHAN=',2I5,' INTO',I5,' CHANNELS')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE BLOGUV (IRET)
C-----------------------------------------------------------------------
C   BLOGUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER XUV*2
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, I, LUNO, INDI,
     *   VO, BO, NUMVIS, XCOUNT, INDO, ILENBU, KBIND, IBIND,
     *   NIOUT, NIOLIM, INCX, NCORI, NCORO
      CHARACTER PHNAME*48
      LOGICAL   T, F
      INCLUDE 'BLOAT.INC'
      REAL      DUM, CBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO, XUV /16, 17, 'UV'/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL (XUV, DISKIN, FCNO(NCFILE), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Open vis file for write
 10   CALL ZPHFIL (XUV, DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      INCX = CATBLK(KINAX)
      NCORI = (LRECI - NRPARM) / INCX
      NCORO = (LRECO - NRPARM) / INCX
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
C                                       uncompressed data
            IF (INCX.GT.1) THEN
               CALL BLOGIT (NUMVIS, BUFF1(IPTRI+ILOCU),
     *            BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *            BUFF1(IPTRI+NRPARM), BUFF2(IPTRO+NRPARM), IRET)
C                                       compressed data
            ELSE
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL BLOGIT (NUMVIS, BUFF1(IPTRI+ILOCU),
     *            BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW), CBUFF, RESULT,
     *            IRET)
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE
               XCOUNT = XCOUNT + 1
               CALL RCOPY (NRPARM, BUFF1(IPTRI), BUFF2(IPTRO))
               IF (INCX.LE.1) CALL ZUVPAK (NCORO, RESULT,
     *            BUFF2(IPTRO+ILOCWT), BUFF2(IPTRO+NRPARM))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       OK, but no output please
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
            IF (NIOUT.LT.NIOLIM) GO TO 190
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.EQ.0) GO TO 160
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
 160           IPTRO = KBIND
               NIOUT = 0
 190        CONTINUE
         GO TO 100
C                                       Final call to BLOGIT.
 200  NUMVIS = -1
      CALL BLOGIT (NUMVIS, DUM, DUM, DUM, BUFF1, BUFF1, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
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
 1000 FORMAT ('BLOGUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('BLOGUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('BLOGUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('BLOGUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('BLOGUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('BLOGUV: BLOGIT ERROR',I3)
 1150 FORMAT ('BLOGUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE BLOHIS
C-----------------------------------------------------------------------
C   BLOHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72
      INTEGER   IERR, LUN1, LUN2, NONOT
      LOGICAL   T
      INCLUDE 'BLOAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA NONOT, NOTTYP /0,'  '/
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CHAN1
      WRITE (HILINE,2001) TSKNAM,CHAN1
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       NCHAN
      WRITE (HILINE,2000) TSKNAM, NCHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BCHAN
      WRITE (HILINE,2002) TSKNAM, BCHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       ECHAN
      WRITE (HILINE,2002) TSKNAM, ECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Ref. frequency
      WRITE (HILINE,2004) TSKNAM, CATD(KDCRV+JLOCF)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LT.2) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
C                                        Update CATBLK.
 30   CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLOHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1020 FORMAT ('BLOHIS: ERROR',I3,' COPYING TABLES')
 2000 FORMAT (A6,'APARM(1) =',I5,' /NUMBER OF OUTPUT CHANNELS')
 2001 FORMAT (A6,'APARM(2) =',I5,' /1ST CHAN. NO. OF INPUT.')
 2002 FORMAT (A6,'APARM(3) =',I5,' / FIRST CHAN. COPIED.')
 2004 FORMAT (A6,'/REFERENCE FREQ = ',1D20.8)
      END
      SUBROUTINE BLOGIT (NUMVIS, U, V, W, VISIN, VISOUT, IRET)
C-----------------------------------------------------------------------
C   BLOGIT copies the input visibilities to temporary storage,
C   zero fills the output arrays, then copies the input visibilities to
C   the correct location.
C   Inputs:
C      NUMVIS   I        Visibility number, -1.0=> final call, no data
C                        passed but allows any operations to be completed.
C      U        R        U in wavelengths
C      V        R        V in wavelengths
C      W        R        W in wavelengths
C      VISIN    R(3,*)   Visibilities: real, imaginary, weight (Jy)
C   Inputs from COMMON
C      CHAN1    I        Output channel number of first input channel
C      BCHAN    I        Number of channels to copy
C   Output:
C      VISOUT   R(3,*)   Visibilities
C      IRET     I        Return code  -1 => don't write
C                                      0 => OK
C                                     >0 => error, terminate.
C   Output in COMMON
C      CATBLK   I        Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, VISIN(3,*), VISOUT(3,*)
C
      INTEGER   NFILL, NS, NIF, NF, JIF, JF, JS, INDEXI, INDEXO, I,
     *   CHOFF
      DOUBLE PRECISION UVWCOR
      INCLUDE 'BLOAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         NFILL = NS * NIF * NF * 3
         CALL RFILL (NFILL, 0.0, VISOUT)
         CHOFF = BCHAN - CHAN1 + 1
         DO 40 JIF = 1,NIF
            DO 30 JF = BCHAN,ECHAN
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * JNCIF + (JF-1) * JNCF +
     *               (JS-1) * JNCS + 1
                  INDEXO = (JIF-1) * KNCIF + (JF-CHOFF) * KNCF +
     *               (JS-1) * KNCS + 1
                  DO 10 I = 1,3
                     VISOUT(I,INDEXO) = VISIN(I,INDEXI)
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       Correct u, v, w
      UVWCOR = CATD(KDCRV+JLOCF) / FREQI
      U = U * UVWCOR
      V = V * UVWCOR
      W = W * UVWCOR
C                                       last call - no vis
      ELSE
         END IF
C
 999  RETURN
      END
