LOCAL INCLUDE 'BASRM.INC'
C                                       Local include for BASRM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MMXSOU
C                                       MMXSOU = max number of sources
      PARAMETER (MMXSOU = 500)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(5)*64
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, NTCOR,
     *   CH1, CHLAST, BIF, EIF, QUAL(30), CATOLD(256), KLOCWT, FRQSEL,
     *   SOUWAN(MMXSOU), NSOUWD, MXIF, MXCH, SOCOUN(MMXSOU), IBASE(10),
     *   ILOCWT
      LOGICAL   SELCH, DROPCH(1024), ISCMP, DOBOTH, DOAC, DOXC, MULTI,
     *   DOSCL, REPTON, ISCOMP
      DOUBLE PRECISION XCOUNT(4), FQOFF
      REAL      XSIN, XDISIN, XSOUT, XDISO, XBAND, XFREQ, XFQID,
     *   BPARM(10), BUFF1(UVBFSL), BUFF2(UVBFSS), APARM(10)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /CHSEL/ SELCH, DROPCH, BIF, EIF, MXIF,
     *   MXCH, QUAL, NSOUWD, SOCOUN, SOUWAN
      COMMON /INPARM/ XCOUNT, FQOFF,
     *   XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   XBAND, XFREQ, XFQID, APARM, BPARM
      COMMON /XTRPRM/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NTCOR, CH1,
     *   CHLAST, KLOCWT, FRQSEL, NUMHIS, ISCMP, DOBOTH, DOAC, DOXC,
     *   MULTI, DOSCL, REPTON, IBASE, ISCOMP, ILOCWT
      COMMON /CHPARM/ NAMEIN, NAMOUT, CLAIN, CLAOUT, HISCRD
LOCAL END
      PROGRAM BASRM
C-----------------------------------------------------------------------
C! BASRM copies selected data from UV data base
C# Task UV Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2015-2016
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
C   BASRM copies a database removing polynomial spectral baselines
C   from total power data.
C   Inputs:
C     AIPS adverb    Prg. name.          Description.
C       INNAME         NAMEIN        Name of input uv data.
C       INCLASS        CLAIN         Class of input uv data.
C       INSEQ          SEQIN         Seq. of input uv data.
C       INDISK         DISKIN        Disk number of input uv data.
C       OUTNAME        NAMOUT        Name of the output uv file.
C                                    Default output is input file.
C       OUTCLASS       CLAOUT        Class of the output uv file.
C       OUTSEQ         SEQOUT        Seq. number of output uv data.
C       OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER IRET
      INCLUDE 'BASRM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BASRM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL COPYIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL COPYUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL COPYHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE COPYIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   COPYIN gets input parameters for COPY and creates an output file
C   if necessary.  Also set the parameters which are used to select
C   data.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                6 => too much data to select ch.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C      NSOUWD     I    Number of sources IDs to copy (0=>all)
C      SOUWAN     I(*) List of source IDs to copy.
C      FRQSEL     I    Freq ID sel, .le. 0 => all
C      SELCH      L    If .TRUE. some of the channels are to be dropped.
C      DROPCH(*)  L    If .TRUE. the corresponding correlator value is
C                      to be dropped.
C      FQOFF      D    Additive change in reference frequency
C      DOSCL      L    If true rescale u,v,w
C      ISCMP      L    If .TRUE. then data is compressed
C----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, BLANK*6, UTYPE*2
      INTEGER   JERR, NCHOLD, NIFOLD, NIFNEW, ICOUNT, CHNUM, ITEMP, I,
     *   OLDCNO, J, IROUND, NPARM, IERR, NUMAN(513), ISUB, LUNAN, INCX,
     *   LUNI, NEVIS
      REAL      TIME1, TIME2, SELBAN
      DOUBLE PRECISION SELFRQ
      LOGICAL   T, F, MATCH
      INCLUDE 'BASRM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IBUFF(UVBFSS)
C
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (BUFF1, IBUFF)
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 37
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
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 10 I = 1, 10
          IBASE(I) = IROUND(BPARM(I))
 10       CONTINUE
C                                       Crunch input parameters
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      CALL FILL (MMXSOU, 0, SOCOUN)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
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
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Save old CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed?
      ISCMP = CATBLK(KINAX).EQ.1
      IF (ISCMP) THEN
         MSGTXT = 'BASRM DOES NOT YET WORKED ON COMPRESSED DATA'
         CALL MSGWRT (6)
         MSGTXT = 'USE UVCMP'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 999
         END IF
C                                       Data type to copy
      DOBOTH = .TRUE.
      DOXC = .TRUE.
      DOAC = .TRUE.
C                                       Report progress?
      REPTON = .FALSE.
C                                       Find WEIGHT random parameter
      IF (ISCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *      IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'COULD NOT FIND WEIGHT FOR COMPRESSED DATA'
            JERR = 6
            GO TO 990
            END IF
      ELSE
         KLOCWT = -1
         END IF
C                                       Freq id
      IF (ILOCFQ.GE.0) THEN
         SELBAN = XBAND
         SELFRQ = XFREQ
         FRQSEL = IROUND (XFQID)
       ELSE
          SELBAN = 0.
          SELFRQ = 0.
          FRQSEL = 0
          END IF
      IF (FRQSEL.LE.0) FRQSEL = -1
      IF ((SELBAN.GT.0.0) .OR. (SELFRQ.GT.0.0)) THEN
         LUNI = 28
         CALL FQMATC (DISKIN, OLDCNO, CATOLD, LUNI, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, JERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            JERR = 1
            GO TO 990
            END IF
         IF (JERR.GT.0) GO TO 999
         END IF
C                                       IF range
      IF (JLOCIF.GT.0) THEN
         NIFOLD = CATBLK(KINAX+JLOCIF)
         BIF = 1
         EIF = NIFOLD
      ELSE
         BIF = 1
         EIF = 1
         NIFOLD = 1
         END IF
      NIFNEW = EIF - BIF + 1
C                                       Check high channel number.
      CH1 = 1
      CHLAST = CATBLK(KINAX+JLOCF)
C                                       Set time range.
      TIME1 = 0
      TIME2 = 0
      XCOUNT(2) = -1.0D6
      IF (TIME1.NE.0.0) XCOUNT(2) = TIME1
      XCOUNT(3) = TIME2
      IF ((TIME1.GE.TIME2) .OR. (TIME2.EQ.0.0)) XCOUNT(3) = 1.0D6
      XCOUNT(4) = 0.0D0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT,CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Determine number of correlators
      NTCOR = 1
      J = CATBLK(KIDIM)
      DO 50 I = 2,J
         NTCOR = NTCOR * CATBLK(KINAX+I-1)
 50      CONTINUE
C                                       Modify header for channel
C                                       select, keep same ref. frequency
C                                       but it will have a new number.
      NCHOLD = CATBLK(KINAX+JLOCF)
      CATBLK(KINAX+JLOCF) = CHLAST - CH1 + 1
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - CH1 + 1
C                                       IF selection in CATBLK
      IF (NIFOLD.GT.NIFNEW) THEN
         CATBLK(KINAX+JLOCIF) = NIFNEW
         CATD(KRCRP+JLOCIF) = 1.0
         CATD(KDCRV+JLOCIF) = 1.0
         END IF
C                                       Change alt. ref. pixel; keep
C                                       same ref. pixel which now has
C                                       a new number.
      CATR(KRARP) = CATR(KRARP) - (CH1 - 1)
C                                       Multi-source file?
      CALL MULSDB (CATOLD, MULTI)
C                                       Source list
      NSOUWD = 0
C                                       Create output file.
      ITEMP = CATBLK(KIGCN)
      CALL ESTSIZ (OLDCNO, NEVIS, IERR)
      IF ((IERR.EQ.0) .AND. (NEVIS.GT.0)) THEN
         CATBLK(KIGCN) = NEVIS
         END IF
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
C
      CATBLK(KIGCN) = ITEMP
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) GO TO 60
            WRITE (MSGTXT,1050) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
 60      IF ((CCNO.EQ.OLDCNO) .AND. (DISKO.EQ.DISKIN) .AND.
     *   (BPARM(2).LE.0.0)) GO TO 65
            WRITE (MSGTXT,1060)
            IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN))
     *         WRITE (MSGTXT,1061)
            GO TO 990
C                                       Recover existing CATBLK
 65      FRW(NCFILE+1) = 2
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
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      IF (IERR.NE.0) GO TO 999
      SEQOUT = CATBLK(KIIMS)
      XCOUNT(1) = 0.0D0
C                                       Set data selection flags.
      SELCH = (NCHOLD .GT. CATBLK(KINAX+JLOCF)) .OR. (NIFOLD.GT.NIFNEW)
C                                       Set correlator flags.
      INCX = CATBLK(KINAX)
      ICOUNT = -INCX
      DO 100 I = 1,NTCOR
C                                       Determine channel number.
         ICOUNT = ICOUNT + INCX
         ITEMP = ICOUNT / INCF
         CHNUM = MOD (ITEMP, NCHOLD) + 1
C                                       Check if data wanted.
         DROPCH(I) = (CHNUM.LT.CH1) .OR. (CHNUM.GT.CHLAST)
C                                       IF Selection
         IF (NIFOLD.GT.NIFNEW) THEN
            ITEMP = ICOUNT / INCIF
            CHNUM = MOD (ITEMP, NIFOLD) + 1
C                                       Check if data wanted.
            DROPCH(I) = DROPCH(I) .OR. ((CHNUM.LT.BIF) .OR.
     *         (CHNUM.GT.EIF))
            END IF
 100     CONTINUE
C                                       Baselines desired -
C                                       get subarray info
      ISUB = 0
      LUNAN = 20
      CALL GETNAN (DISKIN, OLDCNO, CATOLD, LUNAN, BUFF1, NUMAN, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COPYIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY NOT OVERWRITE INPUT FILE IF DELETING SUBARRAYS')
 1061 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('COPYIN: ERROR',I3,' UPDATING NEW CATBLK')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE COPYUV (IRET)
C-----------------------------------------------------------------------
C   COPYUV sends uv data one point at a time to the time check routine
C   and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C                       9 => output record size exceeds input.
C-----------------------------------------------------------------------
      CHARACTER OFILE*48, IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, LENBU, INDI, INDO,
     *   LRECO, I, ILENBU, KBIND, NIOUT, NIOLIM, IBIND, INCX, VO, BO,
     *   NUMVIS, PRTCNT, INTRVL, TIMINT(3), IROUND, CURSOU
      LOGICAL   T, F, DOARR
      INCLUDE 'BASRM.INC'
      REAL      SECS, CBUFF(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO, LENBU /0, 1, 32/
      DATA T, F /.TRUE.,.FALSE./
      DATA INTRVL /10000/
C-----------------------------------------------------------------------
C                                       Set length of complex axis
      INCX = CATBLK(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
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
C                                       LRECO = length of output rec.
      LRECO = CATBLK(KINAX)
      DO 30 I = 2,KICTPN
         LRECO = LRECO * MAX (1, CATBLK(KINAX+I-1))
 30      CONTINUE
      LRECO = LRECO + NRPARM
C                                       Make sure LREC >= LRECO
      IF (LREC.LT.LRECO) THEN
         WRITE (MSGTXT,1030) LRECO, LREC
         IRET = 9
         GO TO 990
         END IF
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      PRTCNT = 0
      DOARR = .FALSE.
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
C                                       Decide if it's kept, select
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCOR, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL COPYIT (NUMVIS, CBUFF, BUFF1(IPTRI), DOARR, IRET)
            ELSE
               CALL COPYIT (NUMVIS, BUFF1(IPTRI+NRPARM), BUFF1(IPTRI),
     *            DOARR, IRET)
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               IF (ISCOMP) THEN
                  CALL RCOPY (NRPARM, BUFF1(IPTRI), BUFF2(IPTRO))
                  CALL ZUVPAK (NCOR, CBUFF, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPARM))
               ELSE
                  CALL RCOPY (LRECO, BUFF1(IPTRI), BUFF2(IPTRO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               IF (ILOCSU.GT.-1) THEN
                  CURSOU = IROUND(BUFF1(IPTRI+ILOCSU))
                  SOCOUN(CURSOU) = SOCOUN(CURSOU) + 1
                  END IF
C                                       Keep user informed.
               PRTCNT = PRTCNT + 1
               IF ((PRTCNT .EQ. INTRVL) .AND. (REPTON)) THEN
C                                       Time of this record.
                  CALL PTIME (BUFF1(IPTRI + ILOCT), F, TIMINT, SECS)
                  WRITE (MSGTXT, 1310) NUMVIS, TIMINT, SECS
                  CALL MSGWRT (6)
                  PRTCNT = 0
                  END IF
               END IF
C                                       OK, but no output please
            IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1140) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         GO TO 100
C                                       Final call to COPYIT
 200     NUMVIS = -1
         CALL COPYIT (NUMVIS, BUFF1, BUFF1, DOARR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1140) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT(1) + 0.01
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
 1000 FORMAT ('COPYUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('COPYUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('COPYUV: LRECO=',I4,'.GT. LREC=',I4)
 1040 FORMAT ('COPYUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1060 FORMAT ('COPYUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('COPYUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('COPYUV: COPYIT ERROR',I3)
 1140 FORMAT ('COPYUV: ERROR',I3,' WRITING VIS FILE')
 1310 FORMAT ('Writing visibility # ',I8,' at ',I3,'/',2I3,F6.2)
      END
      SUBROUTINE COPYHI
C-----------------------------------------------------------------------
C   COPYHI copies and updates history file for COPY
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72,  LABEL*8
      INTEGER   LUN1, LUN2, I, IERR, NONOT
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BASRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T, F /.TRUE.,.FALSE./
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add beginning, end channel.
      IF (SELCH) THEN
         WRITE (HILINE,1011) TSKNAM, CH1, CHLAST
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       IF Selection
      WRITE (HILINE,1013) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       FQ Selection
      IF (FRQSEL.GT.0) THEN
         WRITE (HILINE,1014) TSKNAM, FRQSEL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       QUAL
         WRITE (HILINE,1015) TSKNAM, QUAL(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Source selection
         IF (NSOUWD.LE.0) THEN
C                                       All
            WRITE (HILINE,3000) TSKNAM
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
C                                      Add any other history.
      IF (NUMHIS.GE.1) THEN
         WRITE (LABEL,1010) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 190 I = 1,NUMHIS
            HILINE(9:64) = HISCRD(I)(1:64)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 190        CONTINUE
         END IF
C                                       Close HI file
 200     CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COPYHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1011 FORMAT (A6,' BCHAN =',I5,', ECHAN=',I5,
     *   ' / First and last ch. no.')
 1013 FORMAT (A6,' BIF =',I5,' EIF =',I5,' / Range of IFs copied')
 1014 FORMAT (A6,' FREQID =',I5,' / Selected FQ id.')
 1015 FORMAT (A6,' QUAL =',I5,' / Selected Qualifier')
 1020 FORMAT ('COPYHI: ERROR COPYING TABLES')
 3000 FORMAT (A6,' SOURCES = ''''     /All sources selected')
      END
      SUBROUTINE COPYIT (NUMVIS, VIS, RPARM, DONARR, IRET)
C-----------------------------------------------------------------------
C   COPYIT applies a clipping operation to the Visibilities using
C   XCOUNT(1) as a counter and XCOUNT(2) - XCOUNT(3) as the allowed
C   time range.  It also refuses to pass fully flagged samples unless
C   BPARM(1) is TRUE (> 0).  XCOUNT(4) is used to count the refusals.
C   Also checks that data in the requested uv range and that correct
C   data type is being copied.
C       Correlators can also be selected using SELCH and DROPCH to
C   select the correlators to be dropped.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed
C   In/Out:
C      VIS(3,*)   R    VIS record; RE, IM, WT for NTCOR samples
C      RPARM(*)   R    Random parameter array. When a single FQ ID
C                      is copied the FQ r.p in this array is changed.
C      DONARR     L    T -> an array > 1 has been dropped
C   Inputs from COMMON
C      TIME(8)    R    User time range
C      BPARM(10)  R    User array.
C      RA         D    Right ascension (1950) of phase center. (deg)
C      DEC        D    Declination (1950) of phase center. (deg)
C      FREQ       D    Frequency of observation (Hz)
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      NTCOR      I    Total number of input correlators.
C      NSOUWD     I    Number of sources IDs to copy (0=>all)
C      SOUWAN     I(*) List of source IDs to copy.
C      DOSCL      L    If true rescale u,v,w
C      FRQSEL     I    Freq ID sel, .le.0 => all
C      SELCH      L    If .TRUE. some of the channels are to be dropped.
C      DROPCH(*)  L    If .TRUE. the corresponding correlator value is
C                      to be dropped.
C      ISCMP      L    If .TRUE. then data is compressed
C      KLOCWT     I    0-rel pointer for Weight random parameter
C      CATBLK(256)I    Catalog header record. See [DOC]HEADER.
C      DOBOTH     L    Both xc & ac data copyied
C      DOXC       L    Only xc data
C      DOAC       L    Only ac data
C   Output:
C      VIS        R    Visibilities possibly with some flagged
C      IRET       I    Return code  -1 => don't write
C                                0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(16,NUMHIS) R   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   DONARR
      INTEGER   I, IRET, NP
      REAL      VIS(3,*), RPARM(*), TPLATE(4*MAXCIF),
     *   BFIT(11), XI
      INTEGER   NUMVIS, IA1, IA2, IROUND
      INCLUDE 'BASRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Select data
      IF (NUMVIS.LE.0) GO TO 200
C                                       Data type
         XCOUNT(1) = XCOUNT(1) + 1.D0
         IF (ILOCB.GE.0) THEN
            IA1 = RPARM(1+ILOCB) / 256.0 + 0.1
            IA2 = RPARM(1+ILOCB) - IA1 * 256.0 + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         IRET = -1
         IF (DOBOTH) IRET = 0
         IF (DOXC .AND. (IA1.NE.IA2)) IRET = 0
         IF (DOAC .AND. (IA1.EQ.IA2)) IRET = 0
         IF (IA1.NE.IA2) GO TO 999
         IF (IRET.EQ.-1) GO TO 999
         DO 100 I = 1, NTCOR
            TPLATE(I) = FBLANK
            IF (VIS(3,I).GT.0.0) TPLATE(I) = VIS(1,I)
  100       CONTINUE
         NP = IROUND(APARM(1))
         CALL BLINE (TPLATE, NTCOR, NP, IBASE, BFIT, IRET)
         IF (APARM(3).GT.0.0) THEN
            WRITE (MSGTXT,1500) BFIT(1), BFIT(2), BFIT(3), BFIT(4),
     *         BFIT(5)
            CALL MSGWRT (6)
            END IF
         IF (APARM(2).GT.0.0) THEN
            DO 130 I = 1, NTCOR
               XI = I
               TPLATE(I) = BFIT(1) + BFIT(2)*XI + BFIT(3)*XI*XI +
     *            BFIT(4)*XI*XI*XI + BFIT(5)*XI*XI*XI*XI
  130          CONTINUE
            ENDIF
         DO 150 I = 1,NTCOR
            VIS(1,I) = TPLATE(I)
  150       CONTINUE
         GO TO 999
C                                        History cards
 200  WRITE (MSGTXT,1201)
      CALL MSGWRT (4)
      HISCRD(1)(1:64) = MSGTXT(1:64)
      WRITE (MSGTXT,1203)
      CALL MSGWRT (4)
      HISCRD(2)(1:64) = MSGTXT(1:64)
      WRITE (MSGTXT,1204) XCOUNT(1)
      CALL MSGWRT (4)
      HISCRD(3)(1:64) = MSGTXT(1:64)
      IF (BPARM(1).GT.0.) WRITE (MSGTXT,1205)
      IF (BPARM(1).LE.0.) WRITE (MSGTXT,1206) XCOUNT(4)
      CALL MSGWRT (4)
      HISCRD(4)(1:64) = MSGTXT(1:64)
      NUMHIS = 4
      IF (DONARR) THEN
         WRITE (MSGTXT,1207)
         CALL MSGWRT (4)
         HISCRD(5)(1:64) = MSGTXT(1:64)
         NUMHIS = 5
         END IF
C                                       error check
      IF (XCOUNT(1).LE.0.0) IRET = 8
C
 999  RETURN
C-----------------------------------------------------------------------
 1201 FORMAT ('Copied from the beginning')
 1203 FORMAT ('        to  the end')
 1204 FORMAT ('Copied',F10.0,' vis records')
 1205 FORMAT ('Can include fully flagged samples')
 1206 FORMAT ('Dropped',F9.0,' flagged vis records')
 1207 FORMAT ('Dropped subarray designations > 1')
 1500 FORMAT ('BFIT: ',5F10.4)
      END
      SUBROUTINE ESTSIZ (CNO, NEVIS, IRET)
C-----------------------------------------------------------------------
C   Routine to examine the NX table and based on time ranges, FREQID
C   and selected sources' list determine the estimated number of
C   visibilities in the output file.
C   Input:
C       CNO          I         Cat. # of file
C   Input from common:
C       MULTY        L         Multy- or single- source data
C       NSOUWD       I         Number of selected sources
C       SOWAN(MMXSOU)I         Array of IDs of selected sources
C       XCOUNT(2)    D         Start time
C       XCOUNT(3)    D         Stop time
C       FRQSEL       I         FQ ID to use, < 1 => all
C       DISKIN       I         Vol containing input file
C       OLDCNO       I         Cat. number of input file
C   Output:
C       NEVIS        I         Estimated # visibilities
C       IRET         I         Error code, 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NEVIS, CNO, IRET
C
      INTEGER NXVER, LUNI, IERR, INXRNO, NXKOLS(MAXNXC), K,
     *   NXNUMV(MAXNXC), NUMBNX, I, IDSOUR, ISUB, VSTART, VEND,
     *   FREQID, SUMVIS, BUFFNX(512)
      REAL    RTIME, DTIME
      LOGICAL TABLE, EXIST, FITASC, OURS, OURST
      INCLUDE 'BASRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI /16/
C-----------------------------------------------------------------------
      IRET = 0
      NEVIS = CATBLK(KIGCN)
C                                       Is this multi-source?
      CALL MULSDB (CATBLK, MULTI)
      IF (.NOT.MULTI) GO TO 999
C                                       Is there an NX table
      NXVER = 1
      CALL ISTAB ('NX', DISKIN, CNO, NXVER, LUNI, BUFFNX, TABLE,
     *   EXIST, FITASC, IERR)
      IF (.NOT.EXIST) THEN
         NEVIS = CATBLK(KIGCN)
         GO TO 999
         END IF
C                                       Open the table
      CALL NDXINI ('READ', BUFFNX, DISKIN, CNO, NXVER, CATBLK,
     *   LUNI, INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       How many entries
      NUMBNX = BUFFNX(5)
      SUMVIS = 0
      DO 100 I = 1, NUMBNX
         CALL TABNDX ('READ', BUFFNX, INXRNO, NXKOLS, NXNUMV, RTIME,
     *      DTIME, IDSOUR, ISUB, VSTART, VEND, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Check on FREQID
         OURS = (FRQSEL.LE.0) .OR. (FREQID.EQ.FRQSEL) .OR. (FREQID.LE.0)
C                                       Check time
         OURS = OURS .AND. (((RTIME+DTIME*0.5) .GE. XCOUNT(2)) .AND.
     *      ((RTIME-DTIME*0.5) .LE. XCOUNT(3)))
C                                       Check sources if MULTI
         OURST = .FALSE.
         DO 80 K = 1, NSOUWD
            IF (IDSOUR .EQ. SOUWAN(K)) OURST = .TRUE.
   80       CONTINUE
         IF (OURS .AND. OURST) SUMVIS = SUMVIS + (VEND - VSTART + 1)
 100     CONTINUE
      NEVIS = SUMVIS
      CALL TABNDX ('CLOS', BUFFNX, INXRNO, NXKOLS, NXNUMV, RTIME,
     *   DTIME, IDSOUR, ISUB, VSTART, VEND, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ESTSIZ: ERROR ',I3,' OPENING NX FILE')
 1010 FORMAT ('ESTSIZ: ERROR ',I3,' READING NX FILE')
 1020 FORMAT ('ESTSIZ: ERROR ',I3,' CLOSING NX FILE')
      END
      SUBROUTINE BLINE (ACSPEC, NUMFRQ, NP, BASE, BFIT, IRET)
C-----------------------------------------------------------------------
C  Routine BLINE fits a polynomial baseline to spectra. The
C  fitted parameters are returned and the data is returned
C  with the baseline removed.
C  The routine assumes that only one IF/POLZN is passed down.
C  The calling routine must call BLINE over a loop of # IF's
C  and # polzns.
C
C  Inputs:
C     ACSPEC(*)      R        Input spectrum
C     NUMFRQ         I        # freq. channels in the spectrum
C     NP             I        The degree of polynomial to fit
C     BASE(2,5)      I        Array of start and stop channel
C                             numbers specifying the channels
C                             to be used for fitting
C  Outputs:
C     ACSPEC(*)      R        Output spectrum
C     BFIT(11)       R        The fitted parameters
C     IRET           I        Error code: 0 => OK
C                                         1 => error
C-----------------------------------------------------------------------
      REAL    ACSPEC(*), BFIT(11)
      INTEGER NUMFRQ, NP, BASE(2,5), IRET
C
      INTEGER IB, IBB, IBB1, I, NCHNLS, MX2, J, N, K, NM1, KP1,
     *   L, IP1
      REAL    XB(1024), YB(1024), P(20), A(11,11), B(11), YBAVG,
     *   RTEMP, TEMP, FACTOR, SUM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NCHNLS = 0
C                                       Check channel ranges
      DO 100 IB = 1, 5
         IF (BASE(1,IB).LT.0 .OR. BASE(2,IB).LT.0) THEN
            IRET = 1
            WRITE (MSGTXT,1000) BASE(1,IB), BASE(2,IB)
            GO TO 990
            END IF
         IF (BASE(1,IB).GT.NUMFRQ .OR. BASE(2,IB).GT.NUMFRQ) THEN
            IRET = 1
            WRITE (MSGTXT,1010) BASE(1,IB), BASE(2,IB), NUMFRQ
            GO TO 990
            END IF
         IF (BASE(2,IB).LT.BASE(1,IB)) THEN
            IRET = 1
            WRITE (MSGTXT,1020) BASE(1,IB), BASE(2,IB)
            GO TO 990
            END IF
C                                       Select data
         IBB=BASE(1,IB)
         IBB1=BASE(2,IB)
         IF ((IBB.EQ.0) .OR. (IBB1.EQ.0)) GO TO 100
         DO 50 I=IBB,IBB1
            IF (ACSPEC(I).NE.FBLANK) THEN
               NCHNLS = NCHNLS + 1
               XB(NCHNLS) = I
               YB(NCHNLS) = ACSPEC(I)
               END IF
 50         CONTINUE
 100     CONTINUE
C                                       Special case, NP = 0
      IF (NP.EQ.0) THEN
         YBAVG = 0.0
         IF (NCHNLS.EQ.0) GO TO 120
         DO 110 I = 1, NCHNLS
            YBAVG = YBAVG + YB(I)
 110        CONTINUE
         YBAVG = YBAVG / NCHNLS
 120     DO 150 I = 1, NUMFRQ
            IF (ACSPEC(I).NE.FBLANK) ACSPEC(I) = ACSPEC(I) - YBAVG
 150        CONTINUE
         BFIT(1) = YBAVG
         GO TO 999
         END IF
C                                       Set up for polynomial fit
C                                       Jump out if nothing to fit
      IF (NCHNLS.EQ.0) GO TO 999
      MX2=NP*2
      DO 210 I=1,MX2
         P(I)=0.0
         DO 200 J=1,NCHNLS
            P(I)=P(I)+XB(J)**I
 200        CONTINUE
 210     CONTINUE
C
      N=NP+1
      DO 250 I=1,N
         DO 240 J=1,N
            K=I+J-2
            IF (K.LE.0) THEN
               A(1,1) = NCHNLS
            ELSE
               A(I,J)=P(K)
               END IF
 240        CONTINUE
 250     CONTINUE
C                                       Do the fitting
      B(1)=0.0
      DO 300 J = 1,NCHNLS
         B(1)=B(1)+YB(J)
 300     CONTINUE
      DO 320 I = 2,N
         B(I)=0.0
         DO 310 J=1,NCHNLS
            B(I)=B(I)+YB(J)*XB(J)**(I-1)
 310        CONTINUE
 320     CONTINUE
C
      NM1=N-1
      DO 500 K=1,NM1
         KP1=K+1
         L=K
         DO 340 I=KP1,N
            RTEMP = ABS(A(I,K)) - ABS(A(L,K))
            IF (RTEMP.LE.0) GO TO 340
            L=I
 340        CONTINUE
         IF ((L-K).GT.0) THEN
            DO 350 J=K,N
               TEMP=A(K,J)
               A(K,J)=A(L,J)
               A(L,J)=TEMP
 350           CONTINUE
            TEMP=B(K)
            B(K)=B(L)
            B(L)=TEMP
         ELSE
            DO 400 I=KP1,N
               FACTOR=A(I,K)/A(K,K)
               A(I,K)=0.0
               DO 370 J=KP1,N
                  A(I,J)=A(I,J)-FACTOR*A(K,J)
 370              CONTINUE
               B(I)=B(I)-FACTOR*B(K)
 400           CONTINUE
            END IF
 500        CONTINUE
C                                       Fill in BFIT array with
C                                       fitted parms, be careful
C                                       do it backwards
      BFIT(N)=B(N)/A(N,N)
      I = NM1
      DO 610 K = 1, NM1
         I = NM1 - K + 1
         IP1=I+1
         SUM=0.0
         DO 600 J=IP1,N
            SUM=SUM+A(I,J)*BFIT(J)
 600        CONTINUE
         BFIT(I)=(B(I)-SUM)/A(I,I)
 610  CONTINUE
C                                       Subtract baseline from data
      DO 700 I=1,NUMFRQ
         TEMP=0.0
         DO 650 J=1,N
            TEMP=TEMP+BFIT(J)*I**(J-1)
  650       CONTINUE
         IF (ACSPEC(I).NE.FBLANK) ACSPEC(I)=ACSPEC(I)-TEMP
  700    CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' CHECK BPARMS')
 1010 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' NUMFRQ = ',I5)
 1020 FORMAT ('BLINE: BASELINE CHN SELECTION = ',2I5,' INVERTED??')
      END
