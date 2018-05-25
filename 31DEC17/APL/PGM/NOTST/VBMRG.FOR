LOCAL INCLUDE 'VBMRG.INC'
C                                       Local include for VBMRG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATBLK(256), CATIN(256), SEQIN, SEQOUT, DISKIN, DISKO,
     *   NUMHIS,  JBUFSZ, LRECU, ILOCWT
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(4)*64
      REAL      XSIN, XDISIN, XSOUT, XDISO, YINC,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      LOGICAL   ISCOMP
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, YINC,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, LRECU, ISCOMP, ILOCWT
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      COMMON /MAPHDR/ CATBLK
      COMMON /OLDHDR/ CATIN
LOCAL END
      PROGRAM VBMRG
C-----------------------------------------------------------------------
C! Merge uv records, remove redundant data
C# UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2006, 2008-2009, 2015
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
C   VBMRG consolidates a VLBI uv data set.  Data must be initially in
C   baseline-time order ('BT') and the last visibility with the highest
C   weight for each correlator will be passes.  All data from the same
C   visibility will be combined into the same record.  Thus VBMRG will
C   remove redundant correlations of the same data and combine polar-
C   ization data into the same record.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      YINC           YINC          Integration time (sec) min=0.2
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'VBMRG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'VBMRG '/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      TSKNAM = PRGM
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 0
      CALL VMRGIN (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL VMRGUV (IRET)
      IF (IRET.EQ.0) CALL VMRGHI
C                                       Close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE VMRGIN (JERR)
C-----------------------------------------------------------------------
C   VMRGIN gets input parameters for VBMRG and creates an output file
C   if necessary.
C   Inputs:
C      See prologue comments in VBMRG for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, BLANK*6, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   OLDCNO, IROUND, JERR, NPARM, IERR
      REAL      CATR(256)
      INCLUDE 'VBMRG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA BLANK /'      '/
C-----------------------------------------------------------------------
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 15
      CALL GTPARM (TSKNAM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         JERR = 8
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      YINC = MAX (0.2, YINC) / 86400.0
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Restart AIPS.
      IERR = 0
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (6)
         END IF
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
C                                       Copy old CATBLK
      CALL COPY (256, CATBLK, CATIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Disallow packed uv data
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
         LRECU = LREC * 3 - 2 * NRPARM
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
      ELSE
         LRECU = LREC
         END IF
C                                       Check if sort order 'BT'
      IF (ISORT.NE.'BT') THEN
         WRITE (MSGTXT,1050) ISORT
         JERR = 5
         GO TO 990
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT(1:12), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT(1:6), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1060) IERR
         JERR = 8
         GO TO 990
C                                        Update existing CATBLK
      ELSE IF (IERR.EQ.2) THEN
         FRW(NCFILE+1) = 2
         CALL CATIO ('WRIT', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
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
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
c
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('VMRGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' RESTARTING AIPS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('SORT ORDER ',A2,' NOT BT AS REQUIRED')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1070 FORMAT ('VMRGIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE VMRGUV (IRET)
C-----------------------------------------------------------------------
C   VMRGUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OFILE*48, IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, BO, VO, NUMVIS, XCOUNT,
     *   NCORU
      LOGICAL   T, F
      INCLUDE 'VBMRG.INC'
      REAL       VBUFF(UVBFSL)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VO, BO /0,1/
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1080) IRET
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
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
      NCORU = (LRECU - NRPARM) / 3
C                                       Loop
C                                       Read vis. record.
 95   CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
      ELSE IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS+1
C                                      Call user routine.
            IF (ISCOMP) THEN
               CALL RCOPY (NRPARM, BUFF1(IPTRI), VBUFF)
               CALL ZUVXPN (NCORU, BUFF1(IPTRI+NRPaRM),
     *            BUFF1(IPTRI+ILOCWT), VBUFF(NRPARM+1))
               CALL VMERGE (NUMVIS, BUFF1(IPTRI+ILOCT),
     *            VBUFF(1+NRPARM), VBUFF(1), IRET)
            ELSE
               CALL VMERGE (NUMVIS, BUFF1(IPTRI+ILOCT),
     *            BUFF1(IPTRI+NRPARM), BUFF1(IPTRI), IRET)
               END IF
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
C                                       Copy to output.
               IF (ISCOMP) THEN
                  CALL RCOPY (NRPARM, VBUFF, BUFF2(IPTRO))
                  CALL ZUVPAK (NCORu, VBUFF(NRPARM+1),
     *               BUFF2(IPTRO+ILOCWT), BUFF2(IPTRO+NRPARM))
               ELSE
                  CALL RCOPY (LRECU, BUFF1(IPTRI), BUFF2(IPTRO))
                  END IF
               IPTRO = IPTRO + LREC
               NIOUT = NIOUT + 1
               END IF
            IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1160) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         GO TO 95
         END IF
C                                       Final call to VMERGE.
      NUMVIS = -1
      CALL VMERGE (NUMVIS, BUFF2(IPTRO+ILOCT), BUFF2(IPTRO+NRPARM),
     *   BUFF2(IPTRO), IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
         XCOUNT = XCOUNT + 1
         IPTRO = IPTRO+LRECO
         NIOUT = NIOUT + 1
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1160) IRET
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
 1060 FORMAT ('VMRGUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1070 FORMAT ('VMRGUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1080 FORMAT ('VMRGUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1090 FORMAT ('VMRGUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('VMRGUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('VMRGUV: VMERGE ERROR',I3)
 1160 FORMAT ('VMRGUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE VMRGHI
C-----------------------------------------------------------------------
C  VMRGHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, NONOT, IERR
      LOGICAL   T
      INCLUDE 'VBMRG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1,LUN2/27,28/
      DATA  T /.TRUE./
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (5)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1010) TSKNAM
         HILINE = LABEL // HISCRD(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 20
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 30
         WRITE (MSGTXT,1020) IERR
         IF (IERR.EQ.4) WRITE (MSGTXT,1021)
         CALL MSGWRT (6)
C                                        Update CATBLK.
 30   CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VMRGHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1020 FORMAT ('VMRGHI: ERROR',I3,' COPYING ANTENNA FILE')
 1021 FORMAT ('VMRGHI: NO ANTENNA FILES COPIED DESPITE INPUT HEADER')
      END
      SUBROUTINE VMERGE (NUMVIS, T, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C  VMERGE compresses a (VLBI) dataset.  The last, highest weight value
C  for each correlator is kept.
C  Inputs:
C     NUMVIS   I      Visibility number, -1 => final call, no data
C                     passed but allows any operations to be completed.
C                     Data sent back will be wirtten to output file.
C     T        R      Time in days since 0 on the first day for which
C                     there is data.
C     RPARM    R(*)   Random parameter array which includes U,V,W etc
C                     but also any other random parameters. followed by
C                     VIS data in 1 LRECU long array
C     VIS      R(3,*) Visibilities in order real, imaginary, weight (Jy)
C  Inputs from COMMON
C     NRPARM   I      # random parameters.
C     NCOR     I      # correlators
C     CATBLK   I(256) Catalog header record. See [DOC]HEADER for details
C  Output:
C     RPARM    R(*)    Modified random parameter array. NB U,V,W,
C                      time and baseline should not be modified in RPARM
C                      followed by VIS data in 1 LRECU long array
C     IRET     I       Return code  -1 => don't write
C                                0 => OK
C                               >0 => error, terminate.
C  Output in COMMON
C     NUMHIS     I    # history entries (max. 10)
C     HISCRD(16) R   History records
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      T, VIS(3,*), RPARM(*)
C                                       XVBLEN = Length of record buffer
      INTEGER   LUNSS, JERR, INDEX, IDAY, MCOR, I, XCOUNT, XVBLEN,
     *   ITEMP, ANVER
      LOGICAL   SAMEB
      INCLUDE 'INCS:ZPBUFSZ.INC'
      PARAMETER (XVBLEN = UVBFSL)
      REAL   VBUFF(XVBLEN), TEMP, CATR(256), AMP1, AMP2, CT, TLAST
      DOUBLE PRECISION CATD(128), X8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VBMRG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      SAVE VBUFF, XCOUNT, TLAST, MCOR
      DATA LUNSS /27/
C-----------------------------------------------------------------------
      IRET = -1
C                                       initial call
      IF (NUMVIS.EQ.1) THEN
         XCOUNT = 0
C                                        Get Time - UTC
         DATUTC = 0.0
         ANVER = 1
         CALL ANTINI ('READ', VBUFF, FVOL(NCFILE), FCNO(NCFILE), ANVER,
     *      CATIN, LUNSS, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ,RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
         CALL TABIO ('CLOS', 1, IANRNO, VBUFF, VBUFF, JERR)
         CT = T - DATUTC
         IDAY = CT
         X8 = (CT-IDAY) / YINC
         TLAST = IDAY + DINT (X8) * YINC + YINC
C                                        First vis., copy to buffer.
         CALL RCOPY (LRECU, RPARM(1), VBUFF(1))
         MCOR = (LRECU - NRPARM) / 3
C                                        Check that LREC .le. XVBLEN
         IF (LRECU.GT.XVBLEN) THEN
            ITEMP = XVBLEN
            WRITE (MSGTXT,1100) LRECU, ITEMP
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
         END IF
C                                        Next datum.
      IF (NUMVIS.GE.1) THEN
         CT = T - DATUTC
C                                        Check time and baseline.
         IF (ILOCB.GE.0) THEN
            SAMEB = RPARM(1+ILOCB).EQ.VBUFF(1+ILOCB)
         ELSE
            SAMEB = (RPARM(1+ILOCA1).EQ.VBUFF(1+ILOCA1)) .AND.
     *         (RPARM(1+ILOCA2).EQ.VBUFF(1+ILOCA2)) .AND.
     *         (RPARM(1+ILOCSA).EQ.VBUFF(1+ILOCSA))
            END IF
         IF ((CT.LE.TLAST) .AND. (SAMEB)) THEN
C                                        "Old" vis. - compress
            DO 20 I = 1,MCOR
               INDEX = NRPARM + (I - 1)*3 + 1
C                                       Compare amplitudes iff
C                                       equal weights.
               IF (VIS(3,I).EQ.VBUFF(INDEX+2)) THEN
                  AMP1 = VBUFF(INDEX)*VBUFF(INDEX) + VBUFF(INDEX+1)*
     *               VBUFF(INDEX+1)
                  AMP2 = VIS(1,I)*VIS(1,I) + VIS(2,I)*VIS(2,I)
                  IF (AMP1.LT.0.25*AMP2) VBUFF(INDEX+2) = -10
                  IF (AMP2.LT.0.25*AMP1) VIS(3,I) = -10
                  END IF
               IF (VIS(3,I).GE.VBUFF(INDEX+2)) THEN
                  VBUFF(INDEX) = VIS(1,I)
                  VBUFF(INDEX+1) = VIS(2,I)
                  VBUFF(INDEX+2) = VIS(3,I)
                  END IF
 20            CONTINUE
C                                        "New" vis. - write
         ELSE
            XCOUNT = XCOUNT + 1
            DO 30 I = 1,LRECU
               TEMP = VBUFF(I)
               VBUFF(I) = RPARM(I)
               RPARM(I) = TEMP
 30            CONTINUE
            IDAY = CT
            X8 = (CT-IDAY) / YINC
            TLAST = IDAY + DINT(X8) * YINC + YINC
            IRET = 0
            END IF
C                                         Last datum.
      ELSE
         XCOUNT = XCOUNT + 1
         CALL RCOPY (LRECU, VBUFF, RPARM)
C                                        Write no. written to history.
         NUMHIS = 1
         WRITE (HISCRD,1999) XCOUNT
         WRITE (MSGTXT,1999) XCOUNT
         CALL MSGWRT (5)
         IRET = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('RECORD LENGTH= ',I5,' GREATER THAN ',I5)
 1999 FORMAT (I10,' Visibility records written')
      END
