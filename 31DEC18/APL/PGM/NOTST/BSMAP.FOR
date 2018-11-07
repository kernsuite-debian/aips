LOCAL INCLUDE 'BSMAP.INC'
C                                       Local include for BSMAP
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ,
     *   OLDCNO
      LOGICAL   DOUNIF
      CHARACTER HISCRD(10)*64, NAMEIN*12, CLAIN*6, NAMOUT*12, UNIF*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XUNIF
      REAL      XSIN, XDISIN, XSOUT, XDISO, IMSIZE(2), CELLS(2),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), ERROR, DETIME
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XSOUT,
     *   XDISO, IMSIZE, CELLS, ERROR, XUNIF, DETIME, SEQIN, SEQOUT,
     *   DISKIN, DISKO, DOUNIF, NUMHIS, OLDCNO
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, NAMOUT, UNIF
LOCAL END
      PROGRAM BSMAP
C-----------------------------------------------------------------------
C! Computes image from bi-spectrum analysis of uv data.
C# UV Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2006, 2008, 2015
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
C Experimental Bi-spectrum analysis program.
C  Tim Cornwell                                               July 1986
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'BSMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'BSMAP '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BSIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do it.
      CALL BSSEND (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BSIN gets input parameters for BS.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR
      LOGICAL   T, EQUAL
      INCLUDE 'BSMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2 * UVBFSS
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 20
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
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (4, 1, XUNIF, UNIF)
C                                       Integration time
      IF (DETIME.LE.0.0) DETIME = 1.0/6.0
C                                       Weighting
      DOUNIF = UNIF(1:2) .EQ. 'UN'
      IF (ERROR.EQ.0.0) ERROR = 0.025
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
C                                       Disallow packed uv data
      IF (CATBLK(KINAX).EQ.1) THEN
         JERR = 9
         MSGTXT = 'ERROR: I cannot process packed UV data, use SPLIT'
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check sort order
      EQUAL = ISORT(1:2) .EQ.'TB'
      IF (.NOT.EQUAL) THEN
          WRITE (MSGTXT,1070)
          JERR = 10
          GO TO 990
       END IF
C                                       Set to READ status
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('BSIN: SORT ORDER NOT TB, QUITTING')
      END
      SUBROUTINE BSSEND (IRET)
C-----------------------------------------------------------------------
C   BSSEND sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   INIO, IPTRI, LUNI, INDI, ILENBU, IBIND, I, IA1,
     *   IA2, INCX, BO, VO, NUMVIS, XCOUNT
      LOGICAL   T, F
      REAL      DUM
      CHARACTER IFILE*48
      INCLUDE 'BSMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Init vis file for read.
 20   ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
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
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
         DO 190 I = 1,INIO
            IF (ILOCB.GT.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            CALL BSWORK (NUMVIS, BUFF1(IPTRI+ILOCU),
     *         BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCT), IA1, IA2,
     *         BUFF1(IPTRI+NRPARM), INCX, BUFF2, JBUFSZ, IRET)
C                                       Branch on his return
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
C                                       OK, but no output please
            IPTRI = IPTRI + LREC
C
 190        CONTINUE
         IF (INIO.GT.0) GO TO 100
C                                       Final call to BSWORK.
      NUMVIS = -1
      CALL BSWORK (NUMVIS, DUM, DUM, DUM, IA1, IA2, BUFF1,
     *   INCX, BUFF2, JBUFSZ, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSSEND: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('BSSEND: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('BSSEND: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('BSSEND: BSWORK ERROR',I3)
      END
      SUBROUTINE BSWORK (NUMVIS, U, V, T, IA1, IA2, VIS, INCX, BUFFER,
     *   KBUFSZ, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      T          R    Time in days since 0 IAT on the first day for
C                      which there is data, the julian day corresponding
C                      to this day can be obtained in D   form by:
C                      CALL JULDAY (CATR(KHDOB),XDAY) where XDAY will
C                      be the Julian day number.
C      IA1        I    First antenna number
C      IA2        I    Second antenna number
C      VIS(INCX,*) R    Visibilities in order real, imaginary, weight
C                      (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                      NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON
C      IMSIZE(2)  R    User array.
C      CELLS(2)  R    User array.
C      RA         D    Right ascension (1950) of phase center. (deg)
C      DEC        D    Declination (1950) of phase center. (deg)
C      FREQ       D    Frequency of observation (Hz)
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      CATBLK(256)I    Catalog header record. See Going Aips for
C                      details.
C
C   Output:
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      T          R    Time in same units as input.
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON
C      NUMHIS            I    # history entries (max. 10)
C      HISCRD(NUMHIS)*64 C    History records
C      CATBLK            I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, INCX, KBUFSZ, IRET
      REAL      U, V, T, VIS(INCX,10), BUFFER(*)
      INCLUDE 'BSMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C
      INTEGER MAXANT, MXUBSA, MXVBSA, MAXNX, MAXNY
      PARAMETER (MAXANT=28)
      PARAMETER (MXUBSA=10)
      PARAMETER (MXVBSA=MXUBSA)
      PARAMETER (MAXNX=32)
      PARAMETER (MAXNY=32)
C                                       Arguments
      INTEGER   NANT, NUBSA, NVBSA, NX, NY, NPCALL, IERR, IS1, IS2
      REAL   UCELL(MAXANT,MAXANT), VCELL(MAXANT,MAXANT)
      REAL   BSWT(MAXANT,MAXANT), WAVE
      COMPLEX BSVIS(MAXANT,MAXANT), BS
C                                       Don't try to use these in
C                                       this subroutine, go down
C                                       one level.
      COMPLEX   BSARRY((2*MXUBSA+1)**2*(2*MXVBSA+1)**2)
      REAL      BSWTS ((2*MXUBSA+1)**2*(2*MXVBSA+1)**2)
      INTEGER   BSNUM ((2*MXUBSA+1)**2*(2*MXVBSA+1)**2)
      COMPLEX   BSAINV((2*MXUBSA+1)*(2*MXVBSA+1))
      COMPLEX   BSAWT ((2*MXUBSA+1)*(2*MXVBSA+1))
C
      COMPLEX   WK1((2*MXUBSA+1)*(2*MXVBSA+1))
      COMPLEX   WK2((2*MXUBSA+1)*(2*MXVBSA+1))
C
      COMPLEX   WK3((2*MAXNX+1)*(2*MAXNY+1))
      REAL      BSIMAG((2*MAXNX+1)*(2*MAXNY+1))
      REAL      BSBEAM((2*MAXNX+1)*(2*MAXNY+1))
C
      CHARACTER IMCLAS*6, BMCLAS*6
      REAL   USCALE, VSCALE, WT, S2R, TLAST, UMAX, VMAX, DELT, BANDW
      REAL   IMMAX, IMMIN, BMMAX, BMMIN, VISVAR, FLUX, WT1, WT2
      INTEGER   NNGRID, NSGRID, SIZFFT, ITEMP, ITNX, ITNY
C
      SAVE TLAST, DELT, NPCALL, USCALE, VSCALE, UCELL, VCELL, BSVIS,
     *   BSWT, NX, NY, NUBSA, NVBSA, S2R, UMAX, VMAX, BS, WAVE, VISVAR,
     *   NANT
      DATA TLAST/-1E20/
      DATA NPCALL/0/
      DATA BANDW /1.0/
      DATA NSGRID, NNGRID/0,0/
      DATA IMCLAS /'BSMAP '/
      DATA BMCLAS /'BSBEAM'/
C-----------------------------------------------------------------------
      IRET = -1
C                                       Initialize
      IF (NUMVIS.EQ.1) THEN
C                                       Variance of I data
         VISVAR = 2.0 * ERROR**2
C                                       Length of scan
         DELT = DETIME / (60.0 * 24.0)
         TLAST = T
         WRITE (MSGTXT,2100) DETIME
         CALL MSGWRT (4)
C                                       Number of antennae
         NANT = MAXANT
C                                       Size of image grid, etc
         NX = MIN (NINT((IMSIZE(1) - 1.0) / 2.0) , MAXNX)
         NY = MIN (NINT((IMSIZE(2) - 1.0) / 2.0) , MAXNY)
         WRITE (MSGTXT,1100) 2*NX+1, 2*NY+1
         CALL MSGWRT (4)
C                                       Size of u,v grid, etc
         S2R = 4*ATAN(1.0)/(180.0*3600.0)
         NUBSA = MIN (NX, MXUBSA)
         NVBSA = MIN (NY, MXVBSA)
         UMAX = 1/(S2R*2.0*CELLS(1))
         VMAX = 1/(S2R*2.0*CELLS(2))
C                                       Remember extra padding and
C                                       remember to flip U axis
         ITEMP = SIZFFT(NUBSA)
         USCALE = - REAL(ITEMP) / UMAX
         ITEMP = SIZFFT(NVBSA)
         VSCALE = REAL(ITEMP) / VMAX
C                                       Write into history file
         NUMHIS = 1
         WRITE (HISCRD(NUMHIS),1700) ERROR
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2100) DETIME
         IF (DOUNIF) THEN
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),1800)
            WRITE (MSGTXT,1800)
            CALL MSGWRT (4)
         ELSE
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),1900)
            WRITE (MSGTXT,1900)
            CALL MSGWRT (4)
         END IF
C                                       Reset arrays
         DO 10 IS2 = 1, NANT
            DO 11 IS1 = 1, NANT
               UCELL(IS1,IS2) = 0.0
               VCELL(IS1,IS2) = 0.0
               BSVIS(IS1,IS2) = 0.0
               BSWT(IS1,IS2)  = 0.0
 11         CONTINUE
 10      CONTINUE
      END IF
C-----------------------------------------------------------------------
C                                       Must be end of current
C                                       time-stamp
      IF ((T.GT.(TLAST+1.05*DELT)).OR.(NUMVIS.LT.0)) THEN
         TLAST = T
C                                       Correct for weighting
         DO 90 IS2 = 1, NANT
            DO 80 IS1 = 1, NANT
               IF (BSWT(IS1,IS2).GT.0.0) THEN
                  UCELL(IS1,IS2) = UCELL(IS1,IS2) / BSWT(IS1,IS2)
                  VCELL(IS1,IS2) = VCELL(IS1,IS2) / BSWT(IS1,IS2)
                  BSVIS(IS1,IS2) = BSVIS(IS1,IS2) / BSWT(IS1,IS2)
               END IF
  80        CONTINUE
  90     CONTINUE
C                                       Grid current lot of data
         CALL BSGRID(NPCALL.EQ.0, NANT, UCELL, VCELL, BSVIS, BSWT,
     *      VISVAR, NUBSA, NVBSA, BSARRY, BSWTS, BSNUM, BS, WAVE,
     *      IERR)
         NPCALL = NPCALL + 1
C                                       Reset arrays
         DO 100 IS2 = 1, NANT
            DO 110 IS1 = 1, NANT
               UCELL(IS1,IS2) = 0.0
               VCELL(IS1,IS2) = 0.0
               BSVIS(IS1,IS2) = 0.0
               BSWT(IS1,IS2)  = 0.0
 110        CONTINUE
 100     CONTINUE
C                                       If last point then we should
C                                       make images
         IF (NUMVIS.LT.0) THEN
C                                       How many timestamps were there?
            WRITE (MSGTXT,1200) NPCALL
            CALL MSGWRT (4)
C                                       How many points fell off grid?
            WRITE (MSGTXT,1300) NSGRID, NNGRID
            CALL MSGWRT (4)
C                                       Point source estimation
            FLUX = REAL(BS**(1.0/3.0))
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2000) FLUX
            WRITE (MSGTXT,2000) FLUX
            CALL MSGWRT (4)
C                                       Invert to find Vis. fn.
            CALL BSINV (FLUX, NUBSA, NVBSA, BSARRY, BSWTS, BSNUM,
     *         BSAINV, BSAWT, DOUNIF, WK1, WK2, IERR)
C                                       Check for failure to converge
            IF (IERR.EQ.1) THEN
               IERR = 0
               WRITE (MSGTXT,2200)
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),2200)
            END IF
C                                       FT to get image
            CALL FTUVIM (NX, NY, NUBSA, NVBSA, BSAINV, WK3, BSIMAG,
     *         IMMAX, IMMIN)
C                                       FT to get beam
            CALL FTUVIM (NX, NY, NUBSA, NVBSA, BSAWT,  WK3, BSBEAM,
     *         BMMAX, BMMIN)
C                                       Write out peak flux
            IF (BMMAX.NE.0.0) THEN
               WRITE (MSGTXT,1400) IMMAX/BMMAX
               CALL MSGWRT (4)
            ELSE
               WRITE (MSGTXT,1600)
               CALL MSGWRT (4)
               IERR = 2
               GO TO 999
            END IF
C                                       Renormalise map and beam
            ITNX = 2*NX+1
            ITNY = 2*NY+1
            CALL RENORM (ITNX * ITNY, BSIMAG, 1 / BMMAX)
            CALL RENORM (ITNX * ITNY, BSBEAM, 1 / BMMAX)
C                                       Write out map and beam
            CALL DMPMAP (DISKIN, OLDCNO, BSIMAG, ITNX, ITNY, SOURCE,
     *         CELLS, RA, DEC, FREQ, BANDW, 1.0D0, NAMOUT,
     *         IMCLAS, SEQOUT, DISKO, NUMHIS, HISCRD, BUFFER, KBUFSZ,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1500) IERR
               CALL MSGWRT (4)
               GO TO 999
            END IF
            CALL DMPMAP (DISKIN, OLDCNO, BSBEAM, ITNX, ITNY, SOURCE,
     *         CELLS, RA, DEC, FREQ, BANDW, 0.0D0, NAMOUT,
     *         BMCLAS, SEQOUT, DISKO, NUMHIS, HISCRD, BUFFER, KBUFSZ,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1500) IERR
               CALL MSGWRT (4)
               GO TO 999
            END IF
            GO TO 999
         END IF
C
      END IF
C                                       More of same time-stamp
C                                       so add to current lot
      IF (ABS(T-TLAST).LE.1.05*DELT) THEN
C                                       Calculate weight
            WT1 = MAX(VIS(3,1),0.0)
            WT2 = MAX(VIS(3,2),0.0)
            WT = WT1 + WT2
            IF ((ABS(U).LE.UMAX).AND.(ABS(V).LE.VMAX).AND.
     *          (WT.GT.0.0)) THEN
C                                       Form weighted sum
               BSVIS(IA1,IA2) = BSVIS(IA1,IA2) +
     *            CMPLX(WT1*VIS(1,1)+WT2*VIS(1,2),
     *            WT1*VIS(2,1)+WT2*VIS(2,2))
               UCELL(IA1,IA2) = UCELL(IA1,IA2) + USCALE * U * WT
               VCELL(IA1,IA2) = VCELL(IA1,IA2) + VSCALE * V * WT
               BSWT(IA1,IA2)  = BSWT(IA1,IA2) + WT
               NSGRID = NSGRID + 1
            ELSE
               NNGRID = NNGRID + 1
            END IF
      END IF
C
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('   Size of image = (',I2,',',I2,')')
 1200 FORMAT ('   Number of integration periods = ',I5)
 1300 FORMAT ('   Number of vis. gridded = ',I7,', not gridded = ',I7)
 1400 FORMAT ('   Peak flux = ',1PE11.3,' Jy/beam')
 1500 FORMAT ('BSINV: ERROR',I3,' WRITING ARRAY TO AIPS FILES')
 1600 FORMAT ('BSINV: ERROR: BEAM PEAK IS ZERO')
 1700 FORMAT ('ERROR = ',1PE11.3,' / Specified noise per weight')
 1800 FORMAT ('/ Uniform weighting')
 1900 FORMAT ('/ Natural weighting')
 2000 FORMAT ('/ Estimated point source flux = ',
     *   1PE11.3,' Jy')
 2100 FORMAT ('/ Integration time = ',F7.3,' minutes')
 2200 FORMAT ('/ BSINV DID NOT CONVERGE: DUBIOUS IMAGES ')
      END
      SUBROUTINE BSGRID (DOINIT, NANT, UCELL, VCELL, BSVIS, BSWT,
     *      VISVAR, NUBSA, NVBSA, BSARRY, BSWTS, BSNUM, BS, WAVE, IERR)
C-----------------------------------------------------------------------
C Grids U,V data for Bi-spectrum analysis.
C
C Inputs:
C     DOINIT L     Initialise BSARRY and BSWTS
C     NANT   I     Maximum number of antennas
C     UCELL  R     Array of U coordinates, in units of cells
C     VCELL  R     Array of V coordinates, in units of cells
C     BSVIS  C*8   Array of visibilities.
C     BSWT   R     Array of weights.
C     VISVAR R     Variance of one weights worth of data
C     NUBSA  I     Size of BSARRY
C     NVBSA  I     Size of BSARRY
C
C Output:
C     BSARRY C*8   Gridded data
C     BSWTS  R     Variance
C     BSNUM  I     Number of samples
C     BS     C*8   Averaged Bi-spectrum
C     WAVE   R     Weight for BS
C     IERR   I     Error status
C
C  Programmer: T.J. Cornwell July 1986
C-----------------------------------------------------------------------
      INTEGER MAXANT
      PARAMETER (MAXANT=28)
C                                       Arguments
      LOGICAL   DOINIT
      INTEGER   IERR, NANT
      INTEGER NUBSA, NVBSA
      REAL   VISVAR, WAVE
      REAL   UCELL(MAXANT,*), VCELL(MAXANT,*)
      REAL   BSWT(MAXANT,*)
      COMPLEX BSVIS(MAXANT,MAXANT)
      COMPLEX BSARRY(-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
      INTEGER    BSNUM (-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
      REAL    BSWTS (-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
C                                       Local variables
      INTEGER   IA1, IA2, IA3, IU1, IU2, IV1, IV2
      REAL   VARBS, AMP1, AMP2, AMP3, VAR1, VAR2, VAR3
      COMPLEX BS, BSAVE
      SAVE BSAVE
C
C-----------------------------------------------------------------------
C
      IERR = 0
C                                       Set initial values
      IF (DOINIT) THEN
         WAVE = 0.0
         BSAVE = 0.0
         DO 40 IV2 = -NVBSA, NVBSA
            DO 30 IU2 = -NUBSA, NUBSA
               DO 20 IV1 = -NVBSA, NVBSA
                  DO 10 IU1 = -NUBSA, NUBSA
                     BSARRY(IU1,IV1,IU2,IV2) = 0.0
                     BSWTS (IU1,IV1,IU2,IV2) = 0.0
                     BSNUM(IU1,IV1,IU2,IV2) = 0
  10              CONTINUE
  20           CONTINUE
  30        CONTINUE
  40     CONTINUE
      END IF
C
      DO 130 IA1 = 1, NANT
         DO 120 IA2 = IA1+1, NANT
            IF (BSWT(IA1, IA2).GT.0.0) THEN
C                                       Simple cell summing
            IU1 = NINT(UCELL(IA1,IA2))
            IV1 = NINT(VCELL(IA1,IA2))
C                                       Check for spillover
            IF ((ABS(IU1).LE.NUBSA).AND.(ABS(IV1).LE.NVBSA)) THEN
            AMP1 = ABS(BSVIS(IA1, IA2))**2
            VAR1 = VISVAR/BSWT(IA1, IA2)
            DO 110 IA3 = IA2+1, NANT
               IF ((BSWT(IA2,IA3).GT.0.0).AND.(BSWT(IA1,IA3).GT.0.0))
     *            THEN
C                                       Simple cell summing only
               IU2 = NINT(UCELL(IA2,IA3))
               IV2 = NINT(VCELL(IA2,IA3))
               IF ((ABS(IU2).LE.NUBSA).AND.(ABS(IV2).LE.NVBSA)) THEN
               AMP2 = ABS(BSVIS(IA2, IA3))**2
               AMP3 = ABS(BSVIS(IA1, IA3))**2
               VAR2 = VISVAR/BSWT(IA2, IA3)
               VAR3 = VISVAR/BSWT(IA1, IA3)
C                                       Variance of bi-spectrum
               VARBS = VAR1*AMP2*AMP3 + AMP1*VAR2*AMP3 + AMP1*AMP2*VAR3
     *            +    VAR1*VAR2*AMP3 + AMP1*VAR2*VAR3 + VAR1*AMP2*VAR3
     *            +    VAR1*VAR2*VAR3
               IF(VARBS.GT.0.0) THEN
                  BS = BSVIS(IA1,IA2) * BSVIS(IA2,IA3) *
     *               CONJG(BSVIS(IA1,IA3))
                  BSAVE = BSAVE + BS
                  WAVE  = WAVE  + 1.0
                  BSARRY(IU1,IV1,IU2,IV2) =
     *               BSARRY (IU1,IV1,IU2,IV2) + BS
                  BSWTS(IU1,IV1,IU2,IV2)  =
     *               BSWTS (IU1,IV1,IU2,IV2) + VARBS
                  BSNUM(IU1,IV1,IU2,IV2) =
     *               BSNUM (IU1,IV1,IU2,IV2) + 1
               END IF
               END IF
               END IF
  110       CONTINUE
         END IF
         END IF
  120    CONTINUE
  130 CONTINUE
C
      IF (WAVE.GT.0.0) THEN
         BS = BSAVE/WAVE
      ELSE
         BS = 0.0
      END IF
C
  999 RETURN
      END
      SUBROUTINE BSINV (FLUX, NUBSA, NVBSA, BSARRY, BSWTS, BSNUM,
     *   BSAINV, BSAWT, DOUNIF, DAINV, HAINV, IERR)
C-----------------------------------------------------------------------
C Solves for gridded u,v data from Bi-spectrum.
C Inputs:
C     FLUX   R     Initial guess will be point source of strength FLUX
C     NUBSA  I     Size of BSARRY
C     NVBSA  I     Size of BSARRY
C     BSARRY C*8   Gridded Bi-spectrum
C     BSWTS  R     Weights
C     BSNUM  I     Number of samples
C     DOUNIF L     Apply uniform weighting
C Outputs:
C     BSAINV C*8   Gridded u,v data
C     BSAWT  R     Weights of gridded u,v data
C     IERR   I     Error status
C  Programmer: T.J. Cornwell July 1986
C-----------------------------------------------------------------------
C                                       Arguments
      INTEGER   IERR
      LOGICAL   DOUNIF
      INTEGER NUBSA, NVBSA
      COMPLEX BSARRY(-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
      REAL    BSWTS (-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
      INTEGER    BSNUM (-NUBSA:NUBSA,-NVBSA:NVBSA,-NUBSA:NUBSA,
     *   -NVBSA:NVBSA)
      COMPLEX BSAINV(-NUBSA:NUBSA,-NVBSA:NVBSA)
      COMPLEX BSAWT (-NUBSA:NUBSA,-NVBSA:NVBSA)
      COMPLEX BSAERR
C
      COMPLEX DAINV(-NUBSA:NUBSA,-NVBSA:NVBSA)
      COMPLEX HAINV(-NUBSA:NUBSA,-NVBSA:NVBSA)
C                                       Local variables
      INTEGER   IU1, IU2, IV1, IV2, IU3, IV3
      INTEGER   ITER, NITER
      REAL   S, OLDN, N, EPS, GAIN, SUMWT, FLUX, CHISQ,
     *   ERROR
      COMPLEX BSTMP
      INCLUDE 'INCS:DMSG.INC'
      DATA NITER /100/
      DATA GAIN /0.5/
      DATA EPS / 1E-4/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Correct weighting. Set initial
C                                       guess, etc.
      N = 0.0
      S = 0.0
      SUMWT = 0.0
      DO 9 IV2 = -NVBSA, NVBSA
         DO 8 IU2 = -NUBSA, NUBSA
            DO 7 IV1 = -NVBSA, NVBSA
               DO 6 IU1 = -NUBSA, NUBSA
                  IF (BSNUM(IU1,IV1,IU2,IV2).GT.0.0) THEN
C                                       Correct for summation
                     BSARRY(IU1,IV1,IU2,IV2) =
     *                  BSARRY(IU1,IV1,IU2,IV2)/BSNUM (IU1,IV1,IU2,IV2)
C                                       Convert to inverse variance
                     BSWTS(IU1,IV1,IU2,IV2) =
     *                  BSNUM(IU1,IV1,IU2,IV2)/BSWTS (IU1,IV1,IU2,IV2)
C                                       Initial guess is a point source
C                                       of strength FLUX. Only fill in
C                                       those points with data.
                     BSAINV(IU1,IV1) = FLUX
                     BSAINV(IU2,IV2) = FLUX
                     IU3 = - IU1 - IU2
                     IV3 = - IV1 - IV2
                     BSAINV(IU3,IV3) = FLUX
                  END IF
C                                       Accumulate signal
                  S = S + ABS(BSARRY(IU1,IV1,IU2,IV2))**2 *
     *               BSWTS (IU1,IV1,IU2,IV2)
                  SUMWT = SUMWT + BSWTS(IU1,IV1,IU2,IV2)
C                                       Error in bi-spectrum
                  BSAERR = BSAINV(IU1,IV1) * BSAINV(IU2, IV2) *
     *               BSAINV(IU3,IV3) - BSARRY(IU1,IV1,IU2,IV2)
C                                       Accumulate error
                  N = N + ABS(BSAERR)**2 * BSWTS (IU1,IV1,IU2,IV2)
  6            CONTINUE
  7          CONTINUE
  8       CONTINUE
  9    CONTINUE
C                                       Write initial misfit
      ERROR = (N/SUMWT)**(1.0/6.0)
      WRITE (MSGTXT,1000) ERROR
      CALL MSGWRT (4)
C                                       Initialise arrays
      DO 20 IV1 = -NVBSA, NVBSA
         DO 10 IU1 = -NUBSA, NUBSA
            DAINV(IU1,IV1) = 0.0
            HAINV(IU1,IV1) = 0.0
  10     CONTINUE
  20  CONTINUE
C                                       Iterate to find result
      DO 700 ITER = 1, NITER
C                                       Find suggested changes.
C                                       Also accumulate current errors
      OLDN = N
      N = 0.0
      DO 240 IV2 = -NVBSA, NVBSA
         DO 230 IU2 = -NUBSA, NUBSA
            DO 220 IV1 = -NVBSA, NVBSA
               IV3 = - IV1 - IV2
               IF ((IV3.GE.-NVBSA).AND.(IV3.LE.NVBSA)) THEN
               DO 210 IU1 = -NUBSA, NUBSA
                  IF (BSWTS (IU1,IV1,IU2,IV2).GT.0.0) THEN
                     IU3 = - IU1 - IU2
                     IF ((IU3.GE.-NUBSA).AND.(IU3.LE.NUBSA)) THEN
C                                       Error in bi-spectrum
                        BSAERR = BSAINV(IU1,IV1) * BSAINV(IU2, IV2) *
     *                     BSAINV(IU3,IV3) - BSARRY(IU1,IV1,IU2,IV2)
C                                       Accumulate error
                        N = N + ABS(BSAERR)**2 * BSWTS (IU1,IV1,IU2,IV2)
                        BSTMP = BSAERR * BSWTS(IU1,IV1,IU2,IV2)
C                                       First derivative
                        DAINV(IU1,IV1) = DAINV(IU1,IV1) + BSTMP *
     *                     CONJG(BSAINV(IU2,IV2) * BSAINV(IU3,IV3))
                        DAINV(IU2,IV2) = DAINV(IU2,IV2) + BSTMP *
     *                     CONJG(BSAINV(IU1,IV1) * BSAINV(IU3,IV3))
                        DAINV(IU3,IV3) = DAINV(IU3,IV3) + BSTMP *
     *                     CONJG(BSAINV(IU1,IV1) * BSAINV(IU2,IV2))
C                                       Second derivative
                        HAINV(IU1,IV1) = HAINV(IU1,IV1) +
     *                     ABS(BSAINV(IU2,IV2) * BSAINV(IU3,IV3))**2 *
     *                     BSWTS(IU1,IV1,IU2,IV2)
                        HAINV(IU2,IV2) = HAINV(IU2,IV2) +
     *                     ABS(BSAINV(IU1,IV1) * BSAINV(IU3,IV3))**2 *
     *                     BSWTS(IU1,IV1,IU2,IV2)
                        HAINV(IU3,IV3) = HAINV(IU3,IV3) +
     *                     ABS(BSAINV(IU1,IV1) * BSAINV(IU2,IV2))**2
     *                     * BSWTS(IU1,IV1,IU2,IV2)
C
                     END IF
                  END IF
 210           CONTINUE
            END IF
 220        CONTINUE
 230     CONTINUE
 240  CONTINUE
C                                       Stop if current solution OK or
C                                       if convergence has ceased
      IF ((N.LE.EPS**2*S).OR.ABS(N-OLDN).LT.EPS*OLDN) THEN
         IF (ITER.GT.1) GO TO 900
      END IF
C                                       Take damped step using
C                                       approximate Newton-Raphson
C                                       method.
      DO 520 IV1 = -NVBSA, NVBSA
         DO 510 IU1 = -NUBSA, NUBSA
            IF (CABS(HAINV(IU1,IV1)).NE.0.0) THEN
               BSAINV(IU1,IV1) = BSAINV(IU1,IV1) -
     *            GAIN * DAINV(IU1,IV1)/HAINV(IU1,IV1)
               BSAWT (IU1,IV1) = HAINV(IU1,IV1)
            ELSE
               BSAWT(IU1,IV1) = 0.0
            END IF
            DAINV(IU1,IV1) = 0.0
            HAINV(IU1,IV1) = 0.0
 510     CONTINUE
 520  CONTINUE
C
  700 CONTINUE
C                                       If we got here then the fit
C                                       did not converge
      IERR = 1
C                                       If we jumped directly here
C                                       then the fit did converge
  900 CONTINUE
C                                       Apply weighting
      IF (.NOT.DOUNIF) THEN
         DO 720 IV1 = -NVBSA, NVBSA
            DO 710 IU1 = -NUBSA, NUBSA
               BSAINV(IU1,IV1) = BSAWT(IU1,IV1) * BSAINV(IU1,IV1)
 710        CONTINUE
 720     CONTINUE
      ELSE
         DO 740 IV1 = -NVBSA, NVBSA
            DO 730 IU1 = -NUBSA, NUBSA
               IF (ABS(BSAWT(IU1,IV1)).GT.0.0) THEN
                  BSAWT(IU1,IV1) = 1.0
               END IF
 730        CONTINUE
 740     CONTINUE
      END IF
C
      ITER = MIN (ITER, NITER)
      ERROR = (N/SUMWT)**(1.0/6.0)
      CHISQ = N
      WRITE (MSGTXT,1100) ITER, ERROR
      CALL MSGWRT (4)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('   Initial misfit = ',1PE11.3,' Jy')
 1100 FORMAT ('  ',I3,' Iterations, misfit = ',1PE11.3,' Jy')
      END
      SUBROUTINE FTUVIM (NX, NY, NU, NV, UVFN, TIMFN, IMFN, RMAX, RMIN)
C-----------------------------------------------------------------------
C Transform from U,V plane to image plane using full-plane transform
C Programmer: T.J. Cornwell July 1986
C-----------------------------------------------------------------------
      INTEGER MAXFFT
C                                       MAX. SIZE OF FFT REQUIRED
      PARAMETER (MAXFFT = 128)
C
      INTEGER NX, NY, IX, IY, NU, NV, IU, IV
      COMPLEX  UVFN(-NU:NU,-NV:NV)
      COMPLEX TIMFN(2*NX+1, 2*NY+1)
      REAL     IMFN(2*NX+1, 2*NY+1)
C                                       BUFFER FOR FFTS
      COMPLEX FFTBUF(MAXFFT)
      REAL   RMAX, RMIN, SINC
      INTEGER SIZFFT, FFTSZU, PWR2X, IFFT, FFTSZV, PWR2Y
C-----------------------------------------------------------------------
C
      RMAX = -1E20
      RMIN =  1E20
      FFTSZU = SIZFFT(2*NX)
      FFTSZV = SIZFFT(2*NY)
      PWR2X = NINT(LOG(REAL(FFTSZU))/LOG(2.0))
      PWR2Y = NINT(LOG(REAL(FFTSZV))/LOG(2.0))
C
C DO ROW TRANSFORMS FIRST
C
      DO 20 IV = -NV, NV
C
      DO 10 IFFT = 1, FFTSZU
      FFTBUF(IFFT) = 0.0
   10 CONTINUE
      DO 30 IU = -NU, -1
      FFTBUF(FFTSZU+IU+1) = UVFN(IU, IV)
   30 CONTINUE
      DO 40 IU = 0, NU
      FFTBUF(IU+1) = UVFN(IU, IV)
   40 CONTINUE
C
      CALL FFT(FFTBUF, PWR2X, FFTSZU, -1)
C
      DO 50 IX = -NX, -1
      TIMFN(IX, IV) = FFTBUF(FFTSZU+IX+1)
   50 CONTINUE
      DO 60 IX = 0, NX
      TIMFN(IX, IV) = FFTBUF(IX+1)
   60 CONTINUE
   20 CONTINUE
C
C NOW DO COLUMNS. NOTE BRUTE FORCE TRANSPOSE !
C
      DO 120 IX = -NX, NX
C
      DO 110 IFFT = 1, FFTSZV
      FFTBUF(IFFT) = 0.0
  110 CONTINUE
      DO 130 IV = -NV, -1
      FFTBUF(FFTSZV+IV+1) = TIMFN(IX, IV)
  130 CONTINUE
      DO 140 IV = 0, NV
      FFTBUF(IV+1) = TIMFN(IX, IV)
  140 CONTINUE
C
      CALL FFT(FFTBUF, PWR2Y, FFTSZV, -1)
C
      DO 150 IY = -NY, -1
      IMFN(IX+NX+1, IY+NY+1) =
     *   REAL(FFTBUF(FFTSZV+IY+1))/(REAL(FFTSZU)*REAL(FFTSZV))
     *   / (SINC (REAL(IX)/REAL(2*NX)) * SINC (REAL(IY)/REAL(2*NY)))
      RMAX = MAX(RMAX, IMFN(IX+NX+1, IY+NY+1))
      RMIN = MIN(RMIN, IMFN(IX+NX+1, IY+NY+1))
  150 CONTINUE
      DO 160 IY = 0, NY
      IMFN(IX+NX+1, IY+NY+1) =
     *   REAL(FFTBUF(IY+1))/(REAL(FFTSZU)*REAL(FFTSZV))
     *   / (SINC (REAL(IX)/REAL(2*NX)) * SINC (REAL(IY)/REAL(2*NY)))
      RMAX = MAX(RMAX, IMFN(IX+NX+1, IY+NY+1))
      RMIN = MIN(RMIN, IMFN(IX+NX+1, IY+NY+1))
  160 CONTINUE
  120 CONTINUE
C
      RETURN
      END
      SUBROUTINE FFT (A,M,N,IDIR)
C-----------------------------------------------------------------------
C Does in-place complex FFT using decimation-in-time routine due
C to Cooley, Lewis and Welch
C-----------------------------------------------------------------------
      INTEGER M, N, IDIR
      COMPLEX A(N),U,W,T
      REAL PI
      INTEGER NV2, NM1, I, J, K, L, LE, LE1, IP
C-----------------------------------------------------------------------
      PI = 3.141592653589793*IDIR
      N = 2**M
      NV2 = N/2
      NM1 = N - 1
      J = 1
      DO 7 I = 1,NM1
      IF(I .GE. J) GO TO 5
      T = A(J)
      A(J) = A(I)
      A(I) = T
 5    K = NV2
 6    IF(K .GE. J) GO TO 7
      J = J - K
      K = K/2
      GO TO 6
 7    J = J + K
      DO 20 L = 1,M
      LE = 2**L
      LE1 = LE/2
      U = (1.,0.)
      W = CMPLX(COS(PI/LE1),SIN(PI/LE1))
      DO 20 J = 1,LE1
      DO 10 I = J,N,LE
      IP = I + LE1
      T = A(IP) * U
      A(IP) = A(I) - T
 10   A(I) = A(I) + T
 20   U = U * W
      RETURN
      END
      INTEGER FUNCTION SIZFFT (N)
C
C RETURNS SMALLEST POWER OF 2 GREATER THAN N
C
      INTEGER N, I
C-----------------------------------------------------------------------
C
      SIZFFT = 2
      DO 10 I = 1, 20
      IF (SIZFFT.GE.N) RETURN
      SIZFFT = SIZFFT * 2
   10      CONTINUE
      WRITE (6, *) 'ERROR : CANNOT FIND FFT SIZE FOR N = ',N
      SIZFFT = 4
      RETURN
      END
      SUBROUTINE DMPMAP (DISKIN, OLDCNO, MAPARY, NX, NY, SOURCE, CELLS,
     *   RA, DEC, FREQ, BANDW, STKTYP, NAMOUT,
     *   CLAOUT, SEQOUT, DISKO, NUMHIS, HISCRD, BUFFER, JBUFSZ,
     *   IERR)
C-----------------------------------------------------------------------
C These routines will dump an Fortran array into an AIPS image file.
C The image header is only filled in with the most basic parameters. You
C should call CATIO, etc properly afterwards.
C Inputs:
C  MAPARY(NX,*)      C*4   Map array to be dumped
C  NX                I
C  NY                I
C  SOURCE            C*8   Source name
C  XMSIZE(2)         R     Output image Size
C  CELLS(2)          R     Output cell size in arcseconds
C  RA                D     Right Ascension
C  DEC               D     Declination
C  FREQ              D     Frequency in Hz
C  BANDW             R     Bandwidth in Hz
C  STPTYP            D     Stokes type
C  NAMOUT            C*12  Name of output image
C  CLAOUT            C*6   Class of output image
C  SEQOUT            I     Sequence of output image
C  DISKO             I     Disk of output image
C  NUMHIS            I     Number of history cards
C  HISCRD(*)*64      C     History cards
C
C Output:
C  IERR              I     Error status
C                              = 0 then OK
C                              > 0 then error
C Freely adapted from CANDY by Tim Cornwell, July 1986.
C-----------------------------------------------------------------------
      CHARACTER OLDNAM*12, BLANK*6, NAMOUT*12, CLAOUT*6, DEFNAM*12,
     *   SOURCE*8
      INTEGER   DISKIN, OLDCNO, IRET
C
      INTEGER   IERR
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ
      INTEGER   NX, NY
      REAL      XMSIZE(2), CELLS(2), BANDW, BUFFER(*), MAPARY(NX, *),
     *   INDEF
      CHARACTER HISCRD(10)*64
      DOUBLE PRECISION RA, DEC, FREQ, STKTYP
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       Change this default output name
      DATA DEFNAM, BLANK /'DMP   MAP   ','      '/
C-----------------------------------------------------------------------
      INDEF = FBLANK
C                                       create output file if nec.
      IRET = 0
C                                       Init CATBLK.
      CALL FILL (256, 0, CATBLK)
C                                       Create new file.
C                                       Put values in CATBLK.
      OLDNAM = DEFNAM
      CALL MAKOUT (OLDNAM, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C
      XMSIZE(1) = REAL(NX)
      XMSIZE(2) = REAL(NY)
C                                       Get user modification to CATBLK
      IRET = 4
      CALL DMPHED (SOURCE, CELLS, XMSIZE, RA, DEC, FREQ, BANDW,
     *   STKTYP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1060) IERR
         GO TO 990
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       Set obs. date=current date.
      CATH(KHDOB) = CATH(KHDMP)
      CATH(KHDOB+1) = CATH(KHDMP+1)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL DMPIT (MAPARY, NX, DISKO, NEWCNO, BUFFER,
     *   JBUFSZ, IRET)
C                                       History
      IF (IRET.EQ.0) CALL DMPHIS (SOURCE, CELLS, NAMOUT,
     *   CLAOUT, SEQOUT, DISKO, NEWCNO, NUMHIS, HISCRD, BUFFER,
     *   IRET)
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE DMPIT (MAPARY, NX, DISKO, NEWCNO, BUFFER, JBUFSZ,
     *   IRET)
C-----------------------------------------------------------------------
C   DMPIT actually writes the output file.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET, LUNO,  NX,
     *   NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   LIMO, LIMIT, OBIND, INDO, LIM1
      INTEGER   NEWCNO, DISKO, JBUFSZ, BOTEMP
      REAL   BUFFER(*), OUTMAX, OUTMIN, MAPARY(NX, *)
      CHARACTER IFILE*48
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO /17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Create scratch file.
C                                       Open vis file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
C                                       Setup for I/O
 30   NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
C                                       Setup for looping
      LIM1 = MAX (1, CATBLK(KINAX))
      LIM2 = MAX (1, CATBLK(KINAX+1))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      IPOS(1) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = I4
                  DO 300 I3 = 1,LIM3
      IPOS(3) = I3
C                                       Init output file.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3),
     *   BOTEMP, IRET)
      BOO = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFER, JBUFSZ,
     *   BOO, IRET)
      IF (IRET.EQ.0) GO TO 120
         WRITE (MSGTXT,1100) 'WRIT', IRET
         GO TO 990
 120  DO 250 I2 = 1,LIM2
         IPOS(2) = I2
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFFER, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.EQ.0) GO TO 180
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
C                                       Fill in array
 180     OBIND = OBIND + 1
         CALL RCOPY (LIM1, MAPARY(1,I2), BUFFER(OBIND))
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
         DO 200 I1 = OBIND,LIMIT
            OUTMAX = MAX (OUTMAX, BUFFER(I1))
            OUTMIN = MIN (OUTMIN, BUFFER(I1))
 200        CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFFER, OBIND, IRET)
      IF (IRET.EQ.0) GO TO 260
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
C                                       Update CATBLK.
 260  CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 300
         WRITE (MSGTXT,1260) IRET
         GO TO 990
 300  CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
C
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('DMPIT: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('DMPIT: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('DMPIT: ',A4,' ERROR',I3)
 1260 FORMAT ('DMPIT: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE DMPHIS (SOURCE, CELLS, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *   NEWCNO, NUMHIS, HISCRD, BUFFER, IERR)
C-----------------------------------------------------------------------
C   DMPHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER   NAMOUT*12, CLAOUT*6, ATIME*8, ADATE*12, SOURCE*8
      INTEGER   LUN, IERR, I
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, TIME(3), DATE(3)
      REAL      XMSIZE(2), CELLS(2), BUFFER(*)
      CHARACTER LABEL*8, HILINE*72, HISCRD(10)*64
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CELLSIZE
      WRITE (HILINE,2002) TSKNAM, CELLS
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1011) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DMPHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ''',A8,'''')
 2001 FORMAT (A6,' IMSIZE = ', 2F8.0)
 2002 FORMAT (A6,' CELLSIZE = ',2F10.5)
 2004 FORMAT (A6,' RELEASE = ''',A7,' ''')
      END
      SUBROUTINE DMPHED (SOURCE, CELLS, XMSIZE, RA, DEC, FREQ,
     *   BANDW, STKTYP, IRET)
C-----------------------------------------------------------------------
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C  RA                D     Right Ascension
C  DEC               D     Declination
C  FREQ              D     Frequency in Hz
C  BANDW             R     Bandwidth in Hz
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER BLANK*8, UNITS(10)*8, ATYPES(7)*8, SOURCE*8
      INTEGER   I, NAXIS, IRET, IROUND, INDEX
      REAL      CELLS(2), XMSIZE(2), BANDW
      DOUBLE PRECISION RA, DEC, FREQ, STKTYP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA BLANK /' '/
C                                       User definable values
C                                       Two R   words with 4 char. ea.
      DATA UNITS /'JY/BEAM ',9*'        '/
C                                       Number of axes and types.
C                                       (Set for two axes = Ra, Dec.)
      DATA NAXIS  /4/
      DATA ATYPES /'RA---SIN', 'DEC--SIN',
     *   'STOKES  ', 'FREQ    ', 3*'        '/
C-----------------------------------------------------------------------
C                                       Set output units.
      CALL CHR2H (8, UNITS(1), 1, CATH(KHBUN))
C                                       Fill axis arrays.
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = 1
C                                       Init. increment.
         CATR(KRCIC+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = 1.0
C                                       Init. ref value.
         CATD(KDCRV+I-1) = 0.0D0
C                                       Fill axis type from
C                                       ATYPES or BLANK.
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
         IF (I.LE.NAXIS)
     *      CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
C                                       Blank Random axes.
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
      CATBLK(KINAX) = MAX (IROUND (XMSIZE(1)), 1)
      CATBLK(KINAX+1) = MAX (IROUND (XMSIZE(2)), 1)
C                                       Axis values
      CATD(KDCRV)   = RA
      CATD(KDCRV+1) = DEC
C                                       Stokes
      CATD(KDCRV+2) = STKTYP
      CATR(KRCIC+2) = 1.0
C                                       Frequency
      CATD(KDCRV+3) = FREQ
      CATR(KRCIC+3) = BANDW
C                                       Assume CELLS in sec.
C                                       NOTE: Ra decreases with
C                                       grid number.
      CATR(KRCIC) = - CELLS(1) / 3600.
      CATR(KRCIC+1) = CELLS(2) / 3600.
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Observation date.
      CALL CHR2H (8, BLANK, 1, CATH(KHDOB))
C                                       Telescope.
      CALL CHR2H (8, BLANK, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, BLANK, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, BLANK, 1, CATH(KHOBS))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = 0
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 0.0
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE RENORM (N, A, FACT)
C-----------------------------------------------------------------------
C Normalise an array
C-----------------------------------------------------------------------
      INTEGER N, I
      REAL   A(N)
      REAL   FACT
C-----------------------------------------------------------------------
      DO 10 I = 1,N
         A(I) = A(I) * FACT
  10  CONTINUE
C
      RETURN
      END
      REAL FUNCTION SINC(X)
C-----------------------------------------------------------------------
C   Compute Sin(x) / x function
C-----------------------------------------------------------------------
      REAL X, PI
      DATA PI /3.141592653589793/
C-----------------------------------------------------------------------
      IF (X.EQ.0.0) THEN
         SINC = 1.0
      ELSE
         SINC = SIN(PI*X) / (PI*X)
         END IF
 999  RETURN
      END
