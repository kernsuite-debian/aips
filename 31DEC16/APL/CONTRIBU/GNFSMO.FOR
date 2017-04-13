      SUBROUTINE GNFSMO (TIMSMO, DISK, CNO, VER, LUN, BUFF, IERR)
C-----------------------------------------------------------------------
C! Boxcar smooths and ASCAL solution (GA) file.
C# EXT-appl UV Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   GNFSMO does a boxcar smoothing of gain amplitudes of a ASCAL
C   solution file (GA) with a time window specified for each antenna.
C   Input:
C      TIMSMO(30) R    Smoothing times (hrs) for each antenna.
C      DISK       I    Disk number on which the gain file resides.
C      CNO        I    Cat. slot number of GA file.
C      VER        I    GA file version number.
C      LUN        I    LUN to use.
C      BUFF(768)  I    Work buffer.
C   From COMMON /UVHDR/
C      JLOCF      I    Pointer in header to frequency.
C      JLOCS      I    Pointer in header to stokes.
C   Output:
C      IERR       I    Return error code. 0=>OK. otherwise error.
C   NOTE:  /UVHDR/ should be filled with a call to UVPGET on the uv data
C   header.  Also /HDRVAL/ should be filled with a call to VHDRIN.
C-----------------------------------------------------------------------
      INTEGER   BUFF(768), DISK, CNO, VER, IERR, I, NANT, NIF,
     *   CATBLK(256), NUMSLN(2000), IANT, IF, IFLG(4), BFLG(48),
     *   SPARGA(228), JLREC, LUN, NREC, IND, IBITWD, INEXT, KV,
     *   NSOLN, LIMSLN, ISOLN, NBIT, NWORD, IREF, NUMAN(51), NEXT
      REAL      TIME(2000), AMP(2000), CATR(256),
     *   T1, T2, GARI(2,28,2), AMPL, FACTOR, TIMSMO(30),
     *   SCALE, AMPSMO(2000)
      COMPLEX   GAIN(28,2)
      LOGICAL   FLAGS(32)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /GAREC/ GAIN, IFLG, BFLG, KV, T1, T2, IREF, SPARGA
      EQUIVALENCE (CATBLK, CATR, TIME),     (GAIN, GARI)
      DATA JLREC, LIMSLN /512,2000/
C-----------------------------------------------------------------------
C                                       Get CATBLK
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', AMP, IERR)
      IF (.NOT. ((IERR.GE.1) .AND. (IERR.LE.3))) GO TO 50
         WRITE (MSGTXT,1000) IERR
         GO TO 990
C                                       Get number of IFs.
 50   NIF = 2
      IF (CATBLK(KINAX+JLOCS).LT.2) NIF = 1
C                                       Read antenna header to set NANT.
C                                       Use GETNAN to get number of
C                                       antennas in subarray 1.
      CALL GETNAN (DISK, CNO, CATBLK, LUN, BUFF, NUMAN, IERR)
      NANT = NUMAN(2)
C                                       On error assume 28 antennas.
      IF (IERR.NE.0) NANT = 28
C                                       Open gain file.
      CALL EXTINI ('WRIT', 'GA', DISK, CNO, VER, CATBLK, LUN, IND,
     *   JLREC, NREC, BUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of solutions.
      NSOLN = BUFF(4)
C                                       Loop thru Ifs.
      DO 220 IF = 1,NIF
C                                       Loop thru antennas.
         DO 210 IANT = 1,NANT
C                                       Check if smoothing wanted.
            SCALE = TIMSMO(IANT) * 3600.
C                                       If smoothing time .lt. 1 sec
C                                       skip smoothing this antenna.
            IF (SCALE.LT.(1.0/3600.0)) GO TO 210
C                                       Read gains.
            NEXT = 1
            DO 100 ISOLN = 1,NSOLN
C                                       Read.
               CALL EXTIO ('READ', LUN, IND, ISOLN, GAIN, BUFF, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Check ANT/IF flag
               NBIT = IANT + (IF-1) * 28 - 1
               NWORD = NBIT / NBITWD + 1
               NBIT = MOD (NBIT, NBITWD) + 1
C                                       Crunch bits.
               IBITWD = NBITWD
               CALL LG2BIT (IBITWD, FLAGS, IFLG(NWORD), -1)
C                                       Data good.
               IF (FLAGS(NBIT)) GO TO 100
                  NUMSLN(NEXT) = ISOLN
                  TIME(NEXT) = 0.5 * (T1 + T2)
                  AMP(NEXT) = SQRT ((GARI(1,IANT,IF)**2) +
     *               (GARI(2,IANT,IF)**2))
                  NEXT = NEXT + 1
C                                       Too many visibilities
                  IF (NEXT.LE.LIMSLN) GO TO 100
                     WRITE (MSGTXT,1050) NEXT, LIMSLN, IANT, IF
                     CALL MSGWRT (6)
                     GO TO 110
 100           CONTINUE
 110        NEXT = NEXT - 1
C                                       See if data obtained.
            IF (NEXT.LE.0) GO TO 210
C                                       Call smoother.
               INEXT = NEXT
               CALL BOXSMO (SCALE, TIME, AMP, INEXT, AMPSMO)
C                                       Update gain files.
               DO 200 I = 1,NEXT
                  ISOLN = NUMSLN(I)
C                                       Read.
                  CALL EXTIO ('READ', LUN, IND, ISOLN, GAIN, BUFF,
     *               IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       Get amplitude.
                  AMPL = SQRT ((GARI(1,IANT,IF)**2) +
     *               (GARI(2,IANT,IF)**2))
                  FACTOR = AMPSMO(I) / AMPL
C                                       Correct gain.
                  GAIN(IANT,IF) = GAIN(IANT,IF) * FACTOR
C                                       Write back.
                  CALL EXTIO ('WRIT', LUN, IND, ISOLN, GAIN, BUFF, IERR)
                  IF (IERR.NE.0) GO TO 999
 200              CONTINUE
 210        CONTINUE
 220     CONTINUE
C                                       Close GA file.
      CALL EXTIO ('CLOS', LUN, IND, I, GAIN, BUFF, IERR)
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GNFSMO: ERROR',I5,' READING CATBLK')
 1050 FORMAT ('GNFSMO: TOO MANY GAINS=',I5,',MAX=',I5,',ANT=',I3,
     *   ',IF=',I2)
      END
