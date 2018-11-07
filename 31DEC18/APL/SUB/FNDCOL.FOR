      SUBROUTINE FNDCOL (NKEY, KEYS, LKEY, LORDER, BUFFER, KOLS, IERR)
C-----------------------------------------------------------------------
C! locvates logical column numbers for given titles in a Table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   FNDCOL is used with AIPS Table extension files.  It locates the
C   logical column number(s) which are titled with specified strings.
C   Inputs:
C      NKEY    I            Number columns to be found
C      KEYS    C*24(NKEY)   Column titles to locate
C      LKEY    I            Number characters to check in each of KEYS
C                           (legal values 1 through 24)
C      LORDER  L            T => logical order desired, else phys.
C   In/out:
C      BUFFER  I(>512)      TABINI/TABIO buffer/ header/ work area
C   Output:
C      KOLS    I(NKEY)      Logical column numbers: 0 => none,
C                                 -1 => more than one (!)
C      IERR    I            Error code: 0 => ok, 1 - 10 from ZFIO
C                                >10 = 10 + # of failed columns
C-----------------------------------------------------------------------
      CHARACTER KEYS(*)*(*)
      INTEGER   NKEY, LKEY, BUFFER(*), KOLS(*), IERR
      LOGICAL   LORDER
C
      INTEGER   LK, NK, NWPL, NLPR, I, L, N, M, IP, IRNO
      CHARACTER RTEST*24
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       fix up input
      IERR = 0
      IF (NKEY.LE.0) GO TO 999
      LK = LKEY
      IF (LK.LT.1) LK = 1
      IF (LK.GT.24) LK = 24
      NK = NKEY
      M = BUFFER(10)
      IF (NK.GT.M) NK = M
C                                       prepare IO
      NWPL = 6
      NLPR = 256 / NWPL
      CALL FILL (NKEY, 0, KOLS)
      IRNO = BUFFER(47) - 1
C                                       look for keywords in titles
      DO 25 I = 1,M
C                                       read new record
         IF (MOD(I-1,NLPR).NE.0) GO TO 10
            IP = 257 - NWPL
            IRNO = IRNO + 1
            CALL ZFIO ('READ', BUFFER(81), BUFFER(82), IRNO,
     *         BUFFER(257), IERR)
            IF (IERR.EQ.0) GO TO 10
               WRITE (MSGTXT,1000) IERR
               CALL MSGWRT (8)
               GO TO 999
C                                       Title to scratch area
 10      IP = IP + NWPL
         CALL H2CHR (24, 1, BUFFER(IP), RTEST)
C                                       look for it
         DO 20 N = 1,NKEY
            IF (KEYS(N)(1:LK).NE.RTEST(1:LK)) GO TO 20
            IF (KOLS(N).NE.0) KOLS(N) = -1
            IF (KOLS(N).EQ.0) KOLS(N) = I
 20         CONTINUE
 25      CONTINUE
C                                       convert phys cols # to logical
      IERR = 10 + NKEY
      IF (.NOT.LORDER) GO TO 40
         DO 35 N = 1,NKEY
            IF (KOLS(N).LE.0) GO TO 35
               L = KOLS(N)
               KOLS(N) = 0
               DO 30 I = 1,M
                  IF (BUFFER(128+I).NE.L) GO TO 30
                     KOLS(N) = I
                     IERR = IERR - 1
                     GO TO 35
 30               CONTINUE
 35         CONTINUE
         GO TO 50
C                                       Check physical order
 40   CONTINUE
         DO 45 N = 1,NKEY
            IF (KOLS(N).GT.0) IERR = IERR - 1
 45         CONTINUE
 50   IF (IERR.LE.10) IERR = 0
C                                       Fix up I/O pointers
      BUFFER(77) = 3
      BUFFER(80) = 3
      BUFFER(78) = IRNO
      DO 60 I = 1,M
         IF (BUFFER(128+I).NE.M) GO TO 60
            BUFFER(79) = I
            GO TO 999
 60      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDCOL: ERROR',I5,' READING TABLE FILE TITLES RECORD')
      END
