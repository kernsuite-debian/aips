        SUBROUTINE ZARGS
C-----------------------------------------------------------------------
C! sends command to/from the ARGS TV device
C# Z2 TV-IO
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
C   Called by ZARGXF to write to and read from the ARGS TV device.  Has
C   entries ZARG01 through ZARG16
C   VMS version.
C-----------------------------------------------------------------------
      PARAMETER XMA = 'C000'X, YMA = 'A000'X, YDA = 'E000'X
      PARAMETER XCL = '1700'X, YCL = '2700'X, CUR = '3D00'X
      PARAMETER CLS = '3400'X, DEF = '5500'X, WIN = '0600'X
      PARAMETER ARF = '9700'X, ZDI = '2800'X, LVT = '1800'X
      PARAMETER DEV = '3C40'X, VSR = '2401'X, ZOM = '5001'X
      PARAMETER LDE = '8400'X, RDE = '8500'X, EXC = '1600'X
      PARAMETER TXT = '3600'X, FPW = '6934'X, FPR = '6B34'X
      PARAMETER SXR = '38FF'X, SYR = '3AFF'X
      PARAMETER XOR = (XMA.OR.256), YOR = (YMA.OR.256)
      PARAMETER RG1 = '01C0'X, RG2 = '0008'X, RG3 = '0007'X
      PARAMETER TWO = '0101'X, NXY = '03FF'X, NCZ = '00FF'X
      PARAMETER NCS = '0007'X, NCH = '00FF'X
      EXTERNAL IO$_WRITELBLK, IO$_READLBLK
      INTEGER   SYS$ASSIGN, RC, CHAN, X(*), Y(*), Z(*), I, J, K, L, M,
     *   N, P, INI(13), CTL(10), SET(6), GET(516), VEC(2,1024), T(512),
     *   EXE(35)
      CHARACTER A*(*)
      DATA INI /VSR,RG1,RG2,RG3, WIN,0,0,1024,1024, XOR,YOR,ZOM,TWO/
      DATA EXE /
     *   '7C00'X, '101F'X,             ! 1000        DEV*   INC
     *   '7D00'X, '101C'X,             ! 1002        CUR*   POS
     *   '8A00'X, '101D'X, '1013'X,    ! 1004        SETVV  POS+1, XCL+1
     *   '8A00'X, '101E'X, '1015'X,    ! 1007        SETVV  POS+2, YCL+1
     *   '7C00'X, '101F'X,             ! 100A  LOOP  DEV*   INC
     *   '8800'X, '1020'X, '1013'X,    ! 100C        ADDVV  INC+1, XCL+1
     *   '8800'X, '1021'X, '1015'X,    ! 100F        ADDVV  INC+2, YCL+1
     *   '1700'X, '0000'X,             ! 1012  XCL   XCL    0, 0
     *   '2700'X, '0000'X,             ! 1014  YCL   YCL    0, 0
     *   '8F30'X, '0078'X, '1022'X,    ! 1016        ANDLV  $78, SWI
     *   '8D00'X, '100A'X,             ! 1019        BRAZE  LOOP
     *   '0001'X,                      ! 101B        STP
     *   '0000'X, '0000'X, '0000'X,    ! 101C  POS   WORD   $00, 0, 0
     *   '0044'X, '0000'X, '0000'X,    ! 101F  INC   WORD   $44, 0, 0
     *   '0000'X/                      ! 1022  SWI   WORD   0
C-----------------------------------------------------------------------
      RC = SYS$ASSIGN ('IDA0:',CHAN,,)
      IF (.NOT.RC) CALL SYS$EXIT (%VAL(RC))
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,INI,%VAL(13),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG01
C                                       ENTRY CLEAR
C-----------------------------------------------------------------------
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,CLS,%VAL(1),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG02 (N)
C                                       ENTRY ERASE (N)
C-----------------------------------------------------------------------
      SET(1) = '3501'X
      SET(2) = 256 * N
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(2),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG03 (X, Y, N)
C                                       ENTRY LINE (X,Y,N)
C-----------------------------------------------------------------------
      VEC(1,1) = XMA .OR. (X(1).AND.NXY)
      VEC(2,1) = YMA .OR. (Y(1).AND.NXY)
      DO I = 2,N
         VEC(1,I) = XMA .OR. (X(I).AND.NXY)
         VEC(2,I) = YDA .OR. (Y(I).AND.NXY)
         END DO
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,VEC,%VAL(2*N),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG04 (K, L, A)
C                                       ENTRY TEXT (K,L,A)
C-----------------------------------------------------------------------
      J = (LEN(A)+1) / 2 * 2
      SET(1) = XMA .OR. (K.AND.NXY)
      SET(2) = YMA .OR. (L.AND.NXY)
      SET(3) = TXT .OR. (J.AND.NCH)
      SET(4) = '0100'X
      CALL SYS$QIOW (,%VAL(CHAN), IO$_WRITELBLK,,,, CTL,, SET,
     *   %VAL(4),,)
      CALL SYS$QIOW (, %VAL(CHAN), IO$_WRITELBLK,,,, CTL,, %REF(A),
     *   %VAL(J/2),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG05 (T)
C                                       ENTRY VIDEO (T)
C-----------------------------------------------------------------------
      SET(1) = LVT .OR. 255
      SET(2) = 0
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(2),,)
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,T,%VAL(512),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG06 (N)
C                                       ENTRY COLOUR (N)
C-----------------------------------------------------------------------
      SET(1) = ZDI .OR. (N.AND.NCZ)
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(1),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG07 (K, L, N)
C                                       ENTRY FILLA (K, L, N)
C-----------------------------------------------------------------------
      SET(1) = XMA .OR. (K.AND.NXY)
      SET(2) = YMA .OR. (L.AND.NXY)
      SET(3) = ARF
      SET(4) = (N.AND.NCZ)
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(4),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG09 (K, L, N)
C                                       ENTRY BALL (K,L,N)
C-----------------------------------------------------------------------
      CTL(5) = DEV .OR. 4
      CALL SYS$QIOW (, %VAL(CHAN), IO$_READLBLK,,,, CTL, %VAL(5), GET,
     *   %VAL(7),,)
      K = GET(5)
      L = GET(6)
      N = GET(7)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG10 (N)
C                                       ENTRY LIGHTS (N)
C-----------------------------------------------------------------------
      SET(1) = DEV .OR. 5
      SET(2) = N
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(2),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG11 (K, L, N)
C                                       ENTRY CURSOR (K,L,N)
C-----------------------------------------------------------------------
      SET(1) = VSR
      SET(2) = RG1 .OR. ((N.AND.NCS) * 8)
      SET(3) = RG2
      SET(4) = RG3
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(4),,)
      SET(1) = XCL
      SET(2) = K
      SET(3) = YCL
      SET(4) = L
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(4),,)
      GO TO 999
C-----------------------------------------------------------------------
C     ENTRY RETIC (K,L,N)
C-----------------------------------------------------------------------
C     SET(1) = LDE
C     SET(2) = 35
C     SET(3) = '1000'X
C     CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(3),,)
C     CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,EXE,%VAL(35),,)
C     SET(1) = EXC
C     SET(2) = '1000'X
C     CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(2),,)
C     CTL(5) = RDE
C     CTL(6) = 1
C     CTL(7) = '1022'X
C     CALL SYS$QIOW (, %VAL(CHAN), IO$_READLBLK,,,, CTL, %VAL(7), GET,
C    *   %VAL(5), %VAL(65536),)
C     N = GET(5)
      ENTRY ZARG12 (K, L)
C                                       ENTRY RETIC (K,L)
C-----------------------------------------------------------------------
      CTL(5) = CUR
      CALL SYS$QIOW (, %VAL(CHAN), IO$_READLBLK,,,, CTL, %VAL(5), GET,
     *   %VAL(6),,)
      K = GET(5)
      L = GET(6)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG13 (K, L, Z, N, M)
C                                       ENTRY LOAD (K, L, Z, N, M)
C-----------------------------------------------------------------------
      P = N * M
      SET(1) = FPW
      SET(2) = K
      SET(3) = L
      SET(4) = N
      SET(5) = M
      SET(6) = P
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(6),,)
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,Z,%VAL(P),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG14 (K, L, Z, N, M)
C                                       ENTRY DAOL (K,L,Z,N,M)
C-----------------------------------------------------------------------
      P = N * M
      CTL(5) = FPR
      CTL(6) = K
      CTL(7) = L
      CTL(8) = N
      CTL(9) = M
      CTL(10) = P
      P = P + 4
      CALL SYS$QIOW (, %VAL(CHAN), IO$_READLBLK,,,, CTL, %VAL(10), GET,
     *   %VAL(P),,)
      DO P = 1,N
         Z(P) = GET(P + 4)
         ENDDO
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG15 (K, L, N)
C                                       ENTRY ZOOM (K,L,N)
C-----------------------------------------------------------------------
      SET(1) = XMA .OR. (K.AND.NXY)
      SET(2) = YMA .OR. (L.AND.NXY)
      SET(3) = ZOM
      SET(4) = TWO * (2 * N + 1)
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(4),,)
      GO TO 999
C-----------------------------------------------------------------------
      ENTRY ZARG16 (K, L)
C                                       ENTRY SCROLL (K,L)
C-----------------------------------------------------------------------
      SET(1) = SXR
      SET(2) = K
      SET(3) = SYR
      SET(4) = L
      CALL SYS$QIOW (,%VAL(CHAN),IO$_WRITELBLK,,,,CTL,,SET,%VAL(4),,)
C
 999  RETURN
      END
