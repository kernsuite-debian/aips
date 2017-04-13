      SUBROUTINE CATN2L (OPER, INB, OUT)
C-----------------------------------------------------------------------
C! convert header from local binary to network/FITS binary & back
C# util catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1999, 2005
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
C   Coverts a header or regular or image catalog form from/to network
C   form (i.e. FITS) to/from local.
C   Inputs:
C      OPER   C*4      'N2L', 'L2N' for regular
C                      'N2LI', 'L2NI' for image catalog
C      INB     I(256)   in buffer
C   Outputs:
C      OUT    I(256)   out buffer
C-----------------------------------------------------------------------
      CHARACTER OPER*(*)
      INTEGER   INB(256), OUT(256)
C
      INTEGER   CATI(256), I, IP, LP
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER CHAR8*8, CHAR12*12, CHAR6*6, CHAR2*2, CHAR4*4
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PHDR.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATI, CATR, CATH, CATD)
C-----------------------------------------------------------------------
C                                       simple case: local = FITS
      IF ((NBITWD.EQ.32) .AND. (BYTFLP.EQ.0) .AND. (SPFRMT.EQ.1) .AND.
     *   (DPFRMT.EQ.1)) THEN
         CALL COPY (256, INB, OUT)
C                                       local to network
      ELSE IF (OPER(:3).EQ.'L2N') THEN
         CALL COPY (256, INB, CATI)
         CALL FXHDEX (CATI)
         CALL H2CHR (8, 1, CATH(KHOBJ), CHAR8)
         CALL ZCLC8 (8, CHAR8, 1, OUT)
         CALL H2CHR (8, 1, CATH(KHTEL), CHAR8)
         CALL ZCLC8 (8, CHAR8, 9, OUT)
         CALL H2CHR (8, 1, CATH(KHINS), CHAR8)
         CALL ZCLC8 (8, CHAR8, 17, OUT)
         CALL H2CHR (8, 1, CATH(KHOBS), CHAR8)
         CALL ZCLC8 (8, CHAR8, 25, OUT)
         CALL H2CHR (8, 1, CATH(KHDOB), CHAR8)
         CALL ZCLC8 (8, CHAR8, 33, OUT)
         CALL H2CHR (8, 1, CATH(KHDMP), CHAR8)
         CALL ZCLC8 (8, CHAR8, 41, OUT)
         CALL H2CHR (8, 1, CATH(KHBUN), CHAR8)
         CALL ZCLC8 (8, CHAR8, 49, OUT)
         IP = 57
         LP = KHPTP
         DO 10 I = 1,KIPTPN
            CALL H2CHR (8, 1, CATH(LP), CHAR8)
            CALL ZCLC8 (8, CHAR8, IP, OUT)
            LP = LP + 2
            IP = IP + 8
 10         CONTINUE
         LP = KHCTP
         DO 15 I = 1,KICTPN
            CALL H2CHR (8, 1, CATH(LP), CHAR8)
            CALL ZCLC8 (8, CHAR8, IP, OUT)
            LP = LP + 2
            IP = IP + 8
 15         CONTINUE
         CALL ZRLR64 (KICTPN, 29, CATD(KDCRV), OUT)
C                                       CRP, CRT, EPO, DMX, DMN, BLK
         IP = 3 * KICTPN + 4
         CALL ZRLR32 (IP, 71, CATR(KRCIC), OUT)
C                                       GCN, PCN, DIM, NAX, IMS
         IP = 3 + KICTPN + 1
         CALL ZILI32 (IP, CATI(KIGCN), 96, OUT)
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHAR12)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHAR6)
         CALL H2CHR (2, KHPTYO, CATH(KHPTY), CHAR2)
         CALL ZCLC8 (12, CHAR12, 425, OUT)
         CALL ZCLC8 (6, CHAR6, 437, OUT)
         CALL ZCLC8 (2, CHAR2, 443, OUT)
C                                       ALT
         CALL ZILI32 (2, CATI(KIIMU), 112, OUT)
C                                       BMN, BPA
         CALL ZRLR32 (3, 114, CATR(KRBMJ), OUT)
C                                       TYP
         CALL ZILI32 (2, CATI(KINIT), 117, OUT)
C                                       RST, ARV
         CALL ZRLR64 (3, 61, CATD(KDODE), OUT)
         CALL ZRLR32 (1, 127, CATR(KRARP), OUT)
C                                       CNO, WIN, DEP, COR, TRA, PLT,
C                                       OTH
         IF (OPER.EQ.'L2NI') THEN
            CALL ZILI32 (42, CATI(IIVOL), 128, OUT)
            CALL ZRLR32 (2, 119, CATR(IRRAN), OUT)
         ELSE
            CALL ZRLR64 (1, 60, CATD(KDORA), OUT)
            CALL ZRLR32 (2, 128, CATR(KRXSH), OUT)
            CALL ZILI32 (KIEXTN, CATI(KIVER), 130+KIEXTN, OUT)
            LP = KHEXT
            IP = 4 * 130 - 3
            DO 20 I = 1,KIEXTN
               CALL H2CHR (2, 1, CATH(LP), CHAR2)
               CHAR4 = CHAR2
               CALL ZCLC8 (4, CHAR4, IP, OUT)
               IP = IP + 4
               LP = LP + 1
 20            CONTINUE
            END IF
C         CALL ZRLR32 (1, 170, CATR(KRCOK), OUT)
C         CALL ZRLR64 (10, 86, CATD(KDLON), OUT)
C         CALL ZRLR32 (49, 191, CATR(KRPCM), OUT)
C         CALL ZILI32 (KIRESN, CATI(KIRES), 240, OUT)
         I = 130 + 2 * KIEXTN
         CALL ZRLR32 (1, I, CATR(KRWTN), OUT)
C                                       imaging type, xpoff, ypoff
         I = I + 1
         CALL ZILI32 (1, CATI(KIITY), I, OUT)
         I = I + 1
         CALL ZRLR32 (2, I, CATR(KRXPO), OUT)
         I = I + 2
         CALL ZILI32 (KIRESN, CATI(KIRES), I, OUT)
C                                       Network  -> local
      ELSE
         CALL ZC8CL (8, 1, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHOBJ))
         CALL ZC8CL (8, 9, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHTEL))
         CALL ZC8CL (8, 17, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHINS))
         CALL ZC8CL (8, 25, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHOBS))
         CALL ZC8CL (8, 33, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHDOB))
         CALL ZC8CL (8, 41, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHDMP))
         CALL ZC8CL (8, 49, INB, CHAR8)
         CALL CHR2H (8, CHAR8, 1, CATH(KHBUN))
         IP = 57
         LP = KHPTP
         DO 110 I = 1,KIPTPN
            CALL ZC8CL (8, IP, INB, CHAR8)
            CALL CHR2H (8, CHAR8, 1, CATH(LP))
            LP = LP + 2
            IP = IP + 8
 110        CONTINUE
         LP = KHCTP
         DO 115 I = 1,KICTPN
            CALL ZC8CL (8, IP, INB, CHAR8)
            CALL CHR2H (8, CHAR8, 1, CATH(LP))
            LP = LP + 2
            IP = IP + 8
 115        CONTINUE
         CALL ZR64RL (KICTPN, 29, INB, CATD(KDCRV))
C                                       CRP, CRT, EPO, DMX, DMN, BLK
         IP = 3 * KICTPN + 4
         CALL ZR32RL (IP, 71, INB, CATR(KRCIC))
C                                       GCN, PCN, DIM, NAX, IMS
         IP = 3 + KICTPN + 1
         CALL ZI32IL (IP, 96, INB, CATI(KIGCN))
         CALL ZC8CL (12, 425, INB, CHAR12)
         CALL ZC8CL (6, 437, INB, CHAR6)
         CALL ZC8CL (2, 443, INB, CHAR2)
         CALL CHR2H (12, CHAR12, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CHAR6, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, CHAR2, KHPTYO, CATH(KHPTY))
C                                       ALT
         CALL ZI32IL (2, 112, INB, CATI(KIIMU))
C                                       BMN, BPA
         CALL ZR32RL (3, 114, INB, CATR(KRBMJ))
C                                       TYP
         CALL ZI32IL (2, 117, INB, CATI(KINIT))
C                                       RST, ARV
         CALL ZR64RL (3, 61, INB, CATD(KDODE))
         CALL ZR32RL (1, 127, INB, CATR(KRARP))
C                                       CNO, WIN, DEP, COR, TRA, PLT,
C                                       OTH
         IF (OPER.EQ.'N2LI') THEN
            CALL ZI32IL (42, 128, INB, CATI(IIVOL))
            CALL ZR32RL (2, 119, INB, CATR(IRRAN))
         ELSE
            CALL ZR64RL (1, 60, INB, CATD(KDORA))
            CALL ZR32RL (2, 128, INB, CATR(KRXSH))
            CALL ZI32IL (KIEXTN, 130+KIEXTN, INB, CATI(KIVER))
            LP = KHEXT
            IP = 4 * 130 - 3
            DO 120 I = 1,KIEXTN
               CALL ZC8CL (4, IP, INB, CHAR4)
               CHAR2 = CHAR4
               CALL CHR2H (2, CHAR2, 1, CATH(LP))
               IP = IP + 4
               LP = LP + 1
 120           CONTINUE
            END IF
C         CALL ZR32RL (1, 170, INB, CATR(KRCOK))
C         CALL ZR64RL (10, 86, INB, CATD(KDLON))
C         CALL ZR32RL (49, 191, INB, CATR(KRPCM))
C         CALL ZI32IL (KIRESN, 240, INB, CATI(KIRES))
         I = 130 + 2 * KIEXTN
         CALL ZR32RL (1, I, INB, CATR(KRWTN))
C                                       imaging type, xpoff, ypoff
         I = I + 1
         CALL ZI32IL (1, I, INB, CATI(KIITY))
         I = I + 1
         CALL ZR32RL (2, I, INB, CATR(KRXPO))
         I = I + 2
         CALL ZI32IL (KIRESN, I, INB, CATI(KIRES))
         CALL COPY (256, CATI, OUT)
         END IF
C
 999  RETURN
      END
