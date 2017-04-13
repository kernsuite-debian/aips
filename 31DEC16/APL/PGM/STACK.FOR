LOCAL INCLUDE 'STACK.INC'
      INCLUDE 'INCS:PMAD.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XNMOUT(3), XCLOUT(2), XOPCOD,
     *   XINFIL(12)
      REAL      SEQIN, DSKIN, SEQ2IN, SEQOUT, DSKOUT, BLC(7), TRC(7)
      CHARACTER NAMIN*12, CLSIN*6, NAMOUT*12, CLSOUT*6, OPCODE*4,
     *   INFILE*48
      INTEGER   CATOLD(256), SEQ1, SEQ2, IVOL, ICNO, SEQO, OVOL, OCNO,
     *   LUN, IND, SCRTCH(256), IWIN(4), IVOL0, ICNO0
      REAL      RMAX, RMIN, RBUFF(MABFSS), WTS(32768)
      LOGICAL   WASBLK
C
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, SEQ2IN, XNMOUT,
     *   XCLOUT, SEQOUT, DSKOUT, BLC, TRC, XOPCOD, XINFIL
      COMMON /PSTACK/ SCRTCH, CATOLD, SEQ1, SEQ2, IVOL, ICNO, SEQO,
     *   OVOL, OCNO, LUN, IND, IWIN, IVOL0, ICNO0, RMAX, RMIN, WASBLK
      COMMON /BSTACK/ RBUFF, WTS
      COMMON /CSTACK/ NAMIN, CLSIN, NAMOUT, CLSOUT, OPCODE, INFILE
LOCAL END
      PROGRAM STACK
C-----------------------------------------------------------------------
C! Task to stack image planes
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2015
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
C   STACK will do a weighted average of a number of images or planes of
C   one image cube.
C   Inputs:  (from AIPS)
C      INNAME    C*12   The file name for all input images
C      INCLASS   C*6    The file class of all input images
C      INSEQ     I      The sequence number of the first input image
C      INDISK    I      The disk volume number of the input images
C                       If zero all disks are searched for each sequence
C                       number
C      OUTNAME   C*12   The name of the output image file
C      OUTCLASS  C*6    The class of the output image file
C      OUTSEQ    I      The sequence number for the output image file
C      OUTDISK   I      The disk volume number for the output image
C      BLC       I(7)   The bottom left corner pixel coordinate
C      TRC       I(7)   The top right pixel coordinate
C      OPCODE    C*4    '    ' average INSEQ through IN2SEQ
C                       'CUBE' average planes BLC(3) through TRC(3)
C-----------------------------------------------------------------------
      INCLUDE 'STACK.INC'
      INTEGER   IRET, NX, NY, NZ, NWORDS, IERR
      CHARACTER PRGNAM*6
      LONGINT   PIMAGE, PWEIGH, PINPU
      REAL      IMAGE(2), WEIGHT(2), INPU(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'STACK'/
C-----------------------------------------------------------------------
C                                       init and create output file
      CALL STACKI (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get memory
      NX = IWIN(3) - IWIN(1) + 1
      NY = IWIN(4) - IWIN(2) + 1
      IF (OPCODE.EQ.'MEDC') THEN
         NZ = TRC(3) - BLC(3) + 1.5
      ELSE IF (OPCODE.EQ.'MEDI') THEN
         NZ = SEQ2 - SEQ1 + 1
      ELSE
         NZ = 1
         ENDIF
      NWORDS = (NX * NY * NZ - 1) / 1024 + 3
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGE, PIMAGE, IRET)
      IF ((IRET.EQ.0) .AND. (NZ.EQ.1)) THEN
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WEIGHT, PWEIGH, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, INPU,
     *      PINPU, IRET)
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET NEEDED DYNAMIC MEMORY'
         CALL MSGWRT (8)
      ELSE IF (OPCODE.EQ.'MEDC') THEN
         CALL STACKM (NX, NY, NZ, IMAGE(1+PIMAGE), IRET)
      ELSE IF (OPCODE.EQ.'MEDI') THEN
         CALL STACKN (NX, NY, NZ, IMAGE(1+PIMAGE), IRET)
      ELSE IF (OPCODE.EQ.'CUBE') THEN
         CALL STACKC (NX, NY, INPU(1+PINPU), IMAGE(1+PIMAGE),
     *      WEIGHT(1+PWEIGH), IRET)
      ELSE
         CALL STACKS (NX, NY, INPU(1+PINPU), IMAGE(1+PIMAGE),
     *      WEIGHT(1+PWEIGH), IRET)
         END IF
C                                       history
      IF (IRET.EQ.0) CALL STACKH (NX, NY, NZ, IMAGE(1+PIMAGE),
     *   WEIGHT(1+PWEIGH), IRET)
C                                       Release AIPS if wait state.
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, IMAGE, PIMAGE, IERR)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE STACKI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Init the task and crete the output image
C   Inputs:
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     Error code: > 0 => quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INCLUDE 'STACK.INC'
      INTEGER   INPRMS, IERR, IROUND, IUSER, I, J, JTRIM, TXLUN, TXIND,
     *   KBP
      CHARACTER MTYPE*2, CBLANK*6, LINE*72
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CBLANK /' '/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      INPRMS = 42
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING ADVERBS FROM AIPS'
         CALL MSGWRT (7)
         END IF
C
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLSOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (48, 1, XINFIL, INFILE)
C                                       Set initial values.
      SEQ1 = IROUND (SEQIN)
      SEQ2 = IROUND (SEQ2IN)
      IVOL = IROUND (DSKIN)
      SEQO = IROUND (SEQOUT)
      OVOL = IROUND (DSKOUT)
      IUSER = NLUSER
      LUN = 17
C                                       Open source file.
      MTYPE = 'MA'
      CALL MAPOPN ('READ', IVOL, NAMIN, CLSIN, SEQ1, MTYPE, IUSER,
     *   LUN, IND, ICNO, CATOLD, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING FIRST INPUT FILE'
         GO TO 990
         END IF
      NCFILE = 2
      FVOL(NCFILE) = IVOL
      FCNO(NCFILE) = ICNO
      FRW(NCFILE) = 0
      IVOL0 = IVOL
      ICNO0 = ICNO
C                                       Copy to output header
      CALL COPY (256, CATOLD, CATBLK)
C                                       Set default values BLC, TRC.
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'SETTING INPUT IMAGE WINDOW'
         GO TO 990
         END IF
      IF ((OPCODE.EQ.'CUBE')  .OR. (OPCODE.EQ.'MEDC')) THEN
         SEQ2 = IROUND (TRC(3))
         CALL RCOPY (4, BLC(4), TRC(4))
      ELSE
         CALL RCOPY (5, BLC(3), TRC(3))
         END IF
      I = SEQ2 - SEQ1 + 1
      IF (I.LT.2) THEN
         MSGTXT = 'YOU HAVE NOT ASKED TO AVERAGE ANYTHING'
         GO TO 990
         END IF
      CALL RFILL (I, 0.0, WTS)
C                                       Build new file cat name.
      CALL MAKOUT (NAMIN, CLSIN, SEQ1, CBLANK, NAMOUT, CLSOUT, SEQO)
C                                       Set header values needed
C                                       by MCREAT.
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQO
C                                       Create new cataloged file.
      CALL MCREAT (OVOL, OCNO, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING OUTPUT FILE'
         GO TO 990
         END IF
      SEQO = CATBLK(KIIMS)
      FVOL(1) = OVOL
      FCNO(1) = OCNO
      FRW(1) = 2
C                                       do not Copy header keywords
      IWIN(1) = IROUND (BLC(1))
      IWIN(2) = IROUND (BLC(2))
      IWIN(3) = IROUND (TRC(1))
      IWIN(4) = IROUND (TRC(2))
      RMAX = -1.E12
      RMIN = 1.E12
      WASBLK = .FALSE.
C                                       image weights
      IF (INFILE.NE.' ') THEN
         TXLUN = 3
         CALL ZTXOPN ('READ', TXLUN, TXIND, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING TEXT FILE'
            GO TO 990
            END IF
         I = 0
 20      CALL ZTXIO ('READ', TXLUN, TXIND, LINE, IERR)
         IF (IERR.EQ.0) THEN
            J = JTRIM (LINE)
            IF ((J.GT.0) .AND. (LINE(:1).NE.'#')) THEN
               KBP = 1
               CALL GETNUM (LINE, J, KBP, X)
               IF (X.NE.DBLANK) THEN
                  I = I + 1
                  WTS(I) = X
                  END IF
               END IF
            GO TO 20
         ELSE IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1000) IERR, 'READING TEXT FILE'
            GO TO 990
            END IF
         CALL ZTXCLS (TXLUN, TXIND, IERR)
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKI ERROR',I4,' ON A',A)
      END
      SUBROUTINE STACKC (NX, NY, IMAGE, SUM, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Stacks over BLC(3) to TRC(3)
C   Inputs:
C      NX       I      Number X pixels in usable image plane
C      NY       I      Number Y pixels in usable image plane
C   Outputs:
C      IMAGE    R(*)   Work memory for input images
C      SUM      R(*)   Output mean image
C      WEIGHT   R(*)   Work memory for sum weights
C      IRET     I      > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,NY), SUM(NX,NY), WEIGHT(NX,NY)
C
      INCLUDE 'STACK.INC'
C
      REAL      VALUE, GVALUE
      INTEGER   IBLKOF, IDEPTH(5), I, IBIND, NBY, NPTS, NUMKEY, KEYTYP,
     *   LOCS, MSGSAV, IROUND, IUSER, IX, IY, IP, IP1, IP2
      CHARACTER KEYWRD*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      NBY = 2 * MABFSS
      IUSER = NLUSER
C                                       header value?
      NUMKEY = 1
      KEYWRD = 'ACTNOISE'
      MSGSUP = 32000
      CALL CATKEY ('READ', IVOL, ICNO, KEYWRD, NUMKEY, LOCS, GVALUE,
     *   KEYTYP, SCRTCH, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GVALUE = 0.0
C                                       Init values for loop.
      NPTS = NX * NY
      CALL RFILL (NPTS, 0.0, SUM)
      CALL RFILL (NPTS, 0.0, WEIGHT)
      IP1 = IROUND (BLC(3))
      IP2 = IROUND (TRC(3))
      IDEPTH(1) = IROUND (BLC(3))
      IDEPTH(2) = IROUND (BLC(4))
      IDEPTH(3) = IROUND (BLC(5))
      IDEPTH(4) = IROUND (BLC(6))
      IDEPTH(5) = IROUND (BLC(7))
C                                       Loop for all possible planes.
      DO 100 IP = IP1,IP2
         IDEPTH(1) = IP
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('READ', LUN, IND, CATOLD(KINAX), CATOLD(KINAX+1),
     *      IWIN, RBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O FOR INPUT PLANE', IP
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       read input plane
         DO 20 IY = 1,NY
            CALL MDISK ('READ', LUN, IND, RBUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT PLANE', IP
               GO TO 990
               END IF
            CALL RCOPY (NX, RBUFF(IBIND), IMAGE(1,IY))
 20         CONTINUE
C                                       find a weight
         I = IP - IP1 + 1
         IF (WTS(I).LE.0.0) THEN
            CALL GETRMS (NX, NY, IMAGE, VALUE)
            IF (VALUE.LE.0.0) VALUE = GVALUE
            IF (VALUE.LE.0.0) THEN
               WRITE (MSGTXT,1020) IP
               CALL MSGWRT (7)
               GO TO 100
               END IF
            WTS(I) = 1.0 / (VALUE * VALUE)
            END IF
C                                       sum into SUM and WEIGHT
         DO 50 IY = 1,NY
            DO 40 IX = 1,NX
               IF (IMAGE(IX,IY).NE.FBLANK) THEN
                  SUM(IX,IY) = SUM(IX,IY) + WTS(I) * IMAGE(IX,IY)
                  WEIGHT(IX,IY) = WEIGHT(IX,IY) + WTS(I)
                  END IF
 40            CONTINUE
 50         CONTINUE
 100     CONTINUE
C                                       close inout file
      CALL MAPCLS ('READ', IVOL, ICNO, LUN, IND, CATOLD, .FALSE.,
     *   SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE INPUT IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE - 1
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKC ERROR:',I5,' ON ',A,I6)
 1020 FORMAT ('STACKC PLANE=',I6,' NO WEIGHT FOUND')
      END
      SUBROUTINE STACKS (NX, NY, IMAGE, SUM, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   Stacks over INSEQ to IN2SEQ
C   Inputs:
C      NX       I      Number X pixels in usable image plane
C      NY       I      Number Y pixels in usable image plane
C   Outputs:
C      IMAGE    R(*)   Work memory for input images
C      SUM      R(*)   Output mean image
C      WEIGHT   R(*)   Work memory for sum weights
C      IRET     I      > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,NY), SUM(NX,NY), WEIGHT(NX,NY)
C
      INCLUDE 'STACK.INC'
C
      REAL      VALUE
      INTEGER   IBLKOF, IDEPTH(5), I, IBIND, NBY, NPTS, NUMKEY, KEYTYP,
     *   LOCS, MSGSAV, IROUND, IUSER, IX, IY, ISEQ
      CHARACTER KEYWRD*8, MTYPE*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      NBY = 2 * MABFSS
      IUSER = NLUSER
C
      NPTS = NX * NY
C                                       Init values for loop.
      CALL RFILL (NPTS, 0.0, SUM)
      CALL RFILL (NPTS, 0.0, WEIGHT)
      IDEPTH(1) = IROUND (BLC(3))
      IDEPTH(2) = IROUND (BLC(4))
      IDEPTH(3) = IROUND (BLC(5))
      IDEPTH(4) = IROUND (BLC(6))
      IDEPTH(5) = IROUND (BLC(7))
C                                       Loop for all possible planes.
      DO 100 ISEQ = SEQ1,SEQ2
         IF (ISEQ.NE.SEQ1) THEN
            MTYPE = 'MA'
            IVOL = DSKIN
            CALL MAPOPN ('READ', IVOL, NAMIN, CLSIN, ISEQ, MTYPE, IUSER,
     *         LUN, IND, ICNO, CATOLD, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE', ISEQ
               IF (IRET.NE.3) GO TO 990
               WRITE (MSGTXT,1010) ISEQ
               CALL MSGWRT (7)
               IRET = 0
               GO TO 100
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = IVOL
            FCNO(NCFILE) = ICNO
            FRW(NCFILE) = 0
            END IF
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('READ', LUN, IND, CATOLD(KINAX), CATOLD(KINAX+1),
     *      IWIN, RBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O FOR INPUT FILE', ISEQ
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       read input plane
         DO 20 IY = 1,NY
            CALL MDISK ('READ', LUN, IND, RBUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE', ISEQ
               GO TO 990
               END IF
            CALL RCOPY (NX, RBUFF(IBIND), IMAGE(1,IY))
 20         CONTINUE
C                                       find a weight
         I = ISEQ - SEQ1 + 1
         IF (WTS(I).LE.0.0) THEN
C                                       header value?
            NUMKEY = 1
            KEYWRD = 'ACTNOISE'
            MSGSUP = 32000
            CALL CATKEY ('READ', IVOL, ICNO, KEYWRD, NUMKEY, LOCS,
     *         VALUE, KEYTYP, SCRTCH, IRET)
            MSGSUP = MSGSAV
            IF ((IRET.NE.0) .OR. (VALUE.LE.0.0)) CALL GETRMS (NX, NY,
     *         IMAGE, VALUE)
            IF (VALUE.LE.0.0) THEN
               WRITE (MSGTXT,1020) ISEQ
               CALL MSGWRT (7)
               GO TO 90
               END IF
            WTS(I) = 1.0 / (VALUE * VALUE)
            END IF
C                                       sum into SUM and WEIGHT
         DO 50 IY = 1,NY
            DO 40 IX = 1,NX
               IF (IMAGE(IX,IY).NE.FBLANK) THEN
                  SUM(IX,IY) = SUM(IX,IY) + WTS(I) * IMAGE(IX,IY)
                  WEIGHT(IX,IY) = WEIGHT(IX,IY) + WTS(I)
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       close inout file
 90      CALL MAPCLS ('READ', IVOL, ICNO, LUN, IND, CATOLD, .FALSE.,
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE INPUT IMAGE', ISEQ
            GO TO 990
            END IF
         NCFILE = NCFILE - 1
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKS ERROR:',I5,' ON ',A,I6)
 1010 FORMAT ('STACKS INSEQ=',I6,' NOT FOUND')
 1020 FORMAT ('STACKS INSEQ=',I6,' NO WEIGHT FOUND')
      END
      SUBROUTINE GETRMS (NX, NY, IMAGE, RMS)
C-----------------------------------------------------------------------
C   Robust RMS determination on an image plane
C   Inputs:
C      NX      I      Number X pixels in IMAGE
C      NY      I      Number Y pixels in IMAGE
C      IMAGE   R(*)   Image values
C   Outputs:
C      RMS     R      Image true RMS (0 -> failed)
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      IMAGE(*), RMS
C
      INTEGER   NITER
      PARAMETER (NITER=8)
C
      INTEGER   NP, IT, IP
      REAL      WS(NITER), VP, VM
      DOUBLE PRECISION SV, SSV, NV
      INCLUDE 'INCS:DDCH.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
C                                       ROBUST mean, rms
      NP = NX * NY
      VP = 1.E5
      VM = -VP
      DO 120 IT = 1,NITER
         SV = 0.0D0
         SSV = 0.0D0
         NV = 0.0D0
         DO 110 IP = 1,NP
            IF ((IMAGE(IP).NE.FBLANK) .AND. (IMAGE(IP).NE.0.0)) THEN
               IF ((IMAGE(IP).GT.VM) .AND. (IMAGE(IP).LT.VP)) THEN
                  SV = SV + IMAGE(IP)
                  SSV = SSV + IMAGE(IP) * IMAGE(IP)
                  NV = NV + 1.0D0
                  END IF
               END IF
 110        CONTINUE
         IF (NV.GT.0.0D0) THEN
            SV = SV / NV
            SSV = SSV / NV - SV * SV
            SSV = SQRT (MAX (0.0D0, SSV))
            IF (IT.LT.NITER) THEN
               VP = SV + WS(IT+1) * SSV
               VM = SV - WS(IT+1) * SSV
               END IF
         ELSE
            VP = 1.E4
            VM = -1.E4
            END IF
 120     CONTINUE
      RMS = SSV
C
 999  RETURN
      END
      SUBROUTINE STACKN (NX, NY, NZ, IMAGE, IRET)
C-----------------------------------------------------------------------
C   Stacks over INSEQ to IN2SEQ
C   Inputs:
C      NX       I      Number X pixels in usable image plane
C      NY       I      Number Y pixels in usable image plane
C      NZ       I      Number Z pixels in usable image cube
C   Outputs:
C      IMAGE    R(*)   Work memory for input images
C                      first planeis output image
C      IRET     I      > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'STACK.INC'
C
      REAL      MEDIAN
      INTEGER   IBLKOF, IDEPTH(5), I, IBIND, NBY, NPTS, MSGSAV, IROUND,
     *   IUSER, IX, IY, IZ, ISEQ, IS
      CHARACTER MTYPE*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      NBY = 2 * MABFSS
      IUSER = NLUSER
C
      NPTS = NX * NY * NZ
      CALL RFILL (NPTS, FBLANK, IMAGE)
C                                       Init values for loop.
      IDEPTH(1) = IROUND (BLC(3))
      IDEPTH(2) = IROUND (BLC(4))
      IDEPTH(3) = IROUND (BLC(5))
      IDEPTH(4) = IROUND (BLC(6))
      IDEPTH(5) = IROUND (BLC(7))
C                                       Loop for all possible planes.
      DO 30 ISEQ = SEQ1,SEQ2
         IF (ISEQ.NE.SEQ1) THEN
            MTYPE = 'MA'
            IVOL = DSKIN
            CALL MAPOPN ('READ', IVOL, NAMIN, CLSIN, ISEQ, MTYPE, IUSER,
     *         LUN, IND, ICNO, CATOLD, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE', ISEQ
               IF (IRET.NE.3) GO TO 990
               WRITE (MSGTXT,1010) ISEQ
               CALL MSGWRT (7)
               IRET = 0
               GO TO 30
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = IVOL
            FCNO(NCFILE) = ICNO
            FRW(NCFILE) = 0
            END IF
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('READ', LUN, IND, CATOLD(KINAX), CATOLD(KINAX+1),
     *      IWIN, RBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O FOR INPUT FILE', ISEQ
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       read input plane
         IS = ISEQ - SEQ1 + 1
         DO 20 IY = 1,NY
            CALL MDISK ('READ', LUN, IND, RBUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE', ISEQ
               GO TO 990
               END IF
            CALL RCOPY (NX, RBUFF(IBIND), IMAGE(1,IY,IS))
 20         CONTINUE
C                                       close inout file
         CALL MAPCLS ('READ', IVOL, ICNO, LUN, IND, CATOLD, .FALSE.,
     *      SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE INPUT IMAGE', ISEQ
            GO TO 990
            END IF
         NCFILE = NCFILE - 1
 30      CONTINUE
C                                       do medians
      DO 100 IY = 1,NY
         DO 90 IX = 1,NX
            I = 0
            DO 80 IZ = 1,NZ
               IF (IMAGE(IX,IY,IZ).NE.FBLANK) THEN
                  I = I + 1
                  RBUFF(I) = IMAGE(IX,IY,IZ)
                  END IF
 80            CONTINUE
            IF (I.LE.0) THEN
               IMAGE(IX,IY,1) = FBLANK
            ELSE
               IMAGE(IX,IY,1) = MEDIAN (I, RBUFF)
               END IF
 90         CONTINUE
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKN ERROR:',I5,' ON ',A,I6)
 1010 FORMAT ('STACKN INSEQ=',I6,' NOT FOUND')
      END
      SUBROUTINE STACKM (NX, NY, NZ, IMAGE, IRET)
C-----------------------------------------------------------------------
C   Stacks over BLC(3) to TRC(3) to find median
C   Inputs:
C      NX       I      Number X pixels in usable image plane
C      NY       I      Number Y pixels in usable image plane
C      NZ       I      Number of planes to be read and used
C   Outputs:
C      IMAGE    R(*)   Work memory for input images: first plane is
C                      output median
C      IRET     I      > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,NY,NZ)
C
      INCLUDE 'STACK.INC'
C
      INTEGER   IBLKOF, IDEPTH(5), I, IBIND, NBY, NPTS, MSGSAV, IROUND,
     *   IUSER, IX, IY, IZ, IP, IP1, IP2, IPL
      REAL      MEDIAN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      NBY = 2 * MABFSS
      IUSER = NLUSER
C                                       Init values for loop.
      NPTS = NX * NY * NZ
      CALL RFILL (NPTS, FBLANK, IMAGE)
      IP1 = IROUND (BLC(3))
      IP2 = IROUND (TRC(3))
      IDEPTH(1) = IROUND (BLC(3))
      IDEPTH(2) = IROUND (BLC(4))
      IDEPTH(3) = IROUND (BLC(5))
      IDEPTH(4) = IROUND (BLC(6))
      IDEPTH(5) = IROUND (BLC(7))
C                                       Loop for all possible planes.
      DO 30 IP = IP1,IP2
         IPL = IP - IP1 +1
         IDEPTH(1) = IP
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
C
         CALL MINIT ('READ', LUN, IND, CATOLD(KINAX), CATOLD(KINAX+1),
     *      IWIN, RBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT I/O FOR INPUT PLANE', IP
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       read input plane
         DO 20 IY = 1,NY
            CALL MDISK ('READ', LUN, IND, RBUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT PLANE', IP
               GO TO 990
               END IF
            CALL RCOPY (NX, RBUFF(IBIND), IMAGE(1,IY,IPL))
 20         CONTINUE
 30      CONTINUE
C                                       close inout file
      CALL MAPCLS ('READ', IVOL, ICNO, LUN, IND, CATOLD, .FALSE.,
     *   SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE INPUT IMAGE'
         GO TO 990
         END IF
      NCFILE = NCFILE - 1
C                                       do medians
      DO 100 IY = 1,NY
         DO 90 IX = 1,NX
            I = 0
            DO 80 IZ = 1,NZ
               IF (IMAGE(IX,IY,IZ).NE.FBLANK) THEN
                  I = I + 1
                  RBUFF(I) = IMAGE(IX,IY,IZ)
                  END IF
 80            CONTINUE
            IF (I.LE.0) THEN
               IMAGE(IX,IY,1) = FBLANK
            ELSE
               IMAGE(IX,IY,1) = MEDIAN (I, RBUFF)
               END IF
 90         CONTINUE
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKM ERROR:',I5,' ON ',A,I6)
      END
      SUBROUTINE STACKH (NX, NY, NZ, IMAGE, WEIGHT, IRET)
C-----------------------------------------------------------------------
C   normalizes image and outputs it, copies tables, writes history
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NZ, IRET
      REAL      IMAGE(NX,*), WEIGHT(NX,*)
C
      INCLUDE 'STACK.INC'
C
      INTEGER   IH1LUN, IH2LUN, NFILES, IERR, LUNTMP, IX, IY, IBIND,
     *   JBUFSZ, IBLC(7), ITRC(7), IWTS(32768), I, I1, I2
      CHARACTER HILINE*72, PHNAME*48
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (WTS, IWTS)
      DATA IH1LUN, IH2LUN /27, 28/
      DATA NFILES /2/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       normalize the image
      IF (NZ.LE.1) THEN
         DO 20 IY = 1,NY
            DO 10 IX = 1,NX
               IF (WEIGHT(IX,IY).LE.0.0) THEN
                  WASBLK = .TRUE.
                  IMAGE(IX,IY) = FBLANK
               ELSE
                  IMAGE(IX,IY) = IMAGE(IX,IY) / WEIGHT(IX,IY)
                  RMIN = MIN (RMIN, IMAGE(IX,IY))
                  RMAX = MAX (RMAX, IMAGE(IX,IY))
                  END IF
 10            CONTINUE
 20         CONTINUE
      ELSE
         DO 40 IY = 1,NY
            DO 30 IX = 1,NX
               IF (IMAGE(IX,IY).EQ.FBLANK) THEN
                  WASBLK = .TRUE.
               ELSE
                  RMIN = MIN (RMIN, IMAGE(IX,IY))
                  RMAX = MAX (RMAX, IMAGE(IX,IY))
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       check and update header
      IRET = 10
      IF (RMAX.EQ.RMIN) THEN
         WRITE (MSGTXT,1020) RMAX
         GO TO 990
      ELSE IF (RMAX.LT.RMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         GO TO 990
         END IF
      CATR(KRDMN) = RMIN
      CATR(KRDMX) = RMAX
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C                                       open the IO
      JBUFSZ = 2 * MABFSS
      CALL ZPHFIL ('MA', OVOL, OCNO, 1, PHNAME, IRET)
      LUN = LUNTMP (0)
      CALL ZOPEN (LUN, IND, OVOL, PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT IMAGE FILE'
         GO TO 990
         END IF
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = NX
      IWIN(4) = NY
      CALL MINIT ('WRIT', LUN, IND, NX, NY, IWIN, RBUFF, JBUFSZ, 1,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO OUTPUT IMAGE'
         GO TO 990
         END IF
      DO 50 IY = 1,NY
         CALL MDISK ('WRIT', LUN, IND, RBUFF, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE ROW OF OUTPUT IMAGE'
            GO TO 990
            END IF
         CALL RCOPY (NX, IMAGE(1,IY), RBUFF(IBIND))
 50      CONTINUE
      CALL MDISK ('FINI', LUN, IND, RBUFF, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE LAST ROW OF OUTPUT IMAGE'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, IRET)
      FRW(1) = 1
C                                       Initialize HITAB
      CALL HIINIT (NFILES)
C                                       Create and copy history file.
      CALL HISCOP (IH1LUN, IH2LUN, IVOL0, OVOL, ICNO0, OCNO, CATBLK,
     *   SCRTCH, RBUFF, IERR)
      IF (IERR.GT.3) GO TO 300
      IF (IERR.EQ.3) GO TO 290
C                                       Add STACK history.
      CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQ1, IVOL0, IH2LUN, RBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 290
      CALL HENCOO (TSKNAM, NAMOUT, CLSOUT, SEQO, OVOL, IH2LUN, RBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 290
      DO 200 I = 1,7
         IBLC(I) = BLC(I) + 0.1
         ITRC(I) = TRC(I) + 0.1
 200     CONTINUE
      WRITE (HILINE,1211) IBLC
      CALL HIADD (IH2LUN, HILINE, RBUFF, IERR)
      IF (IERR.NE.0) GO TO 290
      WRITE (HILINE,1212) ITRC
      CALL HIADD (IH2LUN, HILINE, RBUFF, IERR)
      IF (IERR.NE.0) GO TO 290
      IF (OPCODE.EQ.'CUBE') THEN
         WRITE (HILINE,1213) OPCODE
         IX = ITRC(3) - IBLC(3) + 1
      ELSE
         WRITE (HILINE,1214) SEQ2
         IX = SEQ2 - SEQ1 + 1
         END IF
      CALL HIADD (IH2LUN, HILINE, RBUFF, IERR)
      IF (IERR.NE.0) GO TO 290
C                                       normalize weights
      IF (NZ.LE.1) THEN
         RMAX = 0.0
         DO 210 I = 1,IX
            RMAX = MAX (RMAX, WTS(I))
 210        CONTINUE
         RMAX = 9999.0 / RMAX
         DO 220 I = 1,IX
            IWTS(I) = WTS(I) * RMAX + 0.5
 220        CONTINUE
         I2 = 0
 230     I1 = I2 + 1
         I2 = I1 + 8
         I2 = MIN (I2, IX)
         IF (I2.GE.I1) THEN
            WRITE (HILINE,1230) I1, I2, (IWTS(I), I = I1,I2)
            CALL HIADD (IH2LUN, HILINE, RBUFF, IERR)
            IF (IERR.NE.0) GO TO 290
            GO TO 230
            END IF
         END IF
C
 290  CALL HICLOS (IH2LUN, .TRUE., RBUFF, IERR)
C                                       Copy no files
 300  IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STACKH ERROR',I5,' AT ',A)
 1020 FORMAT ('STACKH IMAGE CONSTANT AT',1PE12.4)
 1211 FORMAT ('STACK BLC    =',2(I5,','),4(I5,','),I5)
 1212 FORMAT ('STACK TRC    =',2(I5,','),4(I5,','),I5)
 1213 FORMAT ('STACK OPCODE = ''',A,'''')
 1214 FORMAT ('STACK IN2SEQ =',I6)
 1230 FORMAT ('STACK WTS(',I5,',',I5,') =',9I5)
      END
