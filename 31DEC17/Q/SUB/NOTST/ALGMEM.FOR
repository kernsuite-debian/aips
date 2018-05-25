      SUBROUTINE ALGMEM (DISKI, CNOSCI, DISKO, CNOSCO, CATR, JBUFSZ,
     *   MODSTA, MODEND, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Interpolates model visibility from a grid and subtracts from uv data.
C# UV Modeling AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2008-2009, 2012, 2014-2016, 2018
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
C   ALGMEM interpolates model visibility data from a grid and subtracts
C   it from the observed visibilities.  UV data may be in any sort order
C   Assumes CC grids already in AP memory and described by DMOD.INC.
C   Inputs:
C      SCRGRD   I       /CFILES/ file number for grid file.
C      SCRWRK   I       /CFILES/ file number for work file
C      DISKI    I       Input file disk number for catalogd files,
C                       .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I       Input file catalog slot number or /CFILES/
C                       scratch file number.
C      DISKO    I       Output file disk number for catalogd files,
C                       .LE. 0 => /CFILES/ scratch file.
C      CNOSCO   I       Output file catalog slot number or /CFILES/
C                       scratch file number.
C      CATR     R(256)  UV data catalog header record.
C      LOGRID   I       AP location of CC grid (row -5)
C      JBUFSZ   I       Size in bytes of buffers. Dimension of
C                       BUFF1,2,3  must be at least 4096 words.
C      MODSTA   I       First model to use
C      MODEND   I       Last model to use
C   Inputs: from commons
C      LREC     I       length in words of vis record.
C      NVIS     I       number of vis records
C      FREQG    D(*)    Frequencies of IF and channels in same order
C                       as occurs in the data.
C   Output:
C      BUFF1    R(*)    Working buffer
C      BUFF2    R(*)    Working buffer
C      BUFF3    R(*)    Working buffer
C      IRET     I       Return code, 0 => ok, otherwise not.
C   The AP grid file must contain the gridded UV model for this pass.
C   The UV points are read in and the model is subtracted from them.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, JBUFSZ, MODSTA, MODEND,
     *   IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
C                                       MINT   = Interpol. support size.
      INTEGER   MINT
      PARAMETER (MINT=11)
      INTEGER   VISRED, NIOUT, IDISK, ICNOSC, INDS(3), NX, NY, ILENBU,
     *   IU, U, IVIS, I, NIO, LENBU, M, MO2, CX, VIS, UV, LLREC,
     *   MM, CNJPTR, WORK, INDEX, LROW, FLAG, NS, APSIZE, MAXREC, CNT,
     *   IFACT, ITEMP1, ITEMP2, INPTR, OUTPTR, JNPTR, KNPTR, LNPTR,
     *   NPOINT, JNCS, J, K, EXCESS, JNCF, LRPARM, ROWSIZ, IDATA, MAXU,
     *   EROW, ERRCNT, IXTEMP(3), MSGSAV, HIC, LOC, UVWP, IFIELD, IM,
     *   LOGRID, CHANEL, NCHAN, SOFF, VISC, UVSAVE, UCOUNT, UTARG, II,
     *   JJ, KK
      LOGICAL   DOROT, DOFLAG, ONEFRQ
      REAL      FACT2(4), CATR(256), ZSCLV, ZSCLW, ZSCLU, SCALU, SCALV,
     *   FRSTU, UUMAX, VVMAX, UIN, VIN, UUU, RMAT(3,3), DMAT(3,3),
     *   PMAT(3,3)
      DOUBLE PRECISION GFACT, XRA, XDEC, FFRAC
      CHARACTER CSTOK*1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMOD.INC'
      REAL      DDX(3,MAXMOD), UMAT(3,3,MAXMOD), XTEMP(7,MAXMOD)
      DOUBLE PRECISION MINFRQ(MAXMOD), MAXFRQ(MAXMOD)
      SAVE ERRCNT
      DATA ERRCNT /0/
C-----------------------------------------------------------------------
      IRET = 0
      DOFLAG = ERRCNT.EQ.0
      ONEFRQ = .TRUE.
      DO 10 IM = MODSTA,MODEND
         IF (MODCHN(IM).NE.MODCHN(MODSTA)) ONEFRQ = .FALSE.
 10      CONTINUE
      IFIELD = MODFLD(MODSTA)
C                                       Rotate?
      DOROT = (ABS (SSROT).GT.1.0E-10) .OR. (ABS (CCROT-1.0).GT.1.0E-4)
      APSIZE = PSAPNW * 1024
      IFACT = 2
C                                       Get un-compressed UV increments
      IF (DATDIV) CALL PRMUPD
      IF (COMPDT) THEN
         ITEMP1 = LREC
         ITEMP2 = NRPARM
         LREC = SCLREC
         NRPARM = SCRPRM
         END IF
C                                       Correlator scaling table.
      MSGSAV = MSGSUP
      MSGSUP = 31900
      CALL GETCTL (CATR, FACT2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      CSTOK = MSGTXT(1:1)
      NS = NSTOK
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF,
     *   LRPARM, LLREC)
C                                       Set I/O visibility count
      ILENBU = ((JBUFSZ - 2 * NBPS) / 2) / LLREC - 2
      ILENBU = MAX (ILENBU, 1)
      LENBU  = ILENBU
      IDISK = DISKI
      ICNOSC = CNOSCI
C                                       conv parameters
      M = MINT
      MO2 = M / 2
C                                       AP pointers
      CX = MODGRD(MODEND+1)
      CNJPTR = CX + 200 * (M+1) + 1
      EXCESS = APSIZE - CNJPTR
      MAXREC = EXCESS / (LLREC+8) - 5
      MM = M
      FLAG = 1
      IF ((KSTOK.EQ.3) .AND. (ICOR0.LE.0)) FLAG = -1
C                                       model only
      IF (FACGRD(2).EQ.0.0) THEN
         UUU = 0.0
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
         FACT2(3) = FACT2(3) * FACGRD(1)
         FACT2(4) = FACT2(4) * FACGRD(1)
C                                       Scale FACT2 by -FACGRD
C                                       data - model
      ELSE
         UUU = 1.0
         FACT2(1) = -FACT2(1) * FACGRD(1)
         FACT2(2) = -FACT2(2) * FACGRD(1)
         FACT2(3) = -FACT2(3) * FACGRD(1)
         FACT2(4) = -FACT2(4) * FACGRD(1)
         END IF
C                                       If fft scale by 0.5
      IF (DOFFT) THEN
         FACT2(1) = FACT2(1) * 0.5
         FACT2(2) = FACT2(2) * 0.5
         FACT2(3) = FACT2(3) * 0.5
         FACT2(4) = FACT2(4) * 0.5
         END IF
C                                       Prepare for bs loop
      CALL RFILL (9, 0.0, RMAT)
      RMAT(1,1) = CCROT
      RMAT(1,2) = SSROT
      RMAT(2,1) = -SSROT
      RMAT(2,2) = CCROT
      RMAT(3,3) = 1
      UUMAX = 1.E20
      VVMAX = 1.E20
      GFACT = 0.0
      MAXU = 1
      DO 50 IM = MODSTA,MODEND
         IFIELD = MODFLD(IM)
         DDX(1,IM) = DXCG(IFIELD)
         DDX(2,IM) = DYCG(IFIELD)
         DDX(3,IM) = DZCG(IFIELD)
         CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD),
     *      MAPROT, XRA, XDEC)
         IF (DO3DIM) THEN
            CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT,
     *         UMAT(1,1,IM), PMAT)
            CALL RFILL (9, 0.0, DMAT)
            DO 30 I = 1,3
               DO 25 J = 1,3
                  DO 20 K = 1,3
                     DMAT(I,J) = DMAT(I,J) + PMAT(I,K) * RMAT(K,J)
 20                  CONTINUE
 25               CONTINUE
 30            CONTINUE
            CALL PRJMUL (2, DDX(1,IM), DMAT, DDX(1,IM))
         ELSE
            CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT,
     *         UMAT(1,1,IM), PMAT)
            END IF
C                                       Find Max Frequency
         CHANEL = MODCHN(IM)
         NCHAN = MODNCH(IM)
         MAXFRQ(IM) = FREQG(CHANEL)
         MINFRQ(IM) = FREQG(CHANEL)
         DO 35 I = 2,NCHAN
            IF (FREQG(CHANEL+I-1).GT.0.0D0) THEN
               MAXFRQ(IM) = MAX (MAXFRQ(IM), FREQG(CHANEL+I-1))
               MINFRQ(IM) = MIN (MINFRQ(IM), FREQG(CHANEL+I-1))
               END IF
            IF (IM.EQ.MODSTA) THEN
               FFRAC = 0.0D0
               IF (FREQG(CHANEL+I-2).GT.0.0D0) FFRAC =
     *            FREQG(CHANEL+I-1) / FREQG(CHANEL+I-2) - 1.0D0
               BUFF2(I-1) = FFRAC
               END IF
 35         CONTINUE
         LOC = CHANEL
         HIC = LOC + NCHAN - 1
         WRITE (MSGTXT,1030) IFIELD, CSTOK, LOC, HIC
         CALL MSGWRT (2)
C                                       area for negative U
         LOGRID = MODGRD(IM)
         NX = ABS (FLDSZ(1,IFIELD) * OSFX) + 0.1
         NY = ABS (FLDSZ(2,IFIELD) * OSFY) + 0.1
         ROWSIZ = IFACT * NY
         DO 40 IU = 1,MO2
C                                       Convert to decreasing u
            U = MO2 - IU + 1
C                                       U=0 at lrow/2
C                                       U<0 rows from complex cong u>0
C                                       Index points to end of u row
            INDEX = LOGRID + ((U+MO2+1)*ROWSIZ) - IFACT
C                                       Erow points to start of -u row
            EROW  = LOGRID + ((MO2-U)*ROWSIZ)
            CALL QCVCON (INDEX, -IFACT, EROW+IFACT, IFACT, NY-1)
C                                       Index points to start same u row
            INDEX = INDEX - ROWSIZ + IFACT
C                                       Conjugate a single pixel
            CALL QCVCON (INDEX, IFACT, EROW, IFACT, 1)
 40         CONTINUE
C                                       field scaling parms
         ZSCLU = SCLUG(IFIELD) * OSFX
         ZSCLV = SCLVG(IFIELD) * OSFY
         ZSCLW = SCLWG(IFIELD)
C                                       Limit u to max that will fit
         MAXU   = MAX (NX/2, MAXU)
         VISRED = 0
C                                       Get frequency scaling factors.
         FFRAC = (MINFRQ(IM) / FREQ) - 1.0D0
         GFACT = MAX (GFACT, 1.0D0 / ((1.0D0 + FFRAC) * ZSCLU))
         FFRAC = (MAXFRQ(IM) / FREQ) - 1.0D0
         SCALU = FFRAC * ZSCLU + ZSCLU
         SCALV = FFRAC * ZSCLV + ZSCLV
C                                       Set limits on u,v - don't get
C                                       Within mint/2+1 of edge.
         UUMAX = MIN (UUMAX, ABS (((NX/2) - (MINT/2+1)) / SCALU))
         VVMAX = MIN (VVMAX, ABS (((NY/2) - (MINT/2+1)) / SCALV))
C                                       Setup ap griding constants
         FFRAC = (FREQG(CHANEL) / FREQ) - 1.0D0
         XTEMP(1,IM) = (ZSCLU + FFRAC * ZSCLU)
         XTEMP(2,IM) = (ZSCLV + FFRAC * ZSCLV)
         XTEMP(3,IM) = DDX(1,IM) / ZSCLU
         XTEMP(4,IM) = DDX(2,IM) / ZSCLV
         XTEMP(5,IM) = DDX(3,IM) + DDX(3,IM)*FFRAC
         XTEMP(6,IM) = 0.0
         XTEMP(7,IM) = 0.0
 50      CONTINUE
C                                       Number of rows in a pass
      VISRED = 0
C                                       Correlator scaling table.
      CALL QPUT (FACT2, 16, NS, 2)
      CALL QPUT (1.0, 15, 1, 2)
C                                       Frequency scaling table.
      NCHAN = MODNCH(MODSTA)
      BUFF2(NCHAN) = 0.0
      CALL QPUT (BUFF2, 20, NCHAN, 2)
C                                       Set interpolation tables
      CALL INTPFN (CX, M, BUFF3, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       More ap pointers.
      UVSAVE = CNJPTR + MAXREC
      WORK = UVSAVE + 3*MAXREC
      UV = WORK + 4 + (MAXREC * 4)
      UVWP = UV + ILOCU
C                                       Correction for stokes.
      VIS = UV + LRPARM
      SOFF = (VOFF / INCS) * JNCS
C                                       Set location of data inp ap
      IDATA = UV
      OUTPTR = 1
      INPTR  = 1
      ILENBU = MIN (LENBU, MAXREC)
C                                       Set output file name.
      LUNS(2) = 17
      LUNS(3) = 18
      CALL UVPREP ('WRIT', DISKO, CNOSCO, LUNS(3), INDS(3), NVIS, LREC,
     *   ILENBU, JBUFSZ, BUFF3, NIOUT, OUTPTR, FRSTU, IRET)
      IF (IRET.NE.0) GO TO 999
      NIOUT = ILENBU
      ILENBU = MIN (LENBU, MAXREC)
C                                       Prepare to read through data
      CALL UVPREP ('REED', IDISK, ICNOSC, LUNS(2), INDS(2), NVIS, LREC,
     *   ILENBU, JBUFSZ, BUFF1, NIO, INPTR, FRSTU, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reset input to output file.
      IDISK = DISKO
      ICNOSC = CNOSCO
C                                       Count points this pass
      NPOINT = 0
      UCOUNT = NIO
      UTARG = 1
C                                       Loop forever, read all uvdata
 100  CONTINUE
         IF (UCOUNT.GT.UTARG) THEN
            WRITE (MSGTXT,1100) UTARG
            CALL MSGWRT (2)
            UTARG = UTARG + 200000
            END IF
C                                       Start read from beginning of uvs
         JNPTR = INPTR
C                                       Put uvdata back at beginning
         KNPTR = INPTR
C                                       Use start of output buffer
         LNPTR = OUTPTR
C                                       Count points this buffer
         CNT = 0
C                                       Do all points in buffer
         DO 200 IVIS = 1,NIO
C                                       Input u, v values
            UIN = ABS (BUFF1(JNPTR+ILOCU))
            VIN = ABS (BUFF1(JNPTR+ILOCV))
C                                       uv outside range.
            IF ((UIN.GT.UUMAX) .OR. (VIN.GT.VVMAX)) THEN
               CALL UVLIMT (0.0, 1.0E20, UUMAX, VVMAX, IVIS, UIN, VIN,
     *            ERRCNT)
C                                       Reset u to within range
               UIN = MAXU * GFACT
               UIN = MIN (UUMAX, UIN) * 0.99
C                                       Put location back in buffer
               BUFF1(JNPTR+ILOCU) = UIN
               BUFF1(JNPTR+ILOCV) = 0.0
C                                       Zero data
               CALL RFILL (LREC-LRPARM, 0.0, BUFF1(JNPTR+LRPARM))
C                                       Zero data save weights
            ELSE IF (UUU.EQ.0.0) THEN
               KK = JNPTR + LRPARM - 1
               JJ = LREC - LRPARM
               DO 120 II = 1,JJ,3
                  BUFF1(KK+II) = 0.0
                  BUFF1(KK+II+1) = 0.0
 120              CONTINUE
C              CALL RFILL (LREC-LRPARM, 0.0, BUFF1(JNPTR+LRPARM))
               END IF
C                                       Keep count of number to process
            CNT = CNT + 1
C                                       Set new input and output pos
            JNPTR = JNPTR + LREC
            LNPTR = LNPTR + LREC
C                                       End for all vis recs read
 200        CONTINUE
C                                       Keep track of total
         VISRED = VISRED + CNT
C                                       Count points in range each pass
         NPOINT = NPOINT + CNT
C                                       If points were found
         IF (CNT.GT.0) THEN
C                                       Wait for ap, then load into ap
            CALL QWR
C                                       Uncompress Vis and put in AP
            CALL BUFPUT (CNT, NRPARM, LREC, LLREC, IDATA, BUFF1(INPTR),
     *         BUFF2)
C                                       Loop over grids
            DO 300 IM = MODSTA,MODEND
               IFIELD = MODFLD(IM)
               LOGRID = MODGRD(IM)
               NCHAN = MODNCH(IM)
               CHANEL = MODCHN(IM)
C                                       freq corr
               IF ((.NOT.ONEFRQ) .AND. ((IM.EQ.MODSTA) .OR.
     *            (MODCHN(IM).NE.MODCHN(IM-1)))) THEN
                  DO 210 I = 2,NCHAN
                     FFRAC = 0.0D0
                     IF (FREQG(CHANEL+I-2).GT.0.0D0) FFRAC =
     *                  FREQG(CHANEL+I-1) / FREQG(CHANEL+I-2) - 1.0D0
                     BUFF2(I-1) = FFRAC
 210                 CONTINUE
                  BUFF2(NCHAN) = 0.0
                  CALL QPUT (BUFF2, 20, NCHAN, 2)
                  END IF
C                                       Low row to ap
               IXTEMP(1) = 0
               IXTEMP(2) = LOGRID
               IXTEMP(3) = CX
               CALL QPUT (IXTEMP, 7, 3, 1)
               CALL QPUT (XTEMP(1,IM), 0, 7, 2)
               CALL QWAIT
               NY = ABS (FLDSZ(2,IFIELD) * OSFY) + 0.1
               LROW = NY
C                                       rotate u,v,w
               IF ((IM.EQ.MODSTA) .OR. (IFIELD.NE.MODFLD(IM-1))) THEN
                  CALL QVMOV (UVWP, LLREC, UVSAVE, 3, CNT)
                  CALL QVMOV (UVWP+1, LLREC, UVSAVE+1, 3, CNT)
                  CALL QVMOV (UVWP+2, LLREC, UVSAVE+2, 3, CNT)
                  CALL QPRJM (1, UVWP, UMAT(1,1,IM), UVWP, CNT,
     *               LLREC, LLREC)
                  CALL QUCONJ (1, UV, VIS, CNJPTR, CNT, LLREC)
                  END IF
C                                       interpolate model, correct data
               VISC = VIS + SOFF + (CHANEL - 1) * JNCF
               CALL QUVIN (UV, VISC, WORK, LLREC, MM, LROW, NS, JNCS,
     *            NCHAN, JNCF, CNT, FLAG)
C                                       un-rotate u,v,w
               IF ((IM.EQ.MODEND) .OR. (IFIELD.NE.MODFLD(IM+1))) THEN
                  CALL QUCONJ (2, UV, VIS, CNJPTR, CNT, LLREC)
                  CALL QVMOV (UVSAVE, 3, UVWP, LLREC, CNT)
                  CALL QVMOV (UVSAVE+1, 3, UVWP+1, LLREC, CNT)
                  CALL QVMOV (UVSAVE+2, 3, UVWP+2, LLREC, CNT)
                  END IF
               CALL QWR
 300           CONTINUE
C                                       Read out interpolated data
C                                       If all data in ap
C                                       Get UVs from AP (maybe pack UV)
            CALL BUFGET (CNT, NRPARM, LREC, LLREC, IDATA, BUFF3(OUTPTR),
     *         BUFF2)
C                                       End if points this pass
            END IF
C                                       Write all nio points read
         NIOUT = NIO
C                                       Write data to disk
         CALL UVDISK ('WRIT', LUNS(3), INDS(3), BUFF3, NIOUT, OUTPTR,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET
            GO TO 995
            END IF
C                                       Read a new uvdata buffer
         CALL UVDISK ('READ', LUNS(2), INDS(2), BUFF1, NIO, INPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1300) IRET
            GO TO 995
            END IF
         UCOUNT = UCOUNT + NIO
C                                       If more data loop
         IF (NIO.GT.0) GO TO 100
C                                       Jump here on end uv data
C                                       Write Diag messages ONLY once
C                                       Print only 10 Messages
      WRITE (MSGTXT,1104,ERR=680) MAXU, NPOINT
 680  CALL MSGWRT (2)
C                                       Finish write.
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNS(3), INDS(3), BUFF3, NIOUT, OUTPTR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1200) IRET, I
         GO TO 995
         END IF
C                                       Close files
      CALL ZCLOSE (LUNS(2), INDS(2), IRET)
      CALL ZCLOSE (LUNS(3), INDS(3), IRET)
C                                       Check that written all data read
      IF (NVIS.NE.VISRED) THEN
         WRITE (MSGTXT,1600) NVIS, VISRED
         CALL MSGWRT (8)
         END IF
      IF ((DOFLAG) .AND. (ERRCNT.GT.0)) THEN
         MSGTXT = '**************************************************'
     *     // '*****'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1601) ERRCNT
         CALL MSGWRT (8)
         MSGTXT = '**************************************************'
     *     // '*****'
         CALL MSGWRT (8)
         END IF
C                                       Give up AP.
      CALL QRLSE
      IF (COMPDT) THEN
         LREC = ITEMP1
         NRPARM = ITEMP2
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ALGMEM field',I5,1X,A1,'pol gridded model subtraction',
     *   ' chns',I5,'-',I5)
 1100 FORMAT ('ALGMEM: at visibility number',I12)
 1104 FORMAT ('ALGMEM:',I6,' - 0 cells, with',I12,' Pts')
 1200 FORMAT ('ALGMEM: WRITE ERROR IN VISIBILITY FILE, IER=',I6)
 1300 FORMAT ('ALGMEM: READ  ERROR IN VISIBILITY FILE, IER=',I6)
 1600 FORMAT ('ALGMEM: WARNING! Misplaced Data, NVIS = ',I8,
     *   ' WRITTEN =',I8)
 1601 FORMAT ('ALGMEM:',I10,' POINTS FLAGGED FOR BEING OFF THE GRID')
      END
