      SUBROUTINE GRDSUB (MODEL, DOSUM, SCRGRD, SCRWRK, DISKI, CNOSCI,
     *   DISKO, CNOSCO, IFIELD, CHANEL, NCHAN, CATR, JBUFSZ, BUFF1,
     *   BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Subtracts transform of CLEAN components from uv data.
C# AP-util Map UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2003, 2006, 2008-2010, 2013
C;  Copyright (C) 2015-2016
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
C   GRDSUB subtracts CLEAN components from ungridded visibility data.
C   All un subtracted data processed in one call. Can subtract either
C   up to 16 sets of CC files or use a gridded uv file provided.
C   /UVHDR/ common is presumed to describe the input file.
C   Inputs:
C      MODEL       I     1 CC, 2 image, 3 sub-image
C      DOSUM       L     If true sum the flux in each field
C      SCRGRD      I     /CFILES/ file number for grid file.
C      SCRWRK      I     /CFILES/ file number for work file
C      DISKI       I     Input file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI      I     Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO       I     Output file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCO      I     Output file catalog slot number or /CFILES/
C                        scratch file number.
C      IFIELD      I     Field to do (o -> all)
C      CHANEL      I     First channel to subtract in uv data.
C      NCHAN       I     Number of channels to subtract.
C      CATR(256)   R     uv data catalog header record.
C      JBUFSZ      I     Size of the buffers in bytes. The dimension
C                        of the buffers must be at least 4096.
C      BUFF1,2,3(*)R     Work buffers.
C   Inputs from COMMON /MAPDES/:
C      MFIELD      I     Number of fields
C      NSUBG(*)    I     Number of components already sub.
C      NCLNG(*)    I     Number of components per field
C      CCDISK(*)   I     Disk numbers for CC files
C      CCCNO(*)    I     Catalog slot numbers for CC files.
C      CCVER(*)    I     CC file version number for each field.
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop reading comps < LIMFLX in abs value
C      NGRDAT      L     If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C      FACGRD      R(2)  Value to multiply clean component fluxes
C                        by before subtraction (i.e. negative for sum).
C                        FACGRD(2) is for data and 0 or 1 only values
C                        used.  Model added not subtracted when data are
C                        ignored.
C      SCTYPE      C*2   Scratch file type to create. (eg. 'SC')
C      KSTOK       I     Stokes parameter of model. 1-4=>I,Q,U,V.
C      DOFFT       L     If FALSE need to grid clean components,
C                        if TRUE then the grid already exists in
C                        the /CFILES/ scratch file SCRGRD
C   Input from COMMON /UVHDR/:
C      LREC        I     Length of visibility record.
C      NVIS        I     Number of visibility records.
C      NRPARM      I     "Random" parameters before data, can be used
C                        to skip observed values when computing model.
C      INCS        I     Stokes increment: if 1, then compressed data
C   Output:
C      SCRGRD      I     /CFILES/ file number for grid file: set if
C                        needed and input was 0
C      SCRWRK      I     /CFILES/ file number for work file: set if
C                        needed and input was 0
C      IRET        I     Return code, 0 => ok, otherwise not.
C   Output in COMMON /MAPDES/
C      FLUXG(*)     R      Total flux from each field. (Jy)
C      TFLUXG       R      Total flux from all fields. (Jy)
C-----------------------------------------------------------------------
      LOGICAL   DOSUM
      INTEGER   MODEL, SCRGRD, SCRWRK, DISKI, CNOSCI, DISKO, CNOSCO,
     *   IFIELD, CHANEL, NCHAN, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*), CATR(256)
C
      INTEGER   LFIELD, DISKT, CNOT, KAP, LF1, LF2, NEED, LNEED, CNEED,
     *   UNEED, LLREC, LOGRID, NX, NY, NF, NC, NW, I, LLF2, NCH, LF, II,
     *   MODSTA, MODEND, PNEED, APSIZE
      LOGICAL   F, DOROT, OLD, WESET
      CHARACTER UMET*4
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMOD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      UMET = 'GRID'
      APSIZE = KAPWRD * 1024
C                                       Set initial input file.
      DISKT = DISKI
      CNOT = CNOSCI
      IF (DOSUM) TFLUXG = 0.0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      LLREC = LREC
      IF (INCS.EQ.1) LLREC = (LREC - NRPARM) * 3 + (NRPARM - 2)
C                                       think about models
      WESET = (MODMAX.LE.0) .AND. (.NOT.DOFFT)
      NF = 1
      NCH = NCHAN
      DO 10 I = 1,MODMAX
         NCH = MAX (NCH, MODNCH(I))
 10      CONTINUE
      LOGRID = 120 + NCH
C                                       Loop thru fields filling
C                                       uv grid.
      LLF2 = LF1 - 1
 100  LFIELD = LLF2 + 1
      IF (LFIELD.LE.LF2) THEN
C                                       single spectral channel if
C                                       GRDSUB is handling this
         IF (WESET) THEN
            CALL MODFIT (LFIELD, LF2, 1, NSUBG, NCLNG, LLREC, UMET,
     *         UMET, MODFLD, MODMAX, NC, NW)
            IF (MODMAX.GT.0) THEN
               DO 110 I = 1,MODMAX
                  NC = MODFLD(I)
                  MODCHN(I) = CHANEL
                  MODNCH(I) = NCHAN
                  MODCCV(I) = ABS(CCVER(NC))
                  MODCCB(I) = NSUBG(NC)
 110              CONTINUE
               NEED = NW
               LFIELD = MODFLD(1)
               LLF2 = MODFLD(MODMAX)
               END IF
C                                       outer world set
         ELSE IF (MODMAX.GT.0) THEN
            LFIELD = MODFLD(1)
            LLF2 = LF2
            END IF
         OLD = MODMAX.LE.0
         IF (OLD) LLF2 = LFIELD
         MODSTA = 1
         MODEND = MAX (1, MODMAX)
C                                       Make sure components to subtract
C                                       if subtracting components.
         IF ((DOFFT) .OR. (NCLNG(LFIELD).GE.NSUBG(LFIELD))) THEN
C                                       Get field info. if nec.
C                                       If NGRDAT read CLEAN catblk.
            IF (NGRDAT) THEN
               CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *            KLNBLK, 'REST', BUFF1, IRET)
               IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
                  WRITE (MSGTXT,1000) IRET
                  CALL MSGWRT(8)
                  GO TO 999
                  END IF
C                                       Must grid data
            ELSE IF (MODMAX.GT.0) THEN
               LF = 0
               DO 120 I = 1,MODMAX
                  NC = MODFLD(MODMAX+1-I)
                  IF (NC.NE.LF) THEN
                     CALL GRDAT (F, NC, CATR, BUFF1, IRET)
                     IF (IRET.NE.0) GO TO 999
                     LF = NC
                     END IF
 120              CONTINUE
            ELSE
               CALL GRDAT (F, LFIELD, CATR, BUFF1, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Init AP for gridding or FFT
 121        NX = ABS (FLDSZ(1,LFIELD) * OSFX)
            NY = ABS (FLDSZ(2,LFIELD) * OSFY)
            IF (DOFFT) THEN
               NEED = NX * NY
               LNEED = NEED
            ELSE IF ((WESET) .AND. (MODMAX.GT.0)) THEN
               LNEED = NEED
            ELSE IF (MODMAX.GT.0) THEN
               NEED = 200*12 + 256 * (LLREC + 8) + 120 + NCH
               MODEND = MODMAX
               DO 125 I = MODSTA,MODMAX
                  II = MODFLD(I)
                  NX = ABS (FLDSZ(1,II) * OSFX)
                  NY = ABS (FLDSZ(2,II) * OSFY)
                  PNEED = NEED
                  NEED = NEED + (NX + 24) * NY
C                                       overflow integer or ap size
                  IF ((NEED.LT.PNEED) .OR. (NEED.GT.APSIZE)) THEN
                     MODEND = I - 1
                     NEED = PNEED
                     GO TO 126
                     END IF
 125              CONTINUE
               LNEED = NEED
            ELSE
               NEED = (NX + 24) * NY + 120 + NCH
               CNEED = 4 * (2*NY+1) + 5*(NCLNG(LFIELD)-NSUBG(LFIELD)+1)
               UNEED = 200*12 + 256 * (LLREC + 8)
               NEED = NEED + MAX (CNEED, UNEED)
               LNEED = 104 + 5 * (NCLNG(LFIELD)-NSUBG(LFIELD)) + 6 * NY
               END IF
 126        LNEED = LNEED + 4 * NY
            NEED = NEED + 5 * NY
            LNEED = LNEED / 1024
            NEED = NEED / 1024 + 4
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LT.LNEED)) THEN
               MSGTXT = 'GRDSUB: FAILED TO GET THE NEEDED AP MEMORY'
               CALL MSGWRT (8)
               WRITE (MSGTXT,1125) NEED, MODMAX, PSAPNW
               CALL MSGWRT (8)
               IRET = 9
               GO TO 999
               END IF
            OLD = PSAPNW.LT.NEED
C                                       scratch disks
            IF (SCRGRD.GT.NSCR) SCRGRD = 0
            IF ((SCRGRD.GT.0) .AND. ((SCRVOL(SCRGRD).LE.0) .OR.
     *         (SCRCNO(SCRGRD).LE.0))) SCRGRD = 0
            IF (SCRWRK.GT.NSCR) SCRWRK = 0
            IF ((SCRWRK.GT.0) .AND. ((SCRVOL(SCRWRK).LE.0) .OR.
     *         (SCRCNO(SCRWRK).LE.0))) SCRWRK = 0
            IF (((DOFFT) .OR. (OLD)) .AND. ((SCRGRD.LE.0) .OR.
     *         (SCRWRK.LE.0))) THEN
               CALL GRDSET (MODEL, SCRGRD, SCRWRK, BUFF1, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       FFT image
            IF (DOFFT) THEN
               CALL FFTIM (MODEL, LFIELD, SCRGRD, SCRWRK, JBUFSZ, BUFF1,
     *            BUFF2, BUFF3, IRET)
               CALL QRLSE
C                                       Else, Grid components on model.
            ELSE IF (OLD) THEN
               CALL CCSGRD (LFIELD, DOSUM, SCRGRD, SCRWRK, JBUFSZ,
     *            BUFF1, BUFF2, BUFF3, IRET)
               CALL QRLSE
               IF (IRET.EQ.10) GO TO 500
C                                       gridded entirely in RAM
            ELSE
               IF ((MODSTA.GT.1) .OR. (MODEND.LT.MODMAX)) THEN
                  WRITE (MSGTXT,1126) MODSTA, MODEND
                  CALL MSGWRT (2)
                  END IF
               CALL CCSMEM (LFIELD, DOSUM, CHANEL, NCH, JBUFSZ,
     *            LOGRID, MODSTA, MODEND, BUFF1, BUFF2, BUFF3, IRET)
               IF (IRET.EQ.10) THEN
                  CALL QRLSE
                  GO TO 500
                  END IF
               END IF
            IF (IRET.NE.0) GO TO 999
C                                       Subtract model
C                                       If data is X* sorted and
C                                       data is NOT compressed
C                                       or image rotated
            DOROT = (ABS (SSROT).GT. 1.0E-6) .OR.
     *         (ABS (CCROT-1.0).GT.1.0E-4)
C                                       use general subtraction
            IF ((OLD) .OR. (DOFFT)) THEN
               CALL ALGSTB (LFIELD, SCRGRD, DISKT, CNOT, DISKO, CNOSCO,
     *            CHANEL, NCHAN, CATR, JBUFSZ, BUFF1, BUFF2, BUFF3,
     *            IRET)
C                                       in memory: multiple grids maybe
            ELSE
               CALL ALGMEM (DISKT, CNOT, DISKO, CNOSCO, CATR, JBUFSZ,
     *           MODSTA, MODEND, BUFF1, BUFF3, BUFF2, IRET)
               END IF
            IF (IRET.NE.0) GO TO 999
C                                       Output becomes input for
C                                       multiple fields.
            DISKT = DISKO
            CNOT = CNOSCO
            IF (MODEND.LT.MODMAX) THEN
               MODSTA = MODEND + 1
               GO TO 121
               END IF
            END IF
 500     GO TO 100
         END IF
      IF (WESET) MODMAX = 0
      IF (IRET.EQ.10) IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRDSUB: ERROR',I5,' READING CLEAN CATBLK')
 1125 FORMAT ('GRDSUB NEEDS',I9,' KWORDS FOR',I5,' MODELS, GOT',I9)
 1126 FORMAT ('GRDSUB processing models',I5,' through',I5)
      END
