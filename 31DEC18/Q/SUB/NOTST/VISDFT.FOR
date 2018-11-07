      SUBROUTINE VISDFT (OPCODE, CHANEL, NCHAN, DISKI, CNOSCI, DISKO,
     *   CNOSCO, IFIELD, DOSUM, DOMSG, CATR, JBUFSZ, BUFF1, BUFF2,
     *   IBUFF, IRET)
C-----------------------------------------------------------------------
C! Compute DFT of model and subtract/divide from/into uv data.
C# AP-appl UV Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2003, 2006-2009, 2011-2015, 2018
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
C   VISDFT subtracts/divides CLEAN components from/into ungridded
C   visibility data by a DFT model computation.  Only model components
C   of a single type are processed.  Point components will be taken as
C   Gaussians or Spheres as needed if some of the fields are extended
C   and some not.
C   All un subtracted data processed in one call.
C   Inputs:
C      OPCODE   C*4      Opcode 'SUB ', or 'DIV '
C      CHANEL   I        Frequency channel: used if MODMAX=0
C      NCHAN    I        Number of frequency channels.: ditto
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      IFIELD   I        Field to do (0 -> all): used if MODMAX=0
C      DOSUM    L        If true sum the flux in each field
C      DOMSG    L        If true give percent done messages.
C      CATR     R(256)   UV data catalog header record.
C      JBUFSZ   I        Size of BUFF1,2, IBUFF in AIPS bytes, each
C                        must be at least 4096 words.
C   Inputs: from commons
C      MODMAX   I        DMOD.INC - if set, this controls facets/chans
C      MFIELD   I        Number of fields
C      NCLNG    I(16)    Number of components per field. -
C                        changed if flux limit hit
C      NSUBG    I(16)    The next component to subtract.
C      CCDISK   I(16)    Disk numbers of the clean images.
C      CCCNO    I(16)    Catalog slot numbers of clean images.
C      CCVER    I(*)     CC file version number for each field.
C      NGRDAT   L        If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C      LREC     I        Length in words of vis record.
C      NVIS     I        Number of vis. records
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop if abs(flux) < LIMFLX
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C      PARMOD   R(6)     Model parameters for non point models; used
C                        only if DOPTMOD is true.
C                        1=> model type, 0=point, 1=gaussian, 3=sphere
C                        Gaussian: (2)=major axis(asec), (3)=minor axis
C                                  (4)=PA (degrees)
C                        Sphere: (2)=radius (asec).
C      KSTOK    I        (DGDS.INC) If a point model is specified a
C                        value of 2 indicates a Q pol model, 3 U and
C                        4 V pol.AC
C   In/out:
C      CNOSCO   I        IN: output file catalog slot number or /CFILES/
C                        scratch file number. Will create a scratch file
C                        if CNOSCO and DISKO .le. 0.
C                        Out: file /CFILES/ number if created.
C   Output:
C      BUFF1    R(*)     I/O buffers.
C      BUFF2    R(*)     I/O buffers.
C      IBUFF    I(*)     I/O Buffer.
C      IRET     I        Return code, 0 => ok, otherwise not.
C                            8 => model types not compatible/illegal
C                            9 => Buffers too small to load AP.
C                           10 => Too many components for division.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   CHANEL, NCHAN, DISKI, CNOSCI, DISKO, CNOSCO, IFIELD,
     *   JBUFSZ, IBUFF(*), IRET
      LOGICAL   DOSUM, DOMSG
      REAL      BUFF1(*), BUFF2(*), CATR(256)
C
      CHARACTER KEYS(7)*8, NAME*48, MDTYP(4)*8, ERRTXT*40, UMET*4
      INTEGER   JCOMP, JNCOMP, CCOUNT, XNCOMP, MXCMP, CURCMP, JT,
     *   NCOMP, J, MCOMP, VO, BO, ISIZE, INIO, MMCOMP, NNCOR, IDATA, UV,
     *   LLREC, IAPBUF, IAPCC0, IAPCT, LMCOMP, IAPTMP, VIS, WRK, LLNMOD,
     *   MCHAN, JNCS, JNCF, KAP, SFLAG, APSIZ, MXCC, INIO2, LUNC, VOL,
     *   INDEX, ITYPE, NIOUT, KBIND, LENBU, LENMOD, JLREC, JNREC, FINDI,
     *   FINDO, I, LUNI, LUNO, ITIME(3), IBIND, LFIELD, LMOD(4), NKEY,
     *   KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, MAJKOL, MINKOL, PAKOL,
     *   IPCLST, IPCDNE, NCALL, NTIMES, MODTYP, LRPARM, LF1, LF2, NEED,
     *   MSGSAV, LCHAN, I1, I2, FLD1, FLD2, CHN1, CHN2, UCOUNT, UTARG,
     *   KLNBL2(256)
      REAL      XXOFF, YYOFF, ZZOFF, RECORD(20), FACT2(4), CPA, SPA,
     *   XMAJ, XMIN, ABFACG, XYZ(3), XP(3), UMAT(3,3), PMAT(3,3), UUU
      DOUBLE PRECISION XTLST, PCTOT, PCLST, XRA, XDEC, XPR, YPR, CONST,
     *   CONST2
      LOGICAL   T, F, DIVIDE, WESET
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMOD.INC'
      LOGICAL   DONE(MAXFLD), ONZE
      INTEGER   IAPCC, MFR, NFR, LFR, MMCMP(MAXMOD)
      DOUBLE PRECISION FFRAC
      REAL      FBUFF(MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      PARAMETER (CONST = DG2RAD * TWOPI)
      SAVE ONZE, FBUFF
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5) ,MAJKOL),               (KOLS(6), MINKOL),
     *   (KOLS(7), PAKOL)
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ',
     *   'TYPE OBJ', 'MAJOR AX', 'MINOR AX', 'POSANGLE'/
      DATA LMOD /4, 7, 7, 6/
      DATA MDTYP /'Point   ', 'Gaussian', 'Unknown ', 'Sphere  '/
      DATA VO, BO, MXCC /0, 1, 1024/
      DATA LUNI, LUNO, LUNC /22,23,29/
      DATA T, F /.TRUE.,.FALSE./, ONZE/.FALSE./
C-----------------------------------------------------------------------
      UMET = 'DFT'
      CALL FILL (MAXMOD, 0, MMCMP)
C                                       CONST2 converts FWHM(deg) to
C                                       coefficients for u*u, v*v, u*v
      CONST2 = DG2RAD * (PI / 1.17741022D0) * SQRT (0.5D0)
C                                       Check if divide.
      DIVIDE = OPCODE.EQ.'DIV '
C                                       Tell kind of operation.
      IF (DIVIDE) THEN
         MSGTXT = 'VISDFT: Begin DFT component division'
      ELSE
         MSGTXT = 'VISDFT: Begin DFT component subtraction'
         END IF
      CALL MSGWRT (2)
      NNCOR = 1
C                                       Get un-compressed UV increments
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, LRPARM,
     *   LLREC)
      LFIELD = 0
      CCOUNT = 0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      WESET = (MODMAX.LE.0) .AND. (.NOT.DOPTMD)
      MCHAN = NCHAN
      IF (WESET) THEN
         MODMAX = LF2 - LF1 + 1
         DO 5 I = 1,MODMAX
            MODFLD(I) = I + LF1 - 1
            MODCHN(I) = CHANEL
            MODNCH(I) = NCHAN
            MODCCV(I) = ABS(CCVER(MODFLD(I)))
            MODCCB(I) = NSUBG(MODFLD(I))
 5          CONTINUE
         FLD1 = LF1
         FLD2 = LF2
         CHN1 = CHANEL
         CHN2 = CHANEL + NCHAN - 1
      ELSE IF (MODMAX.GT.0) THEN
         MCHAN = 0
         FLD1 = 100000
         FLD2 = 0
         CHN1 = FLD1
         CHN2 = 0
         DO 6 I = 1,MODMAX
            MCHAN = MAX (MCHAN, MODNCH(I))
            FLD1 = MIN (FLD1, MODFLD(I))
            FLD2 = MAX (FLD2, MODFLD(I))
            CHN1 = MIN (CHN1, MODCHN(I))
            CHN2 = MAX (CHN2, MODCHN(I)+MODNCH(I)-1)
 6          CONTINUE
         END IF
      IF (MODMAX.GT.0) THEN
         WRITE (MSGTXT,1006) FLD1, FLD2, CHN1, CHN2, MODMAX
         CALL REFRMT (MSGTXT, ' ', I)
         CALL MSGWRT (2)
         END IF
C                                       Decide component type.
C                                       From model passed
      IF (DOPTMD) THEN
         MODTYP = PARMOD(1) + 0.5
         IF (MODTYP.EQ.2) MODTYP = 3
         XNCOMP = 1
         MODMAX = 1
         MODFLD(1) = LF1
         MODCHN(1) = CHANEL
         MODNCH(1) = NCHAN
         MODCCV(1) = 0
         MODCCB(1) = 1
         MMCMP(1) = 1
         WESET = .TRUE.
C                                       From CC table, field 1
      ELSE
         LFIELD = LF1
C                                       Get field info. if nec.
         IF (.NOT.NGRDAT) THEN
            CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       If NGRDAT read CLEAN CATBLK.
         IF (NGRDAT) THEN
            ERRTXT = 'READING CLEAN CATBLK'
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBLK,
     *         'REST', IBUFF(2049), IRET)
            IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
            END IF
C                                       For point model
         MODTYP = 0
         XNCOMP = 0
C                                       check all CC files
         MFR = 0
         LFR = 0
         DO 10 I = 1,MODMAX
            IF (MODCHN(I).NE.LFR) THEN
               MFR = MFR + 1
               LFR = MODCHN(I)
               END IF
            LFIELD = MODFLD(I)
            JNREC = 1
            JLREC = 0
            NKEY = 0
C                                       Make sure this header applies
C                                       to this image
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBL2,
     *         'REST', BUFF1, IRET)
            IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
               ERRTXT =  'READ HEADER WITH CATIO'
               GO TO 990
               END IF
C                                       Tabini can update header on disk
            ERRTXT = 'OPENING CLEAN COMPS FILE'
            CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *         MODCCV(I), KLNBL2, LUNC, NKEY, JNREC, JLREC, BUFF1,
     *         IBUFF, IRET)
            IF (IRET.GT.1) GO TO 990
            IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
            DONE(I) = F
            XNCOMP = XNCOMP + NCLNG(LFIELD) - MODCCB(I) + 1
            MMCMP(MFR) = MMCMP(MFR) + NCLNG(LFIELD) - MODCCB(I) + 1
C                                       More complex models
C                                       Find columns (physical)
            IF ((JLREC.GT.3) .AND. (NCLNG(LFIELD).GE.MODCCB(I)))
     *         THEN
               NKEY = JLREC
               IF (JLREC.GT.7) NKEY = 7
               ERRTXT = 'FINDING REQUIRED CC COLUMNS'
               CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
               IF (IRET.NE.0) GO TO 990
C                                       Read 1st record
               JCOMP = MODCCB(I)
               CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1090) IRET, JCOMP
                  GO TO 995
                  END IF
C                                       Get model type.
               ITYPE = RECORD(TYPKOL) + 0.5
               IF (LFIELD.EQ.LF1) MODTYP = ITYPE
               IF (MODTYP.NE.ITYPE) THEN
                  IF ((MODTYP.EQ.0) .OR. (ITYPE.EQ.0)) THEN
                     MSGTXT = 'VISDFT: will treat points as extended'
                     CALL MSGWRT(6)
                     MODTYP = MAX (MODTYP, ITYPE)
                  ELSE
                     WRITE (MSGTXT,1000) MODTYP, ITYPE
                     IRET = 8
                     GO TO 995
                     END IF
                  END IF
               END IF
C                                       Close CLEAN components file.
            CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
 10         CONTINUE
         END IF
C                                       Bad model type.
      IF ((MODTYP.NE.0) .AND. (MODTYP.NE.1) .AND. (MODTYP.NE.3)) THEN
         IRET = 8
         WRITE (MSGTXT,1002) MODTYP
         GO TO 995
         END IF
C                                       Check for point model.
      LFIELD = LF1 - 1
C                                       Tell model type once
      IF (.NOT.ONZE) THEN
C                                       Tell model type
         MSGTXT = 'VISDFT: Model components of type '//MDTYP(MODTYP+1)
         CALL MSGWRT (2)
         ONZE = .NOT. ONZE
C                                       Check Buffer size
         IF ((XNCOMP.GT.10) .AND. (JBUFSZ/2.LT.4096)) THEN
            MSGTXT = 'VISDFT: SCRATCH BUFFER TOO SMALL FOR CCs!'
            CALL MSGWRT (8)
            IRET = 9
            GO TO 999
            END IF
         END IF
C                                       Set model length
      LENMOD = LMOD(MODTYP+1)
C                                       Determine size of uv I/O and
C                                       the number of CC that will fit.
      LENBU = ((JBUFSZ-2*NBPS) / 2) / (LLREC*2)
C                                       How much data fits in AP?
      JT = 15 + LENBU * LLREC
      IF (MXCC.GT.JT) JT = MXCC
      NEED = JT + MCHAN + (XNCOMP+2)*LENMOD
      NEED = NEED / 1024
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = JT + MCHAN + (XNCOMP/10+2)*LENMOD
         NEED = NEED / 1024
         NEED = MIN (32*1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
             IRET = 8
             MSGTXT = 'VISDFT CANNOT GET NEEDED MEMORY'
             GO TO 995
             END IF
          END IF
      APSIZ = PSAPNW * 1024
      CALL QRLSE
      MXCMP = (APSIZ - JT - MCHAN)  / LENMOD
      MXCMP = MXCMP - 2
      CALL RFILL (20, 0.0, RECORD)
C                                       Set AP pointers.
C                                       UV=UV pointer, VIS=vis pointer
C                                       IAPCC0=CLEAN components pointer.
      IDATA = 12 + MCHAN
      UV = IDATA + ILOCU
      IAPCC0 = 15 + LENBU * LLREC + MCHAN
      IAPCC0 = MAX (IAPCC0, MXCC)

      LLNMOD = LENMOD
C                                       Compute number of passes.
      NTIMES = (1.0 * XNCOMP) / MXCMP + 0.99999
      NTIMES = MAX (NTIMES, 1)
C                                       Only one pass allowed for
C                                       division.  No can do.
      IF (NTIMES.GT.1) THEN
         IF (DIVIDE) THEN
            ERRTXT = 'TOO MANY COMPONENTS FOR DIVISION'
            IRET = 10
            GO TO 990
         ELSE IF (.NOT.WESET) THEN
            ERRTXT = 'EXTERNAL FACET LIST WILL NOT FIT'
            IRET = 10
            GO TO 990
            END IF
         END IF
C                                       Fix for Division scaling
      ABFACG = 1.0
      IF (DIVIDE) ABFACG = ABS(FACGRD(1))
C                                       Open uv files.
C                                       Set input file name.
      IF (DISKI.LE.0) THEN
         VOL = SCRVOL(CNOSCI)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1, NAME, IRET)
      ELSE
         VOL = DISKI
         CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
         END IF
C                                       Open input file.
      ERRTXT = 'OPEN-FOR-READ VIS FILE'
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create scratch file if necessary
      IF ((DISKO.LE.0) .AND. (CNOSCO.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         ERRTXT = 'CREATING SCRATCH FILE'
         CALL SCREAT (ISIZE, BUFF2, IRET)
         CNOSCO = NSCR
         IF (IRET.GT.0) THEN
            IF (IRET.EQ.1) ERRTXT = 'NO SPACE FOR SCRATCH FILE'
            GO TO 990
            END IF
C                                       End if creating scratch file
         END IF
C                                       Open vis file for write.
      IF (DISKO.LE.0) THEN
         VOL = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
      ELSE
         VOL = DISKO
         CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
         END IF
      ERRTXT = 'OPEN-FOR-WRITE VIS FILE'
      CALL ZOPEN (LUNO, FINDO, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop, subtracting Max Component
C                                       In AP each pass
      I1 = 1
      DO 500 NCALL = 1,NTIMES
         I2 = I1 - 1
C                                       Setup for % done messages.
         PCTOT = NVIS
         PCLST = PCTOT * (NCALL - 1)
         IPCLST = (100. / NTIMES ) * (NCALL - 1)
C                                       Set AP loc for next CC load
         IAPCT = IAPCC0
C                                       Determine. no. this pass.
         MMCOMP = 0
         NCOMP = MIN (MXCMP, XNCOMP)
C                                       Grab AP.
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            IRET = 10
            MSGTXT = 'VISDFT: BIZARRE FAILURE TO GET AP MEMORY'
            GO TO 995
            END IF
C                                       Initialize REAL time clock for
C                                       AP roller.
         CALL ZTIME (ITIME)
         XTLST = (ITIME(1) * 60.00) + ITIME(2) + (ITIME(3) / 60.0)
C                                       If Not single component model.
         IF (.NOT.DOPTMD) THEN
C                                       Find next FIELD.
C                                       Loop back here for next field.
 70         I2 = I2 + 1
C                                       See if done.
            IF (I2.GT.MODMAX) GO TO 150
            IF (DONE(I2)) GO TO 70
            LFIELD = MODFLD(I2)
C                                       See if there are CC's.
            IF ((MODCCB(I2).GT.NCLNG(LFIELD))) GO TO 70
C                                       Get field info. if nec.
            IF (.NOT.NGRDAT) THEN
               CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       If NGRDAT read CLEAN CATBLK.
            IF (NGRDAT) THEN
               ERRTXT = 'READING CLEAN CATBLK'
               CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *            KLNBLK, 'REST', IBUFF(2049), IRET)
               IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
               END IF
C                                       Set field center offsets.
            XXOFF = DXCG(LFIELD) * CCROT + DYCG(LFIELD) * SSROT
            YYOFF = DYCG(LFIELD) * CCROT - DXCG(LFIELD) * SSROT
            ZZOFF = DZCG(LFIELD)
            CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD),
     *         MAPROT, XRA, XDEC)
            IF (DO3DIM) THEN
               CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
            ELSE
               CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *            PMAT)
               END IF
C                                       Load CLEAN components into AP.
C                                       Open components file.
            JNREC = 1
            JLREC = 0
            NKEY = 0
            ERRTXT = 'OPENING CLEAN COMPS FILE'
            CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *         MODCCV(I2), KLNBLK, LUNC, NKEY, JNREC, JLREC, BUFF1,
     *         IBUFF, IRET)
            IF (IRET.GT.1) GO TO 990
            JCOMP = MODCCB(I2)
C                                       Find columns (physical)
            NKEY = JLREC
            TYPKOL = 7
            IF (JLREC.GT.7) NKEY = 7
            ERRTXT = 'FINDING REQUIRED CC COLUMNS'
            CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
            IF (IRET.GT.0) GO TO 990
C                                       Make sure that there are some
            IF (IBUFF(5).LE.0) GO TO 140
            IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
C                                       Loop loading components.
            IAPBUF = 10
            CURCMP = MMCOMP + 1
C                                       Check next component
            IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 140
            DO 130 J = CURCMP,NCOMP,MXCC
               JT = J - 1
               JNCOMP = 0
               MCOMP = NCOMP - J + 1
               IF (MCOMP.GT.MXCC) MCOMP = MXCC
               IF (MCOMP.GT.(NCLNG(LFIELD)-MODCCB(I2)+1))
     *            MCOMP = NCLNG(LFIELD) - MODCCB(I2) + 1
               DO 110 I = 1,MCOMP
C                                       Check if finished field
                  IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 120
                  CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1090) IRET, JCOMP
                     GO TO 995
                     END IF
C                                        Check that point comp.
                  JCOMP = JCOMP + 1
                  JT = JT + 1
                  IF (JLREC.LE.3) THEN
                     ITYPE = 0
                  ELSE
                     ITYPE = RECORD(TYPKOL) + 0.5
                     END IF
                  IF ((ITYPE.NE.MODTYP) .AND. (ITYPE.NE.0)) THEN
                     WRITE (MSGTXT,1070) LFIELD, JCOMP-1
                     CALL MSGWRT (6)
                     END IF
                  IF (((ITYPE.EQ.MODTYP) .OR. (ITYPE.EQ.0)) .AND.
     *               (IRET.EQ.0)) THEN
C                                       Check negative component limit
                     DONE(I2) = (NONEG.AND.(RECORD(FLXKOL).LE.0.0))
     *                  .OR. (ABS(RECORD(FLXKOL)).LT.LIMFLX)
                     IF (DONE(I2)) THEN
                        NCLNG(LFIELD) = JCOMP - 1
                        GO TO 120
                        END IF
C                                       If req. sum flux
                     IF (DOSUM) THEN
                        FLUXG(LFIELD) = FLUXG(LFIELD) + RECORD(FLXKOL)
                        TFLUXG = TFLUXG + RECORD(FLXKOL)
                        END IF
                     JNCOMP = JNCOMP + 1
                     CCOUNT = CCOUNT + 1
                     XP(1) = (RECORD(RAKOL) + XPOFF(LFIELD)) * CONST
                     XP(2) = (RECORD(DECKOL) + YPOFF(LFIELD)) * CONST
                     XP(3) = 0.0
                     CALL PRJMUL (2, XP, UMAT, XYZ)
                     BUFF1(JNCOMP) = XYZ(1) + XXOFF
                     BUFF1(1024+JNCOMP) = XYZ(2) + YYOFF
                     BUFF1(2048+JNCOMP) = XYZ(3) + ZZOFF
C                                       Handle scaling for division
                     BUFF1(3072+JNCOMP) = ABFACG * RECORD(FLXKOL)
C                                       Gaussian
                     IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
                        IF (ITYPE.EQ.0) THEN
                           BUFF2(JNCOMP) = 0.0
                           BUFF2(1024+JNCOMP) = 0.0
                           BUFF2(2048+JNCOMP) = 0.0
                        ELSE
                           CPA = COS (DG2RAD*RECORD(PAKOL))
                           SPA = SIN (DG2RAD*RECORD(PAKOL))
                           XMAJ = RECORD(MAJKOL) * CONST2
                           XMIN = RECORD(MINKOL) * CONST2
                           BUFF2(JNCOMP) = - (((CPA * XMAJ)**2) +
     *                        (SPA * XMIN)**2)
                           BUFF2(1024+JNCOMP) = - (((SPA * XMAJ)**2)
     *                        + (CPA * XMIN)**2)
                           BUFF2(2048+JNCOMP) = - 2.0 * CPA * SPA *
     *                        (XMAJ*XMAJ - XMIN*XMIN)
                           END IF
C                                       Sphere
                     ELSE IF (MODTYP.EQ.3) THEN
                        BUFF1(3072+JNCOMP) = 3.0 * BUFF1(3072+JNCOMP)
                        IF (ITYPE.EQ.0) THEN
                           BUFF2(JNCOMP) = 0.0
                        ELSE
                           BUFF2(JNCOMP) = RECORD(MAJKOL) * 0.109662271
                           END IF
                        BUFF2(1024+JNCOMP) = 0.1
                        END IF
                     END IF
 110              CONTINUE
C                                       Load components
 120           IF (JNCOMP.GT.0) THEN
                  LMCOMP = JNCOMP
                  MMCOMP = MMCOMP + LMCOMP
C                                       Load into AP
                  IAPBUF = 10
C                                       x component
                  CALL QPUT (BUFF1, IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 1
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       y component
                  CALL QPUT (BUFF1(1025), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 2
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       z component
                  CALL QPUT (BUFF1(2049), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 3
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Flux density
                  CALL QPUT (BUFF1(3073), IAPBUF, LMCOMP, 2)
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPCT, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Gaussian
                  IF (MODTYP.EQ.1) THEN
C                                       Coef 1.
                     CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 4
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Coef 2.
                     CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 5
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Coef 3.
                     CALL QPUT (BUFF2(2049), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 6
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
                     END IF
C                                       Sphere
                  IF (MODTYP.EQ.3) THEN
C                                       Radius
                     CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 4
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
C                                       Minimum argument
                     CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                     IAPTMP = IAPCT + 5
                     CALL QWD
                     CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                     CALL QWR
                     END IF
                  IAPCT = IAPCT + (LLNMOD * LMCOMP)
                  END IF
C                                       Check if finished field.
               IF ((JCOMP.GT.NCLNG(LFIELD)) .OR. DONE(I2))
     *            GO TO 140
 130           CONTINUE
C                                       Close CLEAN components file.
 140        CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
C                                       Update field sub. count.
            NSUBG(LFIELD) = JCOMP
C                                       Check if need another field.
            IF (JT.LT.NCOMP) GO TO 70
C                                      Check no. comps.
 150        IF ((MMCOMP.EQ.0) .AND. (NCALL.EQ.1)) THEN
               ERRTXT = 'NO POINT COMPONENTS FOUND'
               IRET = 1
               GO TO 990
               END IF
C                                       No comps on later pass is OK
            IF (MMCOMP.LE.0) GO TO 510
C                                       Load correlator factors
            CALL GETCTL (CATR, FACT2, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Else, single component model.
         ELSE
            I2 = 1
            CALL RFILL (LENMOD, 0.0, BUFF1)
            BUFF1(2) = PTRAOF * CONST / 3600.D0
            BUFF1(3) = PTDCOF * CONST / 3600.D0
C                                       single comp uses W term
            XPR = PTRAOF / 206264.81D0
            YPR = PTDCOF / 206264.81D0
            BUFF1(4) = -(SQRT (1.0D0 - XPR*XPR - YPR*YPR) - 1.0D0) *
     *         206264.81D0 * CONST / 3600.D0
C                                       Point
            IF (MODTYP.EQ.0) BUFF1(1) = PTFLX
C                                       Gaussian
            IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
               CPA = COS (DG2RAD * PARMOD(4))
               SPA = SIN (DG2RAD * PARMOD(4))
               XMAJ = PARMOD(2) * CONST2 * 2.77777778E-4
               XMIN = PARMOD(3) * CONST2 * 2.77777778E-4
               BUFF1(5) = -(((CPA * XMAJ)**2) + (SPA * XMIN)**2)
               BUFF1(6) = -(((SPA * XMAJ)**2) + (CPA * XMIN)**2)
               BUFF1(7) = -2.0 *  CPA * SPA * (XMAJ*XMAJ - XMIN*XMIN)
               BUFF1(1) = PTFLX
               END IF
C                                       Uniform sphere
            IF (MODTYP.EQ.3) THEN
               BUFF1(5) = PARMOD(2) * 0.109662271 * 2.7777778E-4
               BUFF1(1) = PTFLX * 3.0
               BUFF1(6) = 0.1
               END IF
            MMCOMP = 1
            IAPCC = IAPCC0
            CALL QPUT (BUFF1, IAPCC, LLNMOD, 2)
            IAPCT = IAPCC + LLNMOD
C                                       Set Stokes' for point model
            FACT2(1) = 1.0
            FACT2(2) = 1.0
            IF (DOALL4) THEN
               FACT2(3) = 1.0
               FACT2(4) = 1.0
            ELSE
               FACT2(3) = 0.0
               FACT2(4) = 0.0
               END IF

            VOFF = 0
C                                       RR,LL etc.
            IF (ICOR0.LT.0) THEN
C                                       Q?
               IF (KSTOK.EQ.2) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = 1.0
                  VOFF = (3 - ABS (ICOR0)) * INCS
               ELSE IF (KSTOK.EQ.3) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = -1.0
                  VOFF = (3 - ABS (ICOR0)) * INCS
               ELSE IF (KSTOK.EQ.4) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = -1.0
                  VOFF = 0
                  END IF
C                                       True Stokes'
            ELSE
               IF ((KSTOK.GE.2) .AND. (KSTOK.LE.4)) THEN
                  FACT2(1) = 1.0
                  FACT2(2) = 0.0
                  VOFF = (KSTOK - ICOR0) * INCS
                  END IF
               END IF
            NSTOK = 1
            IF ((CATR(KRCIC+JLOCS).LT.0.0) .AND. (NCOR.GE.2)) THEN
               NSTOK = 2
               IF (DOALL4) NSTOK = NCOR
C                                       TEMPORARY
               NSTOK = NCOR
               END IF
            IF ((NSTOK.EQ.2) .AND. (ABS (ICOR0).EQ.2)) NSTOK = 1
C                                       End if not single comp. model
            END IF
C                                       Correct for FACGRD
         NNCOR = NSTOK
         FACT2(1) = FACT2(1) * FACGRD(1)
         FACT2(2) = FACT2(2) * FACGRD(1)
         FACT2(3) = FACT2(3) * FACGRD(1)
         FACT2(4) = FACT2(4) * FACGRD(1)
         WRK = MCHAN + 2
C                                       replace flag only for subtract
C                                       where DOALL4 always false
         UUU = 1.0
         IF (FACGRD(2).EQ.0.0) UUU = 0.0
         CALL QPUT (UUU, WRK+NNCOR, 1, 2)
         CALL QPUT (FACT2, WRK, NNCOR, 2)
         SFLAG = 1
C                                       Set flag for UPOL and RL,LR data
         IF ((KSTOK.EQ.3) .AND. (ICOR0.LT.0)) SFLAG = -1
C                                       Set vis pointer
         VIS = IDATA + LRPARM + (CHANEL-1) * JNCF + (VOFF/INCS)*JNCS
         CALL QWAIT
C                                       Init for read & write
C                                       visibility file
C                                       Init vis file for write
         ERRTXT = 'INIT-FOR-READ VIS FILE'
         CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF1, BO, IBIND, IRET)
         IF (IRET.NE.0) GO TO 990
         UTARG = 1
         UCOUNT = 0
C                                       Init vis file for read.
         ERRTXT = 'INIT-FOR-WRITE VIS FILE'
         CALL UVINIT ('WRIT', LUNO, FINDO, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF2, BO, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Subtract model from vis data.
C                                       Loop:  Read vis. record.
 200        CONTINUE
            ERRTXT = 'READING VIS FILE'
            CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO2, IBIND, IRET)
            INIO = INIO2
            IF (IRET.NE.0) GO TO 990
C                                       Exit if no more data
            IF (INIO.LE.0) GO TO 300
            UCOUNT = UCOUNT + INIO
            IF (UCOUNT.GT.UTARG) THEN
               WRITE (MSGTXT,1200) UTARG
               CALL MSGWRT (2)
               UTARG = UTARG + 200000
               END IF
C                                       Uncompress Vis and put in AP
            CALL BUFPUT (INIO, NRPARM, LREC, LLREC, IDATA, BUFF1(IBIND),
     *           IBUFF(1))
C                                       Loop over frequency groups
            LFR = 0
            NFR = 0
            IAPCC = IAPCC0
            DO 220 I = I1,I2
               IF (MODCHN(I).NE.LFR) THEN
                  NFR = NFR + 1
                  LFR = MODCHN(I)
                  IF (NFR.GT.1) IAPCC = IAPCC + MMCMP(NFR-1) * LENMOD
                  MMCOMP = MMCMP(NFR)
                  LCHAN = MODNCH(I)
                  VIS = IDATA + LRPARM + (LFR-1)*JNCF + (VOFF/INCS)*JNCS
C                                       Fill frequency table
C                                       Double array to force accuracy
                  FFRAC = (FREQG(LFR) / FREQ) - 1.0D0
                  FBUFF(1) = FFRAC
                  IF (LCHAN.GT.1) THEN
                     DO 210 J = 2,LCHAN
                        INDEX = LFR + J - 1
                        FFRAC = (FREQG(INDEX) / FREQ) - 1.0D0
                        FBUFF(J) = FFRAC
 210                    CONTINUE
                     END IF
                  CALL QWD
                  CALL QPUT (FBUFF, 2, MCHAN, 2)
                  CALL QWAIT
C                                       Do the arithmetic: divide
                  IF (DIVIDE) THEN
C                                       Point
                     IF (MODTYP.EQ.0) CALL QPTDIV (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Gaussian
                     IF (MODTYP.EQ.1) CALL QGADIV (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Sphere
                     IF (MODTYP.EQ.3) CALL QSPDIV (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Subtract
                  ELSE
C                                       Point
                     IF (MODTYP.EQ.0) CALL QXXPTS (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR,
     *                  SFLAG)
C                                       Gaussian
                     IF (MODTYP.EQ.1) CALL QGASUB (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR,
     *                  SFLAG)
C                                       Sphere
                     IF (MODTYP.EQ.3) CALL QSPSUB (IAPCC, UV, VIS,
     *                  LLREC, JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR,
     *                  SFLAG)
                     END IF
                  END IF
 220           CONTINUE
            CALL QWR
C                                       Get UVs from AP (maybe pack UV)
            CALL BUFGET (INIO, NRPARM, LREC, LLREC, IDATA, BUFF2(KBIND),
     *         IBUFF(1))
C                                       Write vis record.
            NIOUT = INIO
            ERRTXT = 'WRITING VIS FILE'
            CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Check if time for % done
C                                       message.
            PCLST = PCLST + NIOUT
            IPCDNE = (100.0 / NTIMES) * (PCLST / PCTOT) + 0.5
            IPCDNE = IPCDNE - MOD (IPCDNE, 10)
C                                       Write % done message.
            IF ((IPCDNE.GT.IPCLST) .AND. (DOMSG)) THEN
               WRITE (MSGTXT,1240) IPCDNE
               IF (IPCDNE.LE.100) CALL MSGWRT (2)
               IPCLST = IPCDNE
               END IF
C                                       Check if time for AP roller
            CALL QROLL (IAPCT, IBUFF, JBUFSZ, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 200
C                                       Finish write
 300     CALL QRLSE
         NIOUT = 0
         ERRTXT = 'FLUSHING VIS FILE'
         CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, NIOUT, KBIND, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Update no. comp. left.
         XNCOMP = XNCOMP - MXCMP
C                                       Check if done.
         IF (XNCOMP.LE.0.01) GO TO 510
C                                       Input now output.
C                                       Close old input
         CALL ZCLOSE (LUNI, FINDI, IRET)
C                                       Set new input file name.
         IF (DISKO.LE.0) THEN
            VOL = SCRVOL(CNOSCO)
            CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, NAME, IRET)
         ELSE
            VOL = DISKO
            CALL ZPHFIL ('UV', VOL, CNOSCO, 1, NAME, IRET)
            END IF
C                                       Open new input file.
         ERRTXT = 'OPEN-FOR-READ VIS FILE'
         CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Open vis file for write.
C                                       Set output file name.
C                                       End big loop, N Comps per pass
         I1 = I2 + 1
 500     CONTINUE
C                                       Close files
 510  CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IF (WESET) MODMAX = 0
      IRET = 0
      GO TO 999
C                                       Error
 990  WRITE(MSGTXT,1990,ERR=999) IRET, ERRTXT
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VISDFT: MODEL TYPES =',I2,I2,' INCOMPATIBLE')
 1002 FORMAT ('VISDFT: ILLEGAL MODEL TYPE =',I3)
 1006 FORMAT ('VISDFT: fields',I5,' -',I5,' chns',I5,' -',I5,' in',I5,
     *   ' CC models')
 1070 FORMAT ('VISDFT WRONG MODTYP AT FIELD, COMP',I5,I9)
 1090 FORMAT ('VISDFT: ERROR',I5,' READING CLEAN COMPS REC',I5)
 1200 FORMAT ('VISDFT at visibility number',I12)
 1240 FORMAT ('Model computation is ',I5,' percent complete')
 1990 FORMAT ('VISDFT: ERROR',I5,' ',A)
      END
