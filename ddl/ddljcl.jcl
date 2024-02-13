//GENDDL1 JOB 241901,'Db2 create',NOTIFY=&SYSUID,
//         CLASS=A,MSGCLASS=H
//*
//* (C) Copyright IBM Corp. 2011, 2021
//*
//JOBLIB   DD DSN=DSNB10.SDSNLOAD,DISP=SHR 
//*JOBLIB   DD DSN=<DB2HLQ>.SDSNLOAD,DISP=SHR
//*
//********************************************************************
//*   CREATE STORAGE GROUP/DATABASES/TABLESPACES                     *
//********************************************************************
//CREATE  EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DBBG)
  RUN PROGRAM(DSNTIAUL) PLAN(DSNTIB11) - 
  LIB('DSNB10.DBBG.RUNLIB.LOAD') 
END    
/*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  SET CURRENT SQLID='DBBG' ;
  CREATE   STOGROUP TRNG VOLUMES ('*') ;

  CREATE   DATABASE DSNDB04 STOGROUP TRNG
           BUFFERPOOL BP1
           CCSID EBCDIC;
  CREATE   TABLESPACE MONGOTS IN DBBG
    USING STOGROUP TRNG
      PRIQTY 10000
      SECQTY 5000
      ERASE  NO
    CLOSE NO
    CCSID EBCDIC
    BUFFERPOOL BP1;
/*
//* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
//*  CREATE TABLES AND INDEXES
//* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
//* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
//*  CREATE customer TABLE
//* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
//*
//CRTABS  EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DBBG)
 RUN  PROGRAM(DSNTIAD) PLAN(DSNTIB11) -
      LIB('DSNB10.RUNLIB.LOAD')
/*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  SET CURRENT SQLID='DBBG' ;
CREATE TABLE IBMUSE1.CUSTM_TABLE 
     ( CUSTM_NO                  INTEGER NOT NULL,   
       CUSTM_NAME_FRSTNM              CHAR(15) NOT NULL,  
       CUSTM_NAME_MDLNM               CHAR(8),            
       CUSTM_NAME_LASTNM              CHAR(15) NOT NULL,  
       CUSTM_ADDR                     CHAR(35) NOT NULL,  
       CUSTM_DOB                      DATE  )
   IN IBMUSE1.MONGOTS;

CREATE TABLE IBMUSE1.LOANMSTR (
     LOANM_CAT                      CHAR(2) NOT NULL,   
     LOANM_NO                       INTEGER NOT NULL,   
     LOANM_TERM                     SMALLINT NOT NULL,  
     LOANM_TITLE                    CHAR(15),           
     LOANM_ROI                      SMALLINT NOT NULL,  
     LOANM_MAX_AMT                  INTEGER NOT NULL,   
     LOANM_MIN_AGE                  SMALLINT NOT NULL,  
     LOANM_MAX_AGE                  SMALLINT NOT NULL,  
     LOANM_ST                       CHAR(1) NOT NULL,   
     LOANM_CUST_NO                  CHAR(10) NOT NULL   )
   IN IBMUSE1.MONGOTS;

CREATE TABLE IBMUSE1.LOANRPHST (
     LOANRPH_CAT                    CHAR(2) NOT NULL,    
     LOANRPH_NO                     INTEGER NOT NULL,    
     LOANRPH_DATE                   DATE NOT NULL,       
     LOANRPH_RP_AMT                 INTEGER NOT NULL,    
     LOANRPH_OS_AMT                 INTEGER NOT NULL       )
   IN IBMUSE1.MONGOTS;

/*
//*
//CRGRACC EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 DSN SYSTEM(DBBG)
  RUN PROGRAM(DSNTIAUL) PLAN(DSNTIB11) - 
  LIB('DSNB10.DBBG.RUNLIB.LOAD')
/*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  SET CURRENT SQLID='<SQLID>' ;
  GRANT DBADM ON DATABASE IBMUSE1      TO PUBLIC;
  GRANT ALL PRIVILEGES ON TABLE IBMUSE1.CUSTM_TABLE   TO PUBLIC;
  GRANT ALL PRIVILEGES ON TABLE IBMUSE1.LOANMSTR     TO PUBLIC;
  GRANT ALL PRIVILEGES ON TABLE IBMUSE1.LOANRPHST    TO PUBLIC;
/*
//*
//********************************************************************
//**INSERT VALUES IN TO CUSTM_TABLE
//********************************************************************
//INSERT  EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=<DB2HLQ>.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
   DSN SYSTEM(DBBG)
   RUN PROGRAM(DSNTIAUL) PLAN(DSNTIB11) - 
   LIB('DSNB10.DBBG.RUNLIB.LOAD')
/*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  SET CURRENT SQLID='DBBG' ;

  Insert Into IBMUSE.CUSTM_TABLE (
       CUSTM_NO,    
       CUSTM_NAME_FRSTNM, 
       CUSTM_NAME_MDLNM,
       CUSTM_NAME_LASTNM,
       CUSTM_ADDR,
       CUSTM_DOB)
         Values (
    1000, 
    'ANDREW',
    'J',
    'PANDY',
    'TRACEY ISLAND',
    '1965-09-30' );

Insert Into IBMUSE.CUSTM_TABLE (
       CUSTM_NO,    
       CUSTM_NAME_FRSTNM, 
       CUSTM_NAME_MDLNM,
       CUSTM_NAME_LASTNM,
       CUSTM_ADDR,
       CUSTM_DOB)
         Values (
    1001, 
    'SCOTT',
    'T',
    'JONES',
    'CONNEY ESTATES',
    '1970-01-26' );

Insert Into IBMUSE.CUSTM_TABLE (
       CUSTM_NO,    
       CUSTM_NAME_FRSTNM, 
       CUSTM_NAME_MDLNM,
       CUSTM_NAME_LASTNM,
       CUSTM_ADDR,
       CUSTM_DOB)
         Values (
    1002, 
    'JOHN',
    'A',
    'NOAKES',
    'YORK SHER',
    '1966-02-02' );

Insert Into IBMUSE.CUSTM_TABLE (
       CUSTM_NO,    
       CUSTM_NAME_FRSTNM, 
       CUSTM_NAME_MDLNM,
       CUSTM_NAME_LASTNM,
       CUSTM_ADDR,
       CUSTM_DOB)
         Values (
    1003, 
    'LOUIS',
    'Z',
    'LANDER',
    'NEW TOWN',
    '1986-03-16' );

Insert Into IBMUSE.CUSTM_TABLE (
       CUSTM_NO,    
       CUSTM_NAME_FRSTNM, 
       CUSTM_NAME_MDLNM,
       CUSTM_NAME_LASTNM,
       CUSTM_ADDR,
       CUSTM_DOB)
         Values (
    1004, 
    'GRAHAM',
    'S',
    'BELL',
    'SISALY WAY',
    '1978-06-20' );

Insert Into IBMUSE1.LOANMSTR (
     LOANM_CAT,                       CHAR(2) NOT NULL,   
     LOANM_NO                       INTEGER NOT NULL,   
     LOANM_TERM                     SMALLINT NOT NULL,  
     LOANM_TITLE                    CHAR(15),           
     LOANM_ROI                      SMALLINT NOT NULL,  
     LOANM_MAX_AMT                  INTEGER NOT NULL,   
     LOANM_MIN_AGE                  SMALLINT NOT NULL,  
     LOANM_MAX_AGE                  SMALLINT NOT NULL,  
     LOANM_ST                       CHAR(1) NOT NULL,   
     LOANM_CUST_NO                  CHAR(10) NOT NULL   )
        Values (
     'XX',
     50000,
     10,
     'PERSONAL LOAN',
     5
     100000,
     10, 
     50, 
    '2'
    '1000' );

Insert Into IBMUSE1.LOANMSTR (
     LOANM_CAT,                       CHAR(2) NOT NULL,   
     LOANM_NO                       INTEGER NOT NULL,   
     LOANM_TERM                     SMALLINT NOT NULL,  
     LOANM_TITLE                    CHAR(15),           
     LOANM_ROI                      SMALLINT NOT NULL,  
     LOANM_MAX_AMT                  INTEGER NOT NULL,   
     LOANM_MIN_AGE                  SMALLINT NOT NULL,  
     LOANM_MAX_AGE                  SMALLINT NOT NULL,  
     LOANM_ST                       CHAR(1) NOT NULL,   
     LOANM_CUST_NO                  CHAR(10) NOT NULL   )
        Values (
     'AA',
     25000,
     12,
     'HOME LOAN',
     5
     1000000,
     10, 
     50, 
    '2'
    '1001' );

Insert Into IBMUSE1.LOANMSTR (
     LOANM_CAT,                       CHAR(2) NOT NULL,   
     LOANM_NO                       INTEGER NOT NULL,   
     LOANM_TERM                     SMALLINT NOT NULL,  
     LOANM_TITLE                    CHAR(15),           
     LOANM_ROI                      SMALLINT NOT NULL,  
     LOANM_MAX_AMT                  INTEGER NOT NULL,   
     LOANM_MIN_AGE                  SMALLINT NOT NULL,  
     LOANM_MAX_AGE                  SMALLINT NOT NULL,  
     LOANM_ST                       CHAR(1) NOT NULL,   
     LOANM_CUST_NO                  CHAR(10) NOT NULL   )
        Values (
     'CC',
     2500,
     2,
     'GOLD LOAN',
     6
     20000,
     20, 
     50, 
    '3'
    '1002' );

Insert Into IBMUSE1.LOANMSTR (
     LOANM_CAT,                       CHAR(2) NOT NULL,   
     LOANM_NO                       INTEGER NOT NULL,   
     LOANM_TERM                     SMALLINT NOT NULL,  
     LOANM_TITLE                    CHAR(15),           
     LOANM_ROI                      SMALLINT NOT NULL,  
     LOANM_MAX_AMT                  INTEGER NOT NULL,   
     LOANM_MIN_AGE                  SMALLINT NOT NULL,  
     LOANM_MAX_AGE                  SMALLINT NOT NULL,  
     LOANM_ST                       CHAR(1) NOT NULL,   
     LOANM_CUST_NO                  CHAR(10) NOT NULL   )
        Values (
     'DD',
     35000,
     40,
     'TRAVEL LOAN',
     6
     200000,
     20, 
     60, 
    '3'
    '1003' );

Insert Into IBMUSE1.LOANRPHST (
     LOANRPH_CAT,
     LOANRPH_NO,
     LOANRPH_DATE,
     LOANRPH_RP_AMT,
     LOANRPH_OS_AMT,
         Values (
    'XX', 
    5,
    '2011-09-23',
    5000,
    45000);

Insert Into IBMUSE1.LOANRPHST (
     LOANRPH_CAT,
     LOANRPH_NO,
     LOANRPH_DATE,
     LOANRPH_RP_AMT,
     LOANRPH_OS_AMT,
         Values (
    'AA', 
     1,
    '2012-10-13',
    3000,
    22000);

Insert Into IBMUSE1.LOANRPHST (
     LOANRPH_CAT,
     LOANRPH_NO,
     LOANRPH_DATE,
     LOANRPH_RP_AMT,
     LOANRPH_OS_AMT,
         Values (
    'CC', 
     1,
    '2015-01-16',
    1000,
    1500);

Insert Into IBMUSE1.LOANRPHST (
     LOANRPH_CAT,
     LOANRPH_NO,
     LOANRPH_DATE,
     LOANRPH_RP_AMT,
     LOANRPH_OS_AMT,
         Values (
    'DD', 
     2,
    '2019-09-01',
    10000,
    25000);

/*
//
