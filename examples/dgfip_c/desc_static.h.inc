
#ifndef _STRUCT_H_
#define _STRUCT_H_

#include "conf.h"

struct S_desc_var
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
};

typedef struct S_desc_contexte
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int modcat;
  char *libelle;
} T_desc_contexte;

typedef struct S_desc_famille
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int modcat;
  char *libelle;
} T_desc_famille;

typedef struct S_desc_revenu
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  T_desc_var *liee;
  char *libelle;
  char *code;
} T_desc_revenu;

typedef struct S_desc_revenu_correc
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int nat_code;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  T_desc_var *liee;
  char *libelle;
} T_desc_revenu_correc;

typedef struct S_desc_variation
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
} T_desc_variation;

typedef struct S_desc_penalite
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
} T_desc_penalite;

typedef struct S_desc_restituee
{
  char *nom;
  int indice;
  long type_donnee;
  int type;
  int primrest;
#ifdef FLG_GENERE_LIBELLE_RESTITUEE
  char *libelle;
#endif /* FLG_GENERE_LIBELLE_RESTITUEE */
#ifdef FLG_EXTRACTION
  int est_extraite;
#endif /* FLG_EXTRACTION */
} T_desc_restituee;

typedef struct S_desc_debug
{
  char *nom;
  int indice;
  long type_donnee;
  T_discord * (*verif)(T_irdata *);
  int classe;
  int priorite;
  int categorie_TL;
  int cotsoc;
  int ind_abat;
  int acompte;
  int avfisc;
  int rapcat;
  int sanction;
  int modcat;
  int nat_code;
  T_desc_var *liee;
#ifdef FLG_EXTRACTION
  int est_extraite;
#endif /* FLG_EXTRACTION */
} T_desc_debug;

typedef struct S_desc_err
{
  char *nom;
  T_erreur *erreur;
} T_desc_err;

typedef struct S_desc_call
{
  int num;
  int (*proc)(T_irdata *irdata);
} T_desc_call;

typedef struct S_desc_ench
{
  char *nom;
  void (*proc)(T_irdata *irdata);
} T_desc_ench;

typedef struct S_desc_verif
{
  int num;
  void (*proc)(T_irdata *irdata);
} T_desc_verif;

extern T_desc_contexte desc_contexte[];
extern T_desc_famille desc_famille[];
extern T_desc_revenu desc_revenu[];
extern T_desc_revenu_correc desc_revenu_correc[];
extern T_desc_variation desc_variation[];
extern T_desc_restituee desc_restituee[];

#endif /*_STRUCT_H_ */
