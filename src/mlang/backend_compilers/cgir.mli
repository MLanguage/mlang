(*
  Copyright DGFiP, 20 septembre 2022.
  Contributeur: David MICHEL <david.michel1@dgfip.finances.gouv.fr>.

  Ce logiciel est régi par la licence CeCILL soumise au droit français et
  respectant les principes de diffusion des logiciels libres. Vous pouvez
  utiliser, modifier et/ou redistribuer ce programme sous les conditions de la
  licence CeCILL telle que diffusée par le CEA, le CNRS et l'INRIA sur le site
  "http://www.cecill.info".

  Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris
  connaissance de la licence CeCILL, et que vous en avez accepté les termes.
*)

type program = Truc of int

val bir_to_cgir :
  Mast.program ->
  Bir.program ->
  Bir_interface.bir_function ->
  Format.formatter ->
  unit ->
  unit

