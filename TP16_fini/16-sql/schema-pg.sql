-- Schema
-- REMINDER: Create order IS important, dependencies first !!

-- Moyens de paiements acceptés pour régler les cotisations.
CREATE TABLE galette_paymenttypes (
  type_id integer NOT NULL,
  type_name character varying(50) NOT NULL,
  PRIMARY KEY (type_id)
);

-- Table des adhérents
CREATE TABLE galette_adherents (
    id_adh integer NOT NULL,
    -- Le statut indique quel rôle joue l'adhérent dans l'association (membre,
    -- président, secrétaire, etc.).
    id_statut integer DEFAULT '10' NOT NULL,
    id_profile integer,
    nom_adh character varying(255) DEFAULT '' NOT NULL,
    prenom_adh character varying(255) DEFAULT '' NOT NULL,
    -- Parfois les gens ne donnent pas d'e-mail, qui reste alors NULL
    email_adh character varying(255),
    login_adh character varying(255) DEFAULT '' NOT NULL,
    -- Le mot de passe stocké n'est PAS le mot de passe en clair. C'est un
    -- haché solide (il est impératif de se renseigner *vraiment* sur les hachages
    -- sécurisés de mot de passe, n'importe quel hachaque ne convient pas).
    mdp_adh character varying(255) DEFAULT '' NOT NULL,
    date_crea_adh date DEFAULT '19010101' NOT NULL,
    date_modif_adh date DEFAULT '19010101' NOT NULL,
    -- Booléen qui indique si le compte est actif ou désactivé
    activite_adh boolean DEFAULT FALSE,
    -- Adhérents exempts de cotisation
    bool_exempt_adh boolean DEFAULT FALSE,
    -- Champ à discuter.
    date_echeance date,
    PRIMARY KEY (id_adh)
);

CREATE TABLE galette_types_cotisation (
  id_type_cotis integer NOT NULL,
  libelle_type_cotis character varying(255) DEFAULT '' NOT NULL,
  -- Booléen qui indique si ce type de cotisation prolonge ou non l'adhésion.
  -- Un don simple ne prolonge par exemple pas l'adhésion
  cotis_extension boolean DEFAULT FALSE,
  PRIMARY KEY (id_type_cotis)
);

-- Une transaction est un paiement effectué à l'association, qui
-- est ensuite réparti en plusieurs cotisations.
CREATE TABLE galette_transactions (
    trans_id integer NOT NULL,
    trans_date date DEFAULT '19010101' NOT NULL,
    trans_amount real DEFAULT '0',
    trans_desc character varying(255) NOT NULL DEFAULT '',
    id_adh integer REFERENCES galette_adherents (id_adh) ON DELETE RESTRICT ON UPDATE CASCADE,
    PRIMARY KEY (trans_id)
);

CREATE TABLE galette_cotisations (
    id_cotis integer NOT NULL,
    id_adh integer REFERENCES galette_adherents (id_adh) ON DELETE RESTRICT ON UPDATE CASCADE,
    id_type_cotis integer REFERENCES galette_types_cotisation (id_type_cotis) ON DELETE RESTRICT ON UPDATE CASCADE,
    montant_cotis real DEFAULT '0',
    type_paiement_cotis integer REFERENCES galette_paymenttypes (type_id) ON DELETE RESTRICT ON UPDATE CASCADE NOT NULL,
    date_enreg date DEFAULT '19010101' NOT NULL,
    date_debut_cotis date DEFAULT '19010101' NOT NULL,
    date_fin_cotis date DEFAULT '19010101' NOT NULL,
    trans_id integer DEFAULT NULL REFERENCES galette_transactions (trans_id) ON DELETE RESTRICT ON UPDATE CASCADE,
    PRIMARY KEY (id_cotis)
);

-- Groupes d'adhérents pour l'organisation interne de l'association
CREATE TABLE galette_groups (
  id_group integer NOT NULL,
  group_name character varying(250) NOT NULL,
  parent_group integer DEFAULT NULL REFERENCES galette_groups(id_group) ON DELETE RESTRICT ON UPDATE CASCADE,
  PRIMARY KEY (id_group)
);

-- table for groups managers
CREATE TABLE galette_groups_managers (
  id_group integer REFERENCES galette_groups(id_group) ON DELETE RESTRICT ON UPDATE CASCADE,
  id_adh integer REFERENCES galette_adherents (id_adh) ON DELETE RESTRICT ON UPDATE CASCADE,
  PRIMARY KEY (id_group,id_adh)
);

-- table for groups members
CREATE TABLE galette_groups_members (
  id_group integer REFERENCES galette_groups(id_group) ON DELETE RESTRICT ON UPDATE CASCADE,
  id_adh integer REFERENCES galette_adherents (id_adh) ON DELETE RESTRICT ON UPDATE CASCADE,
  PRIMARY KEY (id_group,id_adh)
);

-- Listes de diffusion auxquelles les adhérents peuvent s'inscrire librement.
-- Ceci est destiné à regrouper toutes les listes par matière enseignée.
CREATE TABLE galette_ml_lists (
  id_list integer NOT NULL,
  sympa_name character varying(50) NOT NULL,
  sympa_description text,
  authorized boolean DEFAULT FALSE NOT NULL,
  PRIMARY KEY (id_list)
);

-- Cette table définit les inscriptions des adhérents aux listes de diffusion
-- choisies manuellement. Si un abonné préfère le mode automatique, il n'y a
-- aucune entrée dans la table galette_ml_lists_subscriptions pour la liste. Si en
-- revanche il est inscrit, le choix indiqué dans "is_subscribed" a la priorité
-- sur le choix donné automatiquement par la matière.
CREATE TABLE galette_ml_lists_subscriptions (
  id_adh integer NOT NULL REFERENCES galette_ml_lists(id_list) ON DELETE CASCADE ON UPDATE CASCADE,
  id_list integer NOT NULL REFERENCES galette_adherents(id_adh) ON DELETE CASCADE ON UPDATE CASCADE,
  is_subscribed boolean NOT NULL,
  PRIMARY KEY (id_adh, id_list)
);

-- Cette table définit l'ensemble des listes de diffusion par défaut selon les
-- matières des adhérents.
CREATE TABLE galette_ml_lists_profiles (
  id_list integer NOT NULL REFERENCES galette_ml_lists(id_list) ON DELETE CASCADE ON UPDATE CASCADE,
  id_profile integer NOT NULL,
  authorized boolean DEFAULT FALSE NOT NULL,
  DEFAULT_subscribed boolean DEFAULT TRUE NOT NULL,
  can_toggle_sub boolean DEFAULT TRUE NOT NULL,
  PRIMARY KEY (id_list, id_profile)
);
