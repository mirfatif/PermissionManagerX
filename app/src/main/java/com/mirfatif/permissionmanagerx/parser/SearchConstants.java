package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.util.Utils.getString;

import com.mirfatif.permissionmanagerx.R;

public enum SearchConstants {
  INSTANCE;

  SearchConstants() {
    recreate();
  }

  // Search constants which change with Locale change, so cannot be static and final.
  public String SEARCH_CRITICAL;
  public String SEARCH_FRAMEWORK;
  public String SEARCH_SYSTEM;
  public String SEARCH_USER;
  public String SEARCH_DISABLED;
  public String SEARCH_GREEN;
  public String SEARCH_ORANGE;
  public String SEARCH_RED;

  public String SEARCH_APP_OPS;
  public String SEARCH_UID;
  public String SEARCH_PRIVILEGED;
  public String SEARCH_DEV;
  public String SEARCH_FIXED;
  public String SEARCH_TIME;
  public String SEARCH_EXTRA;

  public String SEARCH_PROT_UNKNOWN;
  public String SEARCH_PROT_NORMAL;
  public String SEARCH_PROT_DANGEROUS;
  public String SEARCH_PROT_SIGNATURE;
  public String SEARCH_PROT_INTERNAL;

  public void recreate() {
    SEARCH_CRITICAL = ":" + getString(R.string.pkg_state_critical);
    SEARCH_FRAMEWORK = ":" + getString(R.string.pkg_state_framework);
    SEARCH_SYSTEM = ":" + getString(R.string.pkg_state_system);
    SEARCH_USER = ":" + getString(R.string.pkg_state_user);
    SEARCH_DISABLED = ":" + getString(R.string.pkg_state_disabled);
    SEARCH_GREEN = ":" + getString(R.string.search_str_green);
    SEARCH_ORANGE = ":" + getString(R.string.search_str_orange);
    SEARCH_RED = ":" + getString(R.string.search_str_red);

    SEARCH_APP_OPS = ":" + getString(R.string.prot_lvl_app_ops);
    SEARCH_UID = ":" + getString(R.string.search_str_uid);
    SEARCH_PRIVILEGED = ":" + getString(R.string.prot_lvl_privileged);
    SEARCH_DEV = ":" + getString(R.string.prot_lvl_development);
    SEARCH_FIXED = ":" + getString(R.string.prot_lvl_fixed);
    SEARCH_TIME = ":" + getString(R.string.search_str_time);
    SEARCH_EXTRA = ":" + getString(R.string.search_str_extra);

    SEARCH_PROT_UNKNOWN = ":" + getString(R.string.prot_lvl_unknown);
    SEARCH_PROT_NORMAL = ":" + getString(R.string.prot_lvl_normal);
    SEARCH_PROT_DANGEROUS = ":" + getString(R.string.prot_lvl_dangerous);
    SEARCH_PROT_SIGNATURE = ":" + getString(R.string.prot_lvl_signature);
    SEARCH_PROT_INTERNAL = ":" + getString(R.string.prot_lvl_internal);
  }
}
