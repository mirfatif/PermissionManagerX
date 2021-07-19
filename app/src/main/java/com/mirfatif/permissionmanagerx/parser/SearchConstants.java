package com.mirfatif.permissionmanagerx.parser;

import static com.mirfatif.permissionmanagerx.util.Utils.getString;

import com.mirfatif.permissionmanagerx.R;

public class SearchConstants {

  public static SearchConstants CONSTANTS = new SearchConstants();

  public static synchronized void recreate() {
    CONSTANTS = new SearchConstants();
  }

  private SearchConstants() {}

  // Search constants which change with Locale change, so cannot be static.
  public final String SEARCH_CRITICAL = ":" + getString(R.string.pkg_state_critical);
  public final String SEARCH_FRAMEWORK = ":" + getString(R.string.pkg_state_framework);
  public final String SEARCH_SYSTEM = ":" + getString(R.string.pkg_state_system);
  public final String SEARCH_USER = ":" + getString(R.string.pkg_state_user);
  public final String SEARCH_DISABLED = ":" + getString(R.string.pkg_state_disabled);
  public final String SEARCH_GREEN = ":" + getString(R.string.search_str_green);
  public final String SEARCH_ORANGE = ":" + getString(R.string.search_str_orange);
  public final String SEARCH_RED = ":" + getString(R.string.search_str_red);

  public final String SEARCH_APP_OPS = ":" + getString(R.string.prot_lvl_app_ops);
  public final String SEARCH_UID = ":" + getString(R.string.search_str_uid);
  public final String SEARCH_PRIVILEGED = ":" + getString(R.string.prot_lvl_privileged);
  public final String SEARCH_DEV = ":" + getString(R.string.prot_lvl_development);
  public final String SEARCH_FIXED = ":" + getString(R.string.prot_lvl_fixed);
  public final String SEARCH_TIME = ":" + getString(R.string.search_str_time);
  public final String SEARCH_EXTRA = ":" + getString(R.string.search_str_extra);
}
