# ==================================================================
# UI Styles Module
# Blue sidebar workspace styling for the application
# ==================================================================

ui_styles <- function() {
    tags$head(
        tags$style(HTML("
      :root {
        /* ---- Blue ramp (refined: deeper, more professional) ---- */
        --blue-50: #eef5ff;
        --blue-100: #d9e8ff;
        --blue-200: #bcd8fb;
        --blue-300: #8ec1f6;
        --blue-400: #5a9ff0;
        --blue-500: #2f7fe0;
        --blue-600: #1f6feb;
        --blue-700: #1a5fc7;
        --blue-800: #154ca1;
        --blue-900: #0b3d8f;

        /* ---- Semantic colors ---- */
        --gc-sidebar-bg: var(--blue-900);
        --gc-sidebar-hover: rgba(255, 255, 255, 0.10);
        --gc-primary: var(--blue-600);
        --gc-primary-hover: var(--blue-700);
        --gc-primary-soft: var(--blue-50);
        --gc-border: var(--blue-200);
        --gc-border-strong: var(--blue-300);
        --gc-bg-light: var(--blue-50);
        --gc-text-dark: #0a1f33;
        --gc-muted: #51687d;
        --gc-panel: rgba(255, 255, 255, 0.97);
        --gc-panel-soft: rgba(255, 255, 255, 0.80);

        /* ---- Type scale ---- */
        --fs-xs: 12px;
        --fs-sm: 13px;
        --fs-base: 14px;
        --fs-md: 15px;
        --fs-lg: 18px;
        --fs-xl: 22px;
        --fs-2xl: 27px;
        --fs-3xl: 30px;

        /* ---- Spacing scale (4px rhythm) ---- */
        --sp-1: 4px;
        --sp-2: 8px;
        --sp-3: 12px;
        --sp-4: 16px;
        --sp-5: 20px;
        --sp-6: 24px;
        --sp-7: 32px;
        --card-padding: var(--sp-5);

        /* ---- Radius ---- */
        --radius-sm: 6px;
        --radius: 8px;
        --radius-lg: 12px;
        --radius-pill: 999px;

        /* ---- Shadows (softened) ---- */
        --shadow-sm: 0 1px 2px rgba(11, 61, 143, 0.05);
        --shadow-md: 0 6px 18px rgba(11, 61, 143, 0.07);
        --shadow-lg: 0 10px 28px rgba(11, 61, 143, 0.10);
      }

      html, body {
        min-height: 100%;
        background: #f5faff;
        color: var(--gc-text-dark);
        font-size: var(--fs-md);
        line-height: 1.5;
        font-family: 'Segoe UI', system-ui, -apple-system, 'Helvetica Neue', Arial, sans-serif;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }

      body {
        padding-top: 0;
      }

      .navbar, .bslib-page-title {
        display: none !important;
      }

      .bslib-sidebar-layout {
        min-height: 100vh;
        background: #f5faff;
      }

      .app-workspace {
        min-height: 100vh;
        padding: var(--sp-6) var(--sp-7) var(--sp-7);
        background:
          radial-gradient(circle at 94% 0%, rgba(187, 216, 251, 0.30), transparent 30rem),
          linear-gradient(180deg, #ffffff 0%, #f5faff 100%);
      }

      .app-main-tabs > .nav,
      .app-main-tabs .nav-tabs {
        display: none !important;
      }

      .app-main-tabs .tab-content {
        padding: 0;
        border: 0;
        background: transparent;
      }

      .app-sidebar {
        background: var(--gc-sidebar-bg) !important;
        color: #ffffff;
        padding: 0 !important;
      }

      .nav-rail-brand {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: var(--sp-2);
        padding: var(--sp-6) var(--sp-5) var(--sp-5);
        border-bottom: 1px solid rgba(255, 255, 255, 0.18);
      }

      .brand-logo {
        width: 100%;
        max-width: 190px;
        height: auto;
        object-fit: contain;
      }

      .brand-tagline {
        width: 190px;
        text-align: center;
        color: rgba(255, 255, 255, 0.82);
        font-size: var(--fs-xs);
        font-weight: 600;
        line-height: 1.35;
        letter-spacing: 0.01em;
      }

      .nav-rail-shell {
        display: flex;
        flex-direction: column;
        min-height: calc(100vh - 200px);
        padding: var(--sp-4) var(--sp-3) var(--sp-5);
      }

      .nav-rail-main {
        flex: 0 0 auto;
      }

      .nav-rail-section {
        padding: var(--sp-2) var(--sp-3) var(--sp-3);
        font-size: var(--fs-xs);
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: rgba(255, 255, 255, 0.62);
      }

      .nav-rail-link {
        position: relative;
        width: 100%;
        min-height: 42px;
        display: flex;
        align-items: center;
        gap: 11px;
        border: 0;
        border-radius: var(--radius);
        background: transparent;
        color: rgba(255, 255, 255, 0.86);
        padding: 10px 12px;
        margin: var(--sp-1) 0;
        font-size: var(--fs-base);
        font-weight: 600;
        text-align: left;
        cursor: pointer;
        transition: background-color 0.16s ease, color 0.16s ease;
      }

      .nav-rail-link:hover {
        background: var(--gc-sidebar-hover);
        color: #ffffff;
      }

      .nav-rail-link.active {
        background: rgba(255, 255, 255, 0.14);
        color: #ffffff;
      }

      .nav-rail-link.active::before {
        content: '';
        position: absolute;
        left: 0;
        top: 8px;
        bottom: 8px;
        width: 3px;
        border-radius: var(--radius-pill);
        background: var(--blue-300);
      }

      .nav-rail-icon {
        width: 20px;
        display: inline-flex;
        justify-content: center;
      }

      .nav-rail-bottom {
        flex: 0 0 auto;
        margin-top: auto;
        padding-top: var(--sp-5);
        border-top: 1px solid rgba(255, 255, 255, 0.16);
      }

      .sidebar-footer {
        padding: var(--sp-3) var(--sp-3) 0;
      }

      .sidebar-credit-line {
        font-size: var(--fs-xs);
        line-height: 1.55;
        color: rgba(255, 255, 255, 0.72);
        display: flex;
        align-items: center;
        gap: 4px;
      }

      .app-workspace-header {
        display: flex;
        justify-content: space-between;
        gap: var(--sp-5);
        align-items: flex-start;
        margin-bottom: var(--sp-5);
        padding: var(--sp-4) var(--sp-5);
        border: 1px solid var(--gc-border);
        border-radius: var(--radius-lg);
        background: var(--gc-panel);
        box-shadow: var(--shadow-md);
      }

      .app-workspace-title {
        margin: 0;
        color: var(--blue-900);
        font-size: var(--fs-2xl);
        font-weight: 800;
        line-height: 1.15;
        letter-spacing: -0.01em;
      }

      .app-workspace-subtitle {
        margin: var(--sp-2) 0 0;
        color: var(--gc-muted);
        font-size: var(--fs-base);
      }

      .app-workspace-actions {
        display: flex;
        align-items: center;
        justify-content: flex-end;
        flex-wrap: wrap;
        gap: var(--sp-2);
      }

      .navbar-status,
      .status-pills {
        display: inline-flex;
        align-items: center;
        gap: var(--sp-2);
      }

      .status-pill {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        min-height: 34px;
        padding: 7px 12px;
        border-radius: var(--radius-pill);
        border: 1px solid var(--gc-border);
        background: var(--gc-primary-soft);
        color: var(--blue-900);
        font-size: var(--fs-sm);
        font-weight: 700;
        white-space: nowrap;
      }

      .status-pill.warning {
        border-color: #f0ad4e;
        background: #fff7e6;
        color: #8a5a00;
      }

      .status-pill.ready {
        border-color: #65b970;
        background: #edf9ee;
        color: #1f7a35;
      }

      .content-card,
      .data-input-card,
      .plot-container,
      .settings-panel {
        box-sizing: border-box;
        width: 100%;
        background: var(--gc-panel);
        border: 1px solid var(--gc-border);
        border-radius: var(--radius-lg);
        box-shadow: var(--shadow-sm);
      }

      .content-card {
        padding: var(--card-padding);
        margin-bottom: var(--sp-5);
      }

      .content-card h3,
      .data-input-card h3,
      .settings-panel h3 {
        margin-top: 0;
        color: var(--blue-900);
        font-size: var(--fs-lg);
        font-weight: 750;
      }

      .welcome-page {
        max-width: 960px;
        margin: 0 auto;
      }

      .welcome-hero {
        display: flex;
        align-items: center;
        gap: var(--sp-6);
        margin-bottom: var(--sp-6);
        padding-bottom: var(--sp-5);
        border-bottom: 1px solid var(--gc-border);
      }

      .welcome-logo {
        width: 124px;
        height: auto;
      }

      .welcome-hero h1 {
        margin: 0;
        color: var(--blue-900);
        font-weight: 800;
        font-size: var(--fs-3xl);
        line-height: 1.15;
        letter-spacing: -0.01em;
      }

      .welcome-hero p {
        margin: var(--sp-2) 0 0;
        color: var(--gc-muted);
        font-size: var(--fs-md);
      }

      .carousel-card {
        position: relative;
        max-width: 800px;
        height: 500px;
        margin: var(--sp-7) auto;
        overflow: hidden;
        background: #ffffff;
        border: 1px solid var(--gc-border);
        border-radius: var(--radius-lg);
        box-shadow: var(--shadow-md);
      }

      .carousel-images,
      .carousel-slide {
        width: 100%;
        height: 100%;
      }

      .carousel-slide {
        position: absolute;
        inset: 0;
        object-fit: contain;
        opacity: 0;
        transition: opacity 0.6s ease;
      }

      .carousel-slide.active {
        opacity: 1;
      }

      .slide-indicators {
        position: absolute;
        bottom: 14px;
        left: 50%;
        transform: translateX(-50%);
        display: flex;
        gap: 8px;
      }

      .indicator {
        width: 12px;
        height: 12px;
        border-radius: 999px;
        border: 2px solid var(--blue-700);
        background: rgba(255, 255, 255, 0.78);
        padding: 0;
      }

      .indicator.active {
        background: var(--blue-600);
        border-color: var(--blue-600);
      }

      .data-input-grid {
        display: grid;
        grid-template-columns: minmax(320px, 0.92fr) minmax(520px, 1.55fr);
        gap: var(--sp-5);
        align-items: start;
      }

      .data-input-left-column {
        display: flex;
        flex-direction: column;
        gap: var(--sp-5);
      }

      .data-input-card {
        padding: var(--sp-5);
      }

      .data-input-dynamic-card {
        grid-row: auto;
        min-height: 640px;
      }

      .data-input-action-card {
        border-color: var(--gc-border-strong);
        background: linear-gradient(180deg, #ffffff 0%, var(--gc-primary-soft) 100%);
        align-self: start;
      }

      .data-input-card-header,
      .settings-panel-header {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: var(--sp-3);
        margin-bottom: var(--sp-4);
      }

      .data-input-card-header h3,
      .settings-panel-header h3 {
        margin: 0;
        font-size: var(--fs-lg);
        line-height: 1.25;
      }

      .data-input-card-header > .fa,
      .data-input-card-header > .fas,
      .data-input-card-header > .svg-inline--fa {
        color: var(--gc-primary);
        flex: 0 0 auto;
        width: 22px;
      }

      .input-grid-two {
        display: grid;
        grid-template-columns: repeat(2, minmax(180px, 1fr));
        gap: var(--sp-3) var(--sp-4);
      }

      .input-method-help {
        padding: var(--sp-3) var(--sp-4);
        margin-top: var(--sp-3);
        border-radius: var(--radius);
        background: var(--gc-primary-soft);
        border: 1px solid var(--gc-border);
        color: #36556f;
        font-size: var(--fs-xs);
        line-height: 1.5;
      }

      .input-method-help p {
        margin: 0 0 var(--sp-2);
      }

      .input-method-help p:last-child {
        margin-bottom: 0;
      }

      .generate-action {
        width: 100%;
        min-height: 48px;
        font-size: var(--fs-md);
        font-weight: 800;
        border-radius: var(--radius);
      }

      .plot-container {
        min-height: 680px;
        padding: var(--sp-4);
      }

      .settings-panel {
        position: sticky;
        top: var(--sp-5);
        padding: var(--sp-4);
        max-height: calc(100vh - 36px);
        overflow-y: auto;
        background: #f8fbff;
        border: 1px solid var(--gc-border);
        border-radius: var(--radius);
        box-shadow: none;
      }

      /* ---- Collapsible settings header (chevron toggle injected by JS) ---- */
      .settings-panel-header {
        display: flex;
        align-items: center;
        gap: var(--sp-2);
        width: 100%;
        padding: 2px 2px var(--sp-3);
        margin-bottom: var(--sp-3);
        border-bottom: 1px solid rgba(142, 193, 246, 0.55);
      }

      .settings-panel-header h3 {
        flex: 1 1 auto;
        margin: 0;
        color: var(--blue-900);
        font-size: var(--fs-lg);
        font-weight: 800;
        line-height: 1.25;
      }

      .settings-panel-header .fa,
      .settings-panel-header .fas,
      .settings-panel-header .svg-inline--fa {
        color: var(--gc-primary);
      }

      .settings-collapse-toggle {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 auto;
        width: 32px;
        height: 32px;
        padding: 0;
        border: 1px solid var(--gc-border-strong);
        border-radius: var(--radius);
        background: #ffffff;
        color: var(--gc-primary);
        font-size: var(--fs-sm);
        cursor: pointer;
        transition: background-color 0.16s ease, color 0.16s ease, transform 0.16s ease;
      }

      .settings-collapse-toggle:hover,
      .settings-collapse-toggle:focus {
        background: var(--gc-primary-soft);
        color: var(--blue-900);
      }

      .settings-panel.settings-panel-collapsed .settings-collapse-toggle .fa,
      .settings-panel.settings-panel-collapsed .settings-collapse-toggle .fas {
        transform: rotate(180deg);
      }

      .settings-panel.settings-panel-collapsed .settings-collapse-controls {
        display: none !important;
      }

      .settings-section {
        padding: var(--sp-3);
        margin: var(--sp-3) 0;
        border: 1px solid rgba(142, 193, 246, 0.44);
        border-radius: var(--radius);
        background: rgba(255, 255, 255, 0.82);
      }

      .settings-section-title {
        display: flex;
        align-items: center;
        gap: var(--sp-2);
        margin-bottom: var(--sp-3);
        color: var(--blue-900);
        font-size: var(--fs-xs);
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      .settings-section-title .fa,
      .settings-section-title .fas,
      .settings-section-title .svg-inline--fa {
        color: var(--gc-primary);
      }

      .settings-note {
        margin-top: var(--sp-2);
        padding: var(--sp-2) var(--sp-3);
        border-radius: var(--radius);
        background: var(--gc-primary-soft);
        color: var(--gc-muted);
        font-size: var(--fs-xs);
        line-height: 1.4;
      }

      .settings-panel .form-group,
      .settings-panel .shiny-input-container {
        width: 100%;
        margin-bottom: var(--sp-3);
      }

      .settings-panel label,
      .settings-panel .control-label {
        color: #244761;
        font-size: var(--fs-sm);
        font-weight: 700;
      }

      .download-group {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
        gap: var(--sp-2);
        margin-bottom: var(--sp-3);
        width: 100%;
      }

      .download-group .btn,
      .settings-panel .btn,
      .settings-panel .btn-primary,
      .settings-panel .btn-success {
        width: 100%;
      }

      .gene-list-actions {
        display: flex;
        align-items: flex-end;
        justify-content: flex-end;
        min-height: 82px;
      }

      .pathway-results-card {
        min-height: 640px;
      }

      .pathway-options-panel {
        top: var(--sp-5);
      }

      .pathway-settings-panel {
        top: var(--sp-5);
      }

      .settings-collapse-downloads {
        margin-top: var(--sp-3);
      }

      .selectize-dropdown {
        z-index: 10000;
      }

      .well {
        background-color: #f8fbff;
        border: 1px solid var(--gc-border);
        border-radius: var(--radius);
        box-shadow: none;
        margin-bottom: var(--sp-4);
        padding: var(--sp-4);
      }

      .well h5 {
        display: flex;
        align-items: center;
        gap: var(--sp-2);
        margin: 0 0 var(--sp-3);
        color: var(--blue-900);
        font-size: var(--fs-lg);
        font-weight: 800;
        line-height: 1.25;
      }

      .well h5 .fa,
      .well h5 .fas,
      .well h5 .svg-inline--fa {
        color: var(--blue-700);
      }

      .btn-primary,
      .btn.btn-primary {
        background-color: var(--gc-primary);
        border-color: var(--gc-primary);
        color: #ffffff;
        font-weight: 700;
        border-radius: var(--radius);
      }

      .btn-primary:hover,
      .btn.btn-primary:hover,
      .btn-primary:focus,
      .btn.btn-primary:focus {
        background-color: var(--gc-primary-hover);
        border-color: var(--gc-primary-hover);
        color: #ffffff;
      }

      .btn-info,
      .btn.btn-info {
        background-color: var(--blue-600);
        border-color: var(--blue-600);
        color: #ffffff;
      }

      .btn-success,
      .btn.btn-success {
        background-color: #2ca25f;
        border-color: #2ca25f;
        color: #ffffff;
        font-weight: 700;
        border-radius: var(--radius);
      }

      .form-control:focus,
      .form-select:focus,
      .selectize-input.focus {
        border-color: var(--gc-primary);
        box-shadow: 0 0 0 3px rgba(31, 111, 235, 0.15);
      }

      .form-control,
      .form-select,
      .selectize-input {
        border-radius: var(--radius);
        border-color: var(--gc-border-strong);
        background-color: #fbfdff;
        font-size: var(--fs-base);
      }

      .shiny-input-container {
        margin-bottom: var(--sp-4);
      }

      .control-label,
      .shiny-input-container > label {
        color: #173c5a;
        font-size: var(--fs-base);
        font-weight: 650;
        margin-bottom: var(--sp-2);
      }

      input[type='radio'],
      input[type='checkbox'] {
        accent-color: var(--gc-primary);
      }

      .radio,
      .checkbox {
        margin-top: var(--sp-2);
        margin-bottom: var(--sp-2);
      }

      .radio label,
      .checkbox label {
        line-height: 1.4;
        font-size: var(--fs-base);
      }

      .input-group .form-control {
        min-height: 40px;
      }

      .input-group .btn,
      .btn-file {
        border-radius: var(--radius) 0 0 var(--radius) !important;
        border-color: var(--gc-border-strong);
        background: var(--gc-primary-soft);
        color: var(--blue-700);
        font-weight: 700;
      }

      .input-group .btn:hover,
      .btn-file:hover {
        background: #dff0ff;
        color: var(--blue-900);
      }

      .irs--shiny .irs-bar,
      .irs--shiny .irs-single {
        background: var(--gc-primary);
        border-color: var(--gc-primary);
      }

      .irs--shiny .irs-handle {
        border-color: var(--gc-primary);
      }

      .alert-warning {
        background-color: #fff7e6;
        border-color: #e8a33d;
        color: #7a5200;
        padding: var(--sp-3);
        border-radius: var(--radius);
        margin-top: var(--sp-3);
      }

      h4 {
        color: var(--blue-900);
        font-size: var(--fs-xl);
        font-weight: 800;
        margin: var(--sp-6) 0 var(--sp-2);
        line-height: 1.25;
      }

      hr {
        border: 0;
        border-top: 1px solid var(--gc-border);
        margin: var(--sp-4) 0;
        opacity: 1;
      }

      h5 {
        color: #244761;
        font-size: var(--fs-lg);
        font-weight: 700;
        line-height: 1.25;
      }

      footer {
        display: none;
      }

      .shiny-notification {
        position: fixed;
        top: var(--sp-6);
        right: var(--sp-6);
        width: 400px;
      }

      /* ---- Pathway tab panels & action buttons (class-based, replaces inline styles) ---- */
      .panel-pad {
        padding: var(--sp-4);
      }

      .panel-pad-sm {
        padding: var(--sp-3);
      }

      .pathway-settings-toggle {
        width: 100%;
        font-size: var(--fs-base);
        font-weight: 700;
        padding: var(--sp-2) var(--sp-3);
        margin-bottom: var(--sp-3);
      }

      .section-header {
        display: flex;
        align-items: center;
        gap: var(--sp-2);
        margin: 0 0 var(--sp-3);
        color: var(--blue-900);
        font-size: var(--fs-xs);
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      .section-header .fa,
      .section-header .fas,
      .section-header .svg-inline--fa {
        color: var(--blue-700);
      }

      .run-action {
        width: 100%;
        font-size: var(--fs-md);
        font-weight: 800;
        padding: var(--sp-3);
        border-radius: var(--radius);
      }

      .pathway-result-toolbar {
        margin-top: var(--sp-6);
        display: flex;
        flex-wrap: wrap;
        gap: var(--sp-2);
        align-items: center;
      }

      .section-heading {
        margin-top: 0;
        margin-bottom: var(--sp-4);
        color: var(--blue-900);
        font-size: var(--fs-lg);
        font-weight: 800;
        line-height: 1.25;
      }

      .panel-h4 {
        margin-top: 0;
        margin-bottom: var(--sp-4);
      }

      .subhead {
        display: flex;
        align-items: center;
        gap: var(--sp-2);
        margin: var(--sp-4) 0 var(--sp-3);
        color: var(--blue-900);
        font-size: var(--fs-base);
        font-weight: 800;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      .subhead .fa,
      .subhead .fas,
      .subhead .svg-inline--fa {
        color: var(--blue-700);
      }

      .btn-sm-block {
        width: 100%;
        margin-bottom: var(--sp-2);
      }

      @media (max-width: 1100px) {
        .data-input-grid {
          grid-template-columns: 1fr;
        }

        .data-input-dynamic-card {
          grid-row: auto;
        }
      }

      @media (max-width: 800px) {
        .app-workspace {
          padding: var(--sp-5);
        }

        .app-workspace-header,
        .welcome-hero {
          flex-direction: column;
          align-items: flex-start;
        }

        .input-grid-two {
          grid-template-columns: 1fr;
        }

        .carousel-card {
          height: 320px;
        }
      }
    ")),
        tags$script(HTML("
      Shiny.addCustomMessageHandler('setActiveNav', function(message) {
        const value = message && message.value ? message.value : 'welcome';
        document.querySelectorAll('[data-nav-value]').forEach(function(button) {
          button.classList.toggle('active', button.getAttribute('data-nav-value') === value);
        });
      });

      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('[data-nav-value=\"welcome\"]').forEach(function(button) {
          button.classList.add('active');
        });
      });
    ")),
        tags$script(HTML("
      /* ==================================================================
         Collapsible settings panels - inject a chevron toggle that hides the
         controls region while keeping downloads/action buttons visible.

         Two strategies, picked per panel:
           A. Explicit markers -> panel contains .settings-collapse-controls
              (collapses) and .settings-collapse-downloads (stays visible).
              Pure CSS handles the hide; no JS marking needed.
           B. Heuristic        -> flat panels (no explicit markers) get every
              child between the header and a download/Run marker tagged with
              .settings-hide-when-collapsed (hidden on collapse).

         The toggle attaches whenever the panel carries an action region
         (checked by DOM existence, so it survives being nested inside Shiny
         conditionalPanel()s whose content is not yet laid out).
         ================================================================== */
      (function() {
        function isAlwaysVisibleMarker(element) {
          if (!element || !element.classList) return false;
          var isLabel = element.classList.contains('section-header') || element.tagName === 'H6';
          if (!isLabel) return false;
          var text = (element.textContent || '').replace(/\\s+/g, ' ').trim().toLowerCase();
          return text.indexOf('download') === 0 || text.indexOf('run') === 0;
        }

        function isDisplayed(element) {
          if (!element) return false;
          var node = element;
          while (node && node !== document.body && node !== document.documentElement) {
            if (window.getComputedStyle(node).display === 'none') return false;
            node = node.parentElement;
          }
          return true;
        }

        function hasActionRegion(panel) {
          // Explicit Strategy-A controls marker always enables the chevron,
          // even when the panel has no download/run region to keep visible.
          if (panel.querySelector('.settings-collapse-controls')) return true;
          if (panel.querySelector('.settings-collapse-downloads')) return true;
          var candidates = panel.querySelectorAll('.section-header, h6');
          for (var i = 0; i < candidates.length; i++) {
            if (isAlwaysVisibleMarker(candidates[i])) return true;
          }
          return false;
        }

        function hasExplicitMarkers(panel) {
          return !!panel.querySelector('.settings-collapse-controls');
        }

        function clearCollapseMarks(panel) {
          panel.querySelectorAll('.settings-hide-when-collapsed').forEach(function(element) {
            element.classList.remove('settings-hide-when-collapsed');
          });
        }

        function markHeuristicRegion(container, skipFirstChild) {
          var children = Array.prototype.slice.call(container.children || []);
          var start = skipFirstChild ? 1 : 0;
          var reachedAction = false;

          for (var i = start; i < children.length; i++) {
            var child = children[i];
            if (reachedAction) break;
            if (!isDisplayed(child)) continue;

            if (isAlwaysVisibleMarker(child)) {
              reachedAction = true;
              break;
            }
            if (child.querySelector && child.querySelector('.section-header, h6')) {
              var nested = markHeuristicRegion(child, false);
              if (nested) {
                reachedAction = true;
                break;
              }
            }
            child.classList.add('settings-hide-when-collapsed');
          }
          return reachedAction;
        }

        function applyCollapsedState(panel) {
          clearCollapseMarks(panel);
          if (!panel.classList.contains('settings-panel-collapsed')) return;
          if (hasExplicitMarkers(panel)) return;
          markHeuristicRegion(panel, true);
        }

        function attachToggle(panel, header) {
          panel.dataset.collapseEnhanced = 'true';
          panel.classList.add('settings-panel--collapsible');

          var button = document.createElement('button');
          button.type = 'button';
          button.className = 'settings-collapse-toggle';
          button.setAttribute('aria-label', 'Hide settings controls');
          button.setAttribute('aria-expanded', 'true');
          button.innerHTML = '<i class=\"fas fa-chevron-down\" aria-hidden=\"true\"></i>';

          button.addEventListener('click', function() {
            var collapsed = !panel.classList.contains('settings-panel-collapsed');
            panel.classList.toggle('settings-panel-collapsed', collapsed);
            button.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
            button.setAttribute('aria-label', collapsed ? 'Show settings controls' : 'Hide settings controls');
            applyCollapsedState(panel);
          });

          header.appendChild(button);
        }

        function enhanceSettingsPanels() {
          document.querySelectorAll('.settings-panel').forEach(function(panel) {
            var header = panel.children && panel.children.length ? panel.children[0] : null;
            if (!header) return;
            if (!hasActionRegion(panel)) return;

            if (panel.dataset.collapseEnhanced === 'true') {
              applyCollapsedState(panel);
              return;
            }
            if (header.querySelector('.settings-collapse-toggle')) {
              panel.dataset.collapseEnhanced = 'true';
              applyCollapsedState(panel);
              return;
            }

            attachToggle(panel, header);
            applyCollapsedState(panel);
          });
        }

        function refreshEnhancedPanels() {
          document.querySelectorAll('.settings-panel[data-collapse-enhanced=\"true\"]').forEach(applyCollapsedState);
          enhanceSettingsPanels();
        }

        document.addEventListener('DOMContentLoaded', enhanceSettingsPanels);
        document.addEventListener('shiny:value', function() {
          setTimeout(refreshEnhancedPanels, 0);
        });
        document.addEventListener('shiny:inputchanged', function() {
          setTimeout(refreshEnhancedPanels, 0);
        });

        if (window.MutationObserver) {
          var pending = null;
          new MutationObserver(function() {
            if (pending) return;
            pending = setTimeout(function() {
              pending = null;
              refreshEnhancedPanels();
            }, 120);
          }).observe(document.documentElement, {
            attributes: true,
            attributeFilter: ['style', 'class'],
            childList: true,
            subtree: true
          });
        }
      })();
    "))
    )
}
