// AI Workshop Web UI - app.js - Version: 1
// Vanilla JS, no framework — WebSocket streaming, session management, SSH browser
'use strict';

// Expose functions needed by inline onclick handlers
// (defined later in file, assigned to window after DOMContentLoaded)
window.copyCode        = null;
window.removeAttachment = null;

// ── State ─────────────────────────────────────────────────────
const state = {
  sessions:        [],
  currentSession:  null,
  ws:              null,
  generating:      false,
  attachments:     [],   // [{name, type, content, mime, source, error}]
  sshPath:         '/home',
  sshSelected:     null,
  config:          {},
  wsReconnectTimer: null,
};

// ── DOM helpers ───────────────────────────────────────────────
const $  = id  => document.getElementById(id);
const $$ = sel => document.querySelectorAll(sel);

// ── Init ──────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', async () => {
  await loadTheme();
  await loadConfig();
  await loadModels();
  await loadSessions();
  checkOllamaStatus();
  setInterval(checkOllamaStatus, 15000);
  bindEvents();
  connectWS();

  // Register inline-callable globals after functions are defined
  window.copyCode         = copyCode;
  window.removeAttachment = removeAttachment;
});

// ── Theme ─────────────────────────────────────────────────────
async function loadTheme() {
  try {
    const r = await fetch('/api/theme');
    const { colors } = await r.json();
    applyThemeColors(colors);
    renderThemeSwatches(colors);
  } catch (e) {
    console.warn('Theme load failed, using CSS defaults');
  }
}

function applyThemeColors(colors) {
  const root = document.documentElement;
  const map = {
    bg_primary:           '--bg-primary',
    bg_secondary:         '--bg-secondary',
    bg_tertiary:          '--bg-tertiary',
    panel_bg:             '--panel-bg',
    text_primary:         '--text-primary',
    text_secondary:       '--text-secondary',
    text_accent:          '--text-accent',
    accent_primary:       '--accent-primary',
    accent_secondary:     '--accent-secondary',
    border:               '--border',
    button_normal:        '--button-normal',
    button_hover:         '--button-hover',
    selection_background: '--selection-bg',
    selection_text:       '--selection-text',
    success:              '--success',
    warning:              '--warning',
    error:                '--error',
  };
  for (const [key, cssVar] of Object.entries(map)) {
    if (colors[key]) root.style.setProperty(cssVar, colors[key]);
  }

  // Derive bubble colours from theme by tinting bg_secondary
  const bgSecondary = colors.bg_secondary || '#252525';
  const accent      = colors.accent_primary || '#1976d2';
  const success     = colors.success        || '#4caf50';
  const error       = colors.error          || '#f44336';

  root.style.setProperty('--bubble-user-bg', tintColor(bgSecondary, accent,  0.25));
  root.style.setProperty('--bubble-user-fg', colors.text_primary || '#e0e0e0');
  root.style.setProperty('--bubble-ai-bg',   tintColor(bgSecondary, success, 0.20));
  root.style.setProperty('--bubble-ai-fg',   colors.text_primary || '#e0e0e0');
  root.style.setProperty('--bubble-err-bg',  tintColor(bgSecondary, error,   0.22));
  root.style.setProperty('--bubble-err-fg',  colors.text_primary || '#e0e0e0');
}

function tintColor(base, tint, factor) {
  // Mix tint colour into base at factor strength, return hex
  try {
    const parse = hex => {
      const h = hex.replace('#','');
      return [parseInt(h.substr(0,2),16), parseInt(h.substr(2,2),16), parseInt(h.substr(4,2),16)];
    };
    const toHex = n => n.toString(16).padStart(2,'0');
    const [br,bg,bb] = parse(base);
    const [tr,tg,tb] = parse(tint);
    const r = Math.round(br*(1-factor) + tr*factor);
    const g = Math.round(bg*(1-factor) + tg*factor);
    const b = Math.round(bb*(1-factor) + tb*factor);
    return '#' + toHex(r) + toHex(g) + toHex(b);
  } catch(e) { return base; }
}

function renderThemeSwatches(colors) {
  const preview = $('theme-preview');
  if (!preview) return;
  preview.innerHTML = '';
  const show = ['bg_primary','bg_secondary','accent_primary','text_primary',
                 'success','warning','error','border'];
  for (const key of show) {
    if (!colors[key]) continue;
    const d = document.createElement('div');
    d.className = 'theme-swatch';
    d.style.background = colors[key];
    d.style.color = isLight(colors[key]) ? '#000' : '#fff';
    d.textContent = key.replace(/_/g, ' ');
    d.title = `${key}: ${colors[key]}`;
    preview.appendChild(d);
  }
}

function isLight(hex) {
  const c = hex.replace('#', '');
  const r = parseInt(c.substr(0,2), 16);
  const g = parseInt(c.substr(2,2), 16);
  const b = parseInt(c.substr(4,2), 16);
  return (r*299 + g*587 + b*114) / 1000 > 128;
}

// ── Config ────────────────────────────────────────────────────
async function loadConfig() {
  try {
    const r = await fetch('/api/config');
    state.config = await r.json();
    const c = state.config;
    if ($('cfg-sessions-dir')) $('cfg-sessions-dir').value = c.sessions_dir || '';
    if ($('cfg-port'))         $('cfg-port').value         = c.port || 8080;
    if ($('cfg-ollama-url'))   $('cfg-ollama-url').value   = c.ollama_url || '';
    if ($('ollama-url'))       $('ollama-url').value       = c.ollama_url || 'http://localhost:11434';
    const ssh = c.ssh || {};
    setVal('cfg-ssh-host', ssh.host || '127.0.0.1');
    setVal('cfg-ssh-port', ssh.port || 22);
    setVal('cfg-ssh-user', ssh.username || '');
    setVal('cfg-ssh-key',  ssh.key_path || '');
    setVal('cfg-ssh-root', ssh.root_path || '/home');
    state.sshPath = ssh.root_path || '/home';
  } catch(e) { console.warn('Config load failed:', e); }
}

function setVal(id, val) {
  const el = $(id);
  if (el) el.value = val;
}

async function saveConfig() {
  const cfg = {
    sessions_dir: $('cfg-sessions-dir').value.trim(),
    port:         parseInt($('cfg-port').value) || 8080,
    ollama_url:   $('cfg-ollama-url').value.trim(),
    ssh: {
      host:      $('cfg-ssh-host').value.trim(),
      port:      parseInt($('cfg-ssh-port').value) || 22,
      username:  $('cfg-ssh-user').value.trim(),
      password:  $('cfg-ssh-pass').value,
      key_path:  $('cfg-ssh-key').value.trim(),
      root_path: $('cfg-ssh-root').value.trim(),
    }
  };
  await fetch('/api/config', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(cfg)
  });
  state.config = cfg;
  closeModal('modal-settings');
  checkOllamaStatus();
}

// ── Models ────────────────────────────────────────────────────
async function loadModels() {
  try {
    const r = await fetch('/api/models');
    const { models } = await r.json();
    const sel = $('model-select');
    sel.innerHTML = models.length
      ? models.map(m => `<option value="${m}">${m}</option>`).join('')
      : '<option value="">No models found</option>';
  } catch(e) {
    $('model-select').innerHTML = '<option value="">Error loading models</option>';
  }
}

// ── Ollama status ─────────────────────────────────────────────
async function checkOllamaStatus() {
  try {
    const r = await fetch('/api/ollama/status');
    const { running } = await r.json();
    const dot  = $('ollama-dot');
    const text = $('ollama-status-text');
    dot.className = 'status-dot ' + (running ? 'online' : 'offline');
    dot.title     = running ? 'Ollama running' : 'Ollama offline';
    if (text) {
      text.textContent = running ? '● Ollama running' : '● Ollama offline — run: ollama serve';
      text.style.color = running
        ? getComputedStyle(document.documentElement).getPropertyValue('--success')
        : getComputedStyle(document.documentElement).getPropertyValue('--error');
    }
  } catch(e) {
    const dot = $('ollama-dot');
    if (dot) { dot.className = 'status-dot offline'; dot.title = 'Cannot reach server'; }
  }
}

// ── WebSocket ─────────────────────────────────────────────────
function connectWS() {
  const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
  const wsUrl = `${proto}//${location.host}/ws/chat`;

  if (state.ws) {
    try { state.ws.close(); } catch(e) {}
  }

  state.ws = new WebSocket(wsUrl);

  state.ws.onopen = () => {
    console.log('WS connected');
    if (state.wsReconnectTimer) {
      clearTimeout(state.wsReconnectTimer);
      state.wsReconnectTimer = null;
    }
  };

  state.ws.onmessage = (evt) => {
    const data = JSON.parse(evt.data);
    if (data.error) {
      appendBubble('error', data.error);
      setGenerating(false);
      return;
    }
    if (data.token) {
      state._streamBuffer = (state._streamBuffer || '') + data.token;
      updateLastBubble(state._streamBuffer);
    }
    if (data.done) {
      // Save assistant message to current session locally
      if (state.currentSession) {
        state.currentSession.messages.push({
          role: 'assistant',
          content: data.full || state._streamBuffer || ''
        });
        state._streamBuffer = '';
        refreshSessionList();
      }
      setGenerating(false);
    }
  };

  state.ws.onerror = () => setGenerating(false);

  state.ws.onclose = () => {
    // Auto-reconnect after 3s
    state.wsReconnectTimer = setTimeout(connectWS, 3000);
  };
}

// ── Sessions ──────────────────────────────────────────────────
async function loadSessions() {
  try {
    const r = await fetch('/api/sessions');
    const { sessions } = await r.json();
    state.sessions = sessions || [];
    if (state.sessions.length === 0) {
      await newSession();
    } else {
      renderSessionList();
      selectSession(state.sessions[0]);
    }
  } catch(e) {
    console.warn('Sessions load failed:', e);
    await newSession();
  }
}

async function newSession(name = '') {
  try {
    const r = await fetch('/api/sessions', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name })
    });
    const session = await r.json();
    state.sessions.unshift(session);
    renderSessionList();
    selectSession(session);
    return session;
  } catch(e) {
    console.warn('New session failed:', e);
  }
}

function selectSession(session) {
  state.currentSession = session;
  renderSessionList();
  renderChat();
}

function renderSessionList(filter = '') {
  const list = $('session-list');
  if (!list) return;
  const q = filter.toLowerCase();
  const visible = q
    ? state.sessions.filter(s =>
        s.name.toLowerCase().includes(q) ||
        (s.messages || []).some(m => m.content.toLowerCase().includes(q)))
    : state.sessions;

  list.innerHTML = visible.map(s => {
    const active  = state.currentSession && s.id === state.currentSession.id ? 'active' : '';
    const pin     = s.pinned ? 'pinned' : '';
    const count   = (s.messages || []).length;
    const name    = escHtml(s.name);
    return `<li class="${active} ${pin}" data-id="${s.id}" title="${name}">
      ${name}<span class="session-count">${count}</span>
    </li>`;
  }).join('');

  list.querySelectorAll('li').forEach(li => {
    li.addEventListener('click', () => {
      const s = state.sessions.find(x => x.id === li.dataset.id);
      if (s) selectSession(s);
    });
    li.addEventListener('contextmenu', e => {
      e.preventDefault();
      showSessionMenu(e, li.dataset.id);
    });
  });
}

function refreshSessionList() {
  renderSessionList($('session-search')?.value || '');
}

// ── Session context menu ──────────────────────────────────────
function showSessionMenu(e, sessionId) {
  document.querySelector('.ctx-menu')?.remove();

  const session = state.sessions.find(s => s.id === sessionId);
  if (!session) return;

  const menu = document.createElement('div');
  menu.className = 'ctx-menu';
  menu.style.cssText = `position:fixed;left:${e.clientX}px;top:${e.clientY}px;
    background:var(--bg-secondary);border:1px solid var(--border);border-radius:5px;
    padding:4px 0;z-index:2000;min-width:160px;box-shadow:0 4px 16px rgba(0,0,0,0.4);`;

  const items = [
    { label: 'Rename',          action: () => renameSession(sessionId) },
    { label: session.pinned ? 'Unpin ⭐' : 'Pin ⭐', action: () => togglePin(sessionId) },
    { label: '─', separator: true },
    { label: 'Export as .md',   action: () => exportSession(sessionId, 'md') },
    { label: 'Export as .txt',  action: () => exportSession(sessionId, 'txt') },
    { label: '─', separator: true },
    { label: 'Delete',          action: () => deleteSession(sessionId), danger: true },
  ];

  items.forEach(item => {
    if (item.separator) {
      const hr = document.createElement('div');
      hr.style.cssText = 'border-top:1px solid var(--border);margin:3px 0;';
      menu.appendChild(hr);
      return;
    }
    const btn = document.createElement('div');
    btn.textContent = item.label;
    btn.style.cssText = `padding:7px 14px;cursor:pointer;font-size:13px;
      color:${item.danger ? 'var(--error)' : 'var(--text-primary)'};`;
    btn.addEventListener('mouseenter', () => btn.style.background = 'var(--bg-tertiary)');
    btn.addEventListener('mouseleave', () => btn.style.background = '');
    btn.addEventListener('click', () => { menu.remove(); item.action(); });
    menu.appendChild(btn);
  });

  document.body.appendChild(menu);
  setTimeout(() => document.addEventListener('click', () => menu.remove(), { once: true }), 0);
}

async function renameSession(id) {
  const session = state.sessions.find(s => s.id === id);
  if (!session) return;
  const name = prompt('Session name:', session.name);
  if (!name || !name.trim()) return;
  session.name = name.trim();
  await fetch(`/api/sessions/${id}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name: session.name })
  });
  refreshSessionList();
}

async function togglePin(id) {
  const session = state.sessions.find(s => s.id === id);
  if (!session) return;
  session.pinned = !session.pinned;
  await fetch(`/api/sessions/${id}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ pinned: session.pinned })
  });
  // Re-sort: pinned first
  state.sessions.sort((a, b) => (b.pinned ? 1 : 0) - (a.pinned ? 1 : 0));
  refreshSessionList();
}

async function deleteSession(id) {
  if (state.sessions.length <= 1) {
    alert('Cannot delete the only session.');
    return;
  }
  if (!confirm('Delete this session?')) return;
  await fetch(`/api/sessions/${id}`, { method: 'DELETE' });
  state.sessions = state.sessions.filter(s => s.id !== id);
  if (state.currentSession?.id === id) {
    selectSession(state.sessions[0]);
  }
  refreshSessionList();
}

async function clearCurrentSession() {
  if (!state.currentSession) return;
  if (!confirm('Clear all messages in this session?')) return;
  state.currentSession.messages = [];
  await fetch(`/api/sessions/${state.currentSession.id}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages: [] })
  });
  renderChat();
  refreshSessionList();
}

function exportSession(id, fmt) {
  window.open(`/api/sessions/${id}/export?fmt=${fmt}`, '_blank');
}

// ── Chat rendering ────────────────────────────────────────────
function renderChat() {
  const container = $('chat-messages');
  if (!container) return;
  container.innerHTML = '';
  if (!state.currentSession) return;
  for (const msg of (state.currentSession.messages || [])) {
    appendBubble(msg.role, msg.content);
  }
}

function appendBubble(role, content) {
  const container = $('chat-messages');
  if (!container) return;

  const div = document.createElement('div');
  div.className = `bubble bubble-${role}`;
  div.dataset.role = role;

  const labels = { user: 'You', assistant: 'AI', error: 'Error' };
  const label = labels[role] || role;

  div.innerHTML = `<div class="bubble-role">${escHtml(label)}</div>`
                + formatContent(content);

  container.appendChild(div);
  container.scrollTop = container.scrollHeight;
  return div;
}

function updateLastBubble(content) {
  const container = $('chat-messages');
  if (!container) return;
  const bubbles = container.querySelectorAll('.bubble-assistant');
  if (bubbles.length === 0) {
    appendBubble('assistant', content);
    return;
  }
  const last = bubbles[bubbles.length - 1];
  last.innerHTML = `<div class="bubble-role">AI</div>` + formatContent(content);
  container.scrollTop = container.scrollHeight;
}

function formatContent(text) {
  // Strip leading line numbers from copy-pasted code (e.g. "  1\t" or "123: ")
  // These appear when copying from editors with line numbers visible
  const stripped = text.replace(/^[ \t]*\d+[\t: ]+/gm, (match, offset, str) => {
    // Only strip if it looks like a block of numbered lines (3+ consecutive)
    return match;
  });

  let html = escHtml(text);

  // Fenced code blocks ```lang\n...\n```
  html = html.replace(/```(\w*)\n?([\s\S]*?)```/g, (_, lang, code) => {
    // Strip leading line numbers from pasted code blocks
    const cleanCode = stripLineNumbers(code);
    const langLabel = lang
      ? `<span class="code-lang">${escHtml(lang)}</span>`
      : '';
    const codeId = 'code-' + Math.random().toString(36).substr(2,6);
    const numbered = addLineNumbers(cleanCode);
    return `<div class="code-block line-numbers" id="${codeId}">
      <div class="code-block-header">
        ${langLabel || '<span class="code-lang">code</span>'}
        <button class="code-copy-btn" onclick="copyCode('${codeId}')">Copy</button>
      </div>
      <pre><code>${numbered}</code></pre>
    </div>`;
  });

  // Inline code `...`
  html = html.replace(/`([^`\n]+)`/g, '<code>$1</code>');

  // Bold **...**
  html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');

  // Newlines → <br> but not inside pre blocks (already handled above)
  html = html.replace(/\n/g, '<br>');

  return html;
}

function stripLineNumbers(code) {
  // Detect if most lines start with a number — if so, strip them
  const lines = code.split('\n');
  if (lines.length < 2) return code;
  const numberedLines = lines.filter(l => /^\s*\d+[\t: |]+/.test(l));
  if (numberedLines.length / lines.length > 0.6) {
    return lines.map(l => l.replace(/^\s*\d+[\t: |]+/, '')).join('\n');
  }
  return code;
}

function addLineNumbers(code) {
  // Wrap each line in a <span class="line"> for CSS counter
  const lines = code.trimEnd().split('\n');
  return lines.map(line => `<span class="line">${line || ' '}</span>`).join('\n');
}

function copyCode(blockId) {
  const block = document.getElementById(blockId);
  if (!block) return;
  const pre = block.querySelector('pre');
  if (!pre) return;
  // Get text without the line number pseudo-elements
  const text = Array.from(pre.querySelectorAll('.line'))
    .map(l => l.textContent)
    .join('\n');
  navigator.clipboard.writeText(text).then(() => {
    const btn = block.querySelector('.code-copy-btn');
    if (btn) {
      btn.textContent = 'Copied!';
      btn.classList.add('copied');
      setTimeout(() => { btn.textContent = 'Copy'; btn.classList.remove('copied'); }, 2000);
    }
  }).catch(() => {
    // Fallback
    const ta = document.createElement('textarea');
    ta.value = text;
    document.body.appendChild(ta);
    ta.select();
    document.execCommand('copy');
    document.body.removeChild(ta);
  });
}

function escHtml(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

// ── Send message ──────────────────────────────────────────────
async function sendMessage() {
  const input = $('chat-input');
  const text  = input.value.trim();
  if (!text && state.attachments.length === 0) return;
  if (state.generating) return;
  if (!state.currentSession) await newSession();

  // Build display text
  let displayText = text;
  if (state.attachments.length > 0) {
    const names = state.attachments.map(a => a.name).join(', ');
    displayText = `[📎 ${names}]\n${text}`.trim();
  }

  // Add user message locally
  state.currentSession.messages.push({ role: 'user', content: displayText });
  appendBubble('user', displayText);
  input.value = '';

  // Clear attachments
  clearAttachments();

  // Save user message to server
  await fetch(`/api/sessions/${state.currentSession.id}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages: state.currentSession.messages })
  });

  refreshSessionList();

  // Build API messages
  const systemPrompt = $('system-prompt')?.value?.trim();
  const apiMessages = [];
  if (systemPrompt) apiMessages.push({ role: 'system', content: systemPrompt });

  // Include history + current message with attachment content injected
  for (const msg of state.currentSession.messages.slice(0, -1)) {
    apiMessages.push({ role: msg.role, content: msg.content });
  }

  // Last message: inject attachment content
  let apiContent = text;
  if (state.attachments.length > 0) {
    let injected = '';
    for (const att of state.attachments) {
      if (att.error) {
        injected += `\n[Attachment error: ${att.name}: ${att.error}]\n`;
      } else if (att.type === 'image') {
        // Images handled as multimodal — skip text injection
      } else {
        const ext = att.name.split('.').pop() || '';
        injected += `\n### Attached: \`${att.name}\`\n\`\`\`${ext}\n${att.content}\n\`\`\`\n`;
      }
    }
    apiContent = injected + '\n' + text;
  }
  apiMessages.push({ role: 'user', content: apiContent });

  // Start generation
  setGenerating(true);
  state._streamBuffer = '';
  appendBubble('assistant', '');

  const model       = $('model-select')?.value || '';
  const temperature = parseFloat($('temp-slider')?.value || '0.7');
  const maxTokens   = parseInt($('max-tokens')?.value || '2048');

  if (!model) {
    appendBubble('error', 'No model selected. Choose a model in the Settings panel.');
    setGenerating(false);
    return;
  }

  if (!state.ws || state.ws.readyState !== WebSocket.OPEN) {
    appendBubble('error', 'WebSocket not connected. Trying to reconnect…');
    connectWS();
    setGenerating(false);
    return;
  }

  state.ws.send(JSON.stringify({
    model,
    messages:    apiMessages,
    temperature,
    max_tokens:  maxTokens,
    session_id:  state.currentSession.id,
  }));
}

function setGenerating(val) {
  state.generating = val;
  const sendBtn = $('btn-send');
  const stopBtn = $('btn-stop');
  const indicator = $('typing-indicator');
  if (sendBtn) sendBtn.disabled = val;
  if (stopBtn) stopBtn.disabled = !val;
  if (indicator) indicator.classList.toggle('hidden', !val);
  if (val) {
    const model = $('model-select')?.value || 'AI';
    if (indicator) indicator.textContent = `${model} is thinking…`;
  }
}

function stopGeneration() {
  // Close and reopen WS to interrupt stream
  if (state.ws) {
    try { state.ws.close(); } catch(e) {}
  }
  setGenerating(false);
  // Remove empty last bubble if any
  const container = $('chat-messages');
  const bubbles = container?.querySelectorAll('.bubble-assistant');
  if (bubbles?.length) {
    const last = bubbles[bubbles.length - 1];
    if (!last.querySelector('code,pre') && last.textContent.replace('AI','').trim() === '') {
      last.remove();
    }
  }
  setTimeout(connectWS, 200);
}

// ── File attachments ──────────────────────────────────────────
async function attachLocalFiles(files) {
  for (const file of files) {
    const formData = new FormData();
    formData.append('file', file);
    try {
      const r = await fetch('/api/upload', { method: 'POST', body: formData });
      const att = await r.json();
      addAttachment(att);
    } catch(e) {
      addAttachment({ name: file.name, type: 'binary', error: e.message, content: '' });
    }
  }
}

function addAttachment(att) {
  state.attachments.push(att);
  renderAttachments();
}

function removeAttachment(index) {
  state.attachments.splice(index, 1);
  renderAttachments();
}

function clearAttachments() {
  state.attachments = [];
  renderAttachments();
}

function renderAttachments() {
  const bar = $('attachments-bar');
  if (!bar) return;
  if (state.attachments.length === 0) {
    bar.classList.add('hidden');
    bar.innerHTML = '';
    return;
  }
  bar.classList.remove('hidden');
  bar.innerHTML = state.attachments.map((att, i) => {
    const icons = { image: '🖼', text: '📄', project: '🗂', binary: '📦' };
    const icon  = icons[att.type] || '📎';
    const cls   = att.error ? 'att-chip error' : 'att-chip';
    return `<div class="${cls}">
      ${icon} <span>${escHtml(att.name)}</span>
      <button onclick="removeAttachment(${i})" title="Remove">✕</button>
    </div>`;
  }).join('');
}

// ── SSH file browser ──────────────────────────────────────────
async function openSSHBrowser() {
  // Check SSH connected first
  try {
    const r = await fetch('/api/ssh/status');
    const { connected, username } = await r.json();
    if (!connected) {
      // Auto-connect
      const cr = await fetch('/api/ssh/connect', { method: 'POST' });
      const { ok, message } = await cr.json();
      if (!ok) {
        alert(`SSH connection failed:\n${message}\n\nConfigure SSH in Settings → SSH tab.`);
        return;
      }
    }
  } catch(e) {
    alert('Cannot reach server SSH endpoint.');
    return;
  }

  openModal('modal-ssh');
  await sshBrowse(state.sshPath);
}

async function sshBrowse(path) {
  state.sshPath = path;
  state.sshSelected = null;
  $('btn-ssh-attach').disabled = true;
  $('ssh-path-bar').textContent = path;

  const list = $('ssh-file-list');
  list.innerHTML = '<li style="color:var(--text-secondary);padding:8px 14px;">Loading…</li>';

  try {
    const r = await fetch(`/api/ssh/ls?path=${encodeURIComponent(path)}`);
    if (!r.ok) throw new Error(await r.text());
    const { entries } = await r.json();

    list.innerHTML = '';

    // Parent dir
    if (path !== '/') {
      const parent = path.split('/').slice(0,-1).join('/') || '/';
      const li = document.createElement('li');
      li.innerHTML = '📁 <span>..</span>';
      li.addEventListener('dblclick', () => sshBrowse(parent));
      list.appendChild(li);
    }

    for (const entry of entries) {
      const li = document.createElement('li');
      const icon = entry.is_dir ? '📁' : '📄';
      const size = !entry.is_dir && entry.size > 0
        ? `<span class="ssh-size">${(entry.size/1024).toFixed(1)} KB</span>` : '';
      li.innerHTML = `${icon} <span>${escHtml(entry.name)}</span>${size}`;
      li.dataset.path   = entry.path;
      li.dataset.is_dir = entry.is_dir;

      if (entry.is_dir) {
        li.addEventListener('dblclick', () => sshBrowse(entry.path));
      } else {
        li.addEventListener('click', () => {
          list.querySelectorAll('li').forEach(x => x.classList.remove('selected'));
          li.classList.add('selected');
          state.sshSelected = entry;
          $('btn-ssh-attach').disabled = false;
        });
        li.addEventListener('dblclick', () => attachSSHFile(entry));
      }
      list.appendChild(li);
    }
  } catch(e) {
    list.innerHTML = `<li style="color:var(--error);padding:8px 14px;">Error: ${escHtml(e.message)}</li>`;
  }
}

async function attachSSHFile(entry) {
  if (!entry) entry = state.sshSelected;
  if (!entry || entry.is_dir) return;

  try {
    const r = await fetch(`/api/ssh/read?path=${encodeURIComponent(entry.path)}`);
    if (!r.ok) throw new Error(await r.text());
    const att = await r.json();
    addAttachment(att);
    closeModal('modal-ssh');
  } catch(e) {
    alert(`Failed to read file:\n${e.message}`);
  }
}

// ── Modal helpers ─────────────────────────────────────────────
function openModal(id) {
  const el = $(id);
  if (el) el.classList.remove('hidden');
}

function closeModal(id) {
  const el = $(id);
  if (el) el.classList.add('hidden');
}

// ── Tab switching (settings modal) ────────────────────────────
function switchTab(tabId) {
  $$('.tab-content').forEach(t => t.classList.add('hidden'));
  $$('.tab-btn').forEach(b => b.classList.remove('active'));
  $(tabId)?.classList.remove('hidden');
  document.querySelector(`[data-tab="${tabId}"]`)?.classList.add('active');
}

// ── SSH connect test ──────────────────────────────────────────
async function testSSHConnection() {
  // Save SSH settings first
  const cfg = {
    ssh: {
      host:      $('cfg-ssh-host').value.trim(),
      port:      parseInt($('cfg-ssh-port').value) || 22,
      username:  $('cfg-ssh-user').value.trim(),
      password:  $('cfg-ssh-pass').value,
      key_path:  $('cfg-ssh-key').value.trim(),
      root_path: $('cfg-ssh-root').value.trim(),
    }
  };
  await fetch('/api/config', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(cfg)
  });

  const statusEl = $('ssh-status-text');
  if (statusEl) statusEl.textContent = 'Connecting…';

  try {
    const r = await fetch('/api/ssh/connect', { method: 'POST' });
    const { ok, message } = await r.json();
    if (statusEl) {
      statusEl.textContent = ok ? `✓ ${message}` : `✗ ${message}`;
      statusEl.style.color = ok
        ? getComputedStyle(document.documentElement).getPropertyValue('--success')
        : getComputedStyle(document.documentElement).getPropertyValue('--error');
    }
  } catch(e) {
    if (statusEl) statusEl.textContent = `✗ ${e.message}`;
  }
}

// ── Temperature slider live label ─────────────────────────────
function bindTempSlider() {
  const slider = $('temp-slider');
  const label  = $('temp-val');
  if (!slider || !label) return;
  slider.addEventListener('input', () => {
    label.textContent = parseFloat(slider.value).toFixed(2);
  });
}

// ── Auto-resize textarea ──────────────────────────────────────
function bindInputResize() {
  const input = $('chat-input');
  if (!input) return;
  input.addEventListener('input', () => {
    input.style.height = 'auto';
    input.style.height = Math.min(input.scrollHeight, 160) + 'px';
  });
}

// ── Mobile panel toggles ──────────────────────────────────────
function bindMobileToggles() {
  $('btn-sessions-toggle')?.addEventListener('click', () => {
    $('sessions-panel').classList.toggle('open');
    $('settings-panel').classList.remove('open');
  });
  $('btn-settings-toggle')?.addEventListener('click', () => {
    $('settings-panel').classList.toggle('open');
    $('sessions-panel').classList.remove('open');
  });
}

// ── Bind all events ───────────────────────────────────────────
function bindEvents() {
  // Send
  $('btn-send')?.addEventListener('click', sendMessage);
  $('btn-stop')?.addEventListener('click', stopGeneration);

  // Enter to send, Shift+Enter or Ctrl+Enter for newline
  $('chat-input')?.addEventListener('keydown', e => {
    if (e.key === 'Enter' && !e.shiftKey && !e.ctrlKey && !e.metaKey) {
      e.preventDefault();
      sendMessage();
    } else if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault();
      sendMessage();
    }
  });

  // New session
  $('btn-new-session')?.addEventListener('click', () => newSession());
  $('btn-new-session-side')?.addEventListener('click', () => newSession());

  // Clear
  $('btn-clear')?.addEventListener('click', clearCurrentSession);

  // Session search
  $('session-search')?.addEventListener('input', e => {
    renderSessionList(e.target.value);
  });

  // Settings modal
  $('btn-settings')?.addEventListener('click', () => openModal('modal-settings'));
  $('btn-save-settings')?.addEventListener('click', saveConfig);

  // Modal close buttons
  $$('.modal-close').forEach(btn => {
    btn.addEventListener('click', () => closeModal(btn.dataset.modal));
  });

  // Click outside modal to close
  $$('.modal').forEach(modal => {
    modal.addEventListener('click', e => {
      if (e.target === modal) closeModal(modal.id);
    });
  });

  // Tab buttons
  $$('.tab-btn').forEach(btn => {
    btn.addEventListener('click', () => switchTab(btn.dataset.tab));
  });

  // File attach
  $('btn-attach-local')?.addEventListener('click', () => $('file-input')?.click());
  $('file-input')?.addEventListener('change', e => {
    if (e.target.files.length) {
      attachLocalFiles(Array.from(e.target.files));
      e.target.value = '';
    }
  });

  // Drag-and-drop onto chat
  const chatPanel = $('chat-panel');
  chatPanel?.addEventListener('dragover', e => { e.preventDefault(); chatPanel.style.outline = '2px dashed var(--accent-primary)'; });
  chatPanel?.addEventListener('dragleave', () => { chatPanel.style.outline = ''; });
  chatPanel?.addEventListener('drop', e => {
    e.preventDefault();
    chatPanel.style.outline = '';
    if (e.dataTransfer.files.length) attachLocalFiles(Array.from(e.dataTransfer.files));
  });

  // SSH attach
  $('btn-attach-ssh')?.addEventListener('click', openSSHBrowser);
  $('btn-ssh-attach')?.addEventListener('click', () => attachSSHFile(state.sshSelected));

  // SSH connect test
  $('btn-ssh-connect')?.addEventListener('click', testSSHConnection);

  // Refresh models
  $('btn-refresh-models')?.addEventListener('click', loadModels);

  // Reload theme
  $('btn-reload-theme')?.addEventListener('click', async () => {
    await loadTheme();
  });

  // Chat font size slider
  const chatSizeSlider = $('chat-size-slider');
  const chatSizeVal    = $('chat-size-val');
  if (chatSizeSlider) {
    chatSizeSlider.addEventListener('input', () => {
      const size = chatSizeSlider.value;
      chatSizeVal.textContent = size + 'px';
      document.documentElement.style.setProperty('--chat-font-size', size + 'px');
    });
  }

  // Chat font family
  const chatFontSel = $('chat-font-select');
  if (chatFontSel) {
    chatFontSel.addEventListener('change', () => {
      document.documentElement.style.setProperty('--chat-font-family', chatFontSel.value);
    });
  }

  // Temperature slider
  bindTempSlider();

  // Input auto-resize
  bindInputResize();

  // Mobile
  bindMobileToggles();

  // Keyboard shortcuts
  document.addEventListener('keydown', e => {
    if (e.key === 'Escape') {
      $$('.modal:not(.hidden)').forEach(m => closeModal(m.id));
      document.querySelector('.ctx-menu')?.remove();
    }
    if ((e.ctrlKey || e.metaKey) && e.key === 'n') {
      e.preventDefault();
      newSession();
    }
  });
}
