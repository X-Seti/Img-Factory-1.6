// AI Workshop Web UI - app.js - Version: 1
// X-Seti March 2026

const state = {
  sessions: [], currentSession: null,
  model: "", temperature: 0.70, maxTokens: 2048,
  systemPrompt: document.getElementById('system-prompt-input').value,
  ollamaUrl: "http://localhost:11434",
  attachments: [], generating: false,
  ws: null, sshConnId: null, sshSelectedPath: null,
  searchQuery: "", _streamBuffer: "",
};

// ── INIT ──────────────────────────────────────────────────────────────────
async function init() {
  await loadTheme();
  await loadModels();
  await loadSessions();
  connectWebSocket();
  checkOllamaStatus();
  setInterval(checkOllamaStatus, 15000);
}

// ── THEME ─────────────────────────────────────────────────────────────────
async function loadTheme(name = null) {
  const url = name ? `/api/theme?name=${encodeURIComponent(name)}` : '/api/theme';
  const data = await fetch(url).then(r => r.json()).catch(() => ({ colors: {}, themes: [] }));
  applyThemeColors(data.colors || {});
  const sel = document.getElementById('theme-select');
  if (data.themes && data.themes.length) {
    sel.innerHTML = data.themes.map(t =>
      `<option value="${t}" ${t === data.current ? 'selected' : ''}>${t}</option>`
    ).join('');
  }
}

function applyThemeColors(colors) {
  const map = {
    bg_primary: '--bg-primary', bg_secondary: '--bg-secondary',
    bg_tertiary: '--bg-tertiary', panel_bg: '--panel-bg',
    text_primary: '--text-primary', text_secondary: '--text-secondary',
    text_accent: '--text-accent', accent_primary: '--accent-primary',
    accent_secondary: '--accent-secondary', border: '--border',
    button_normal: '--btn-normal', button_hover: '--btn-hover',
    selection_background: '--selection-bg', selection_text: '--selection-text',
    success: '--success', warning: '--warning', error: '--error',
  };
  for (const [k, v] of Object.entries(map))
    if (colors[k]) document.documentElement.style.setProperty(v, colors[k]);
}

async function applyTheme(name) {
  await loadTheme(name);
  await fetch('/api/theme', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ theme: name })
  });
}

// ── OLLAMA STATUS ─────────────────────────────────────────────────────────
async function checkOllamaStatus() {
  const data = await fetch('/api/ollama/status').then(r => r.json()).catch(() => ({ running: false }));
  const on = data.running;
  document.getElementById('ollama-dot').className = 'dot' + (on ? ' online' : '');
  document.getElementById('right-ollama-dot').className = 'status-dot' + (on ? ' online' : '');
  const txt = on ? 'Ollama running' : 'Ollama offline';
  document.getElementById('ollama-status-text').textContent = txt;
  document.getElementById('right-ollama-text').textContent = txt;
}

// ── MODELS ────────────────────────────────────────────────────────────────
async function loadModels() {
  const data = await fetch('/api/models').then(r => r.json()).catch(() => ({ models: [] }));
  const sel = document.getElementById('model-select');
  if (data.models && data.models.length) {
    sel.innerHTML = data.models.map(m => `<option value="${m}">${m}</option>`).join('');
    state.model = data.models[0];
  } else {
    sel.innerHTML = '<option value="">No models found — is Ollama running?</option>';
  }
}

// ── SESSIONS ──────────────────────────────────────────────────────────────
async function loadSessions() {
  const data = await fetch('/api/sessions').then(r => r.json()).catch(() => ({ sessions: [] }));
  state.sessions = data.sessions || [];
  renderSessionList();
  if (state.sessions.length) await selectSession(state.sessions[0].id);
  else await newSession();
}

function renderSessionList() {
  const q   = state.searchQuery.toLowerCase();
  const vis = q ? state.sessions.filter(s => s.name.toLowerCase().includes(q)) : state.sessions;
  document.getElementById('session-list').innerHTML = vis.map(s => `
    <li class="${s.id === state.currentSession?.id ? 'active' : ''}"
        onclick="selectSession('${s.id}')">
      ${s.pinned ? '<span class="pin">⭐</span>' : ''}
      <span class="name">${esc(s.name)}</span>
      <span class="count">${s.msg_count}</span>
    </li>`).join('');
}

function filterSessions(q) { state.searchQuery = q; renderSessionList(); }

async function selectSession(sid) {
  const data = await fetch(`/api/sessions/${sid}`).then(r => r.json()).catch(() => null);
  if (!data) return;
  state.currentSession = data;
  renderSessionList();
  renderMessages(data.messages || []);
  document.getElementById('session-name-display').textContent = data.name || '—';
  document.getElementById('msg-count-display').textContent =
    data.messages?.length ? `${data.messages.length} messages` : '';
}

async function newSession() {
  const data = await fetch('/api/sessions', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({})
  }).then(r => r.json());
  state.sessions.unshift({ id: data.id, name: data.name, pinned: false, updated_at: data.updated_at, msg_count: 0 });
  renderSessionList();
  await selectSession(data.id);
}

async function deleteCurrentSession() {
  if (!state.currentSession) return;
  if (!confirm(`Delete session "${state.currentSession.name}"?`)) return;
  await fetch(`/api/sessions/${state.currentSession.id}`, { method: 'DELETE' });
  state.sessions = state.sessions.filter(s => s.id !== state.currentSession.id);
  state.currentSession = null;
  if (state.sessions.length) await selectSession(state.sessions[0].id);
  else await newSession();
}

async function renameCurrentSession() {
  if (!state.currentSession) return;
  const name = prompt('Rename session:', state.currentSession.name);
  if (!name?.trim()) return;
  state.currentSession.name = name.trim();
  await fetch(`/api/sessions/${state.currentSession.id}`, {
    method: 'PUT', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name: name.trim() })
  });
  document.getElementById('session-name-display').textContent = name.trim();
  const s = state.sessions.find(x => x.id === state.currentSession.id);
  if (s) s.name = name.trim();
  renderSessionList();
}

function exportSession(fmt) {
  if (!state.currentSession) return;
  window.open(`/api/sessions/${state.currentSession.id}/export?fmt=${fmt}`, '_blank');
}

// ── MESSAGES ──────────────────────────────────────────────────────────────
function renderMessages(messages) {
  const el = document.getElementById('chat-messages');
  el.innerHTML = messages.map(m => buildBubble(m.role, m.content)).join('');
  scrollToBottom();
}

function buildBubble(role, content) {
  const label = role === 'user' ? 'You' : role === 'assistant' ? 'AI' : role.toUpperCase();
  return `<div class="bubble ${role}">
    <div class="role-label">${label}</div>
    <div class="content">${renderMarkdown(content)}</div>
  </div>`;
}

function renderMarkdown(text) {
  return text
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/```(\w*)\n([\s\S]*?)```/g, (_, lang, code) =>
      `<pre><code class="lang-${lang}">${code}</code></pre>`)
    .replace(/`([^`\n]+)`/g, '<code>$1</code>')
    .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
    .replace(/\*(.+?)\*/g, '<em>$1</em>')
    .replace(/\n/g, '<br>');
}

function appendBubble(role, content) {
  const el = document.getElementById('chat-messages');
  if (role === 'assistant') {
    const bubbles = el.querySelectorAll('.bubble.assistant');
    const last = bubbles[bubbles.length - 1];
    if (last && last.querySelector('.content')?.innerHTML === '') {
      last.querySelector('.content').innerHTML = renderMarkdown(content);
      scrollToBottom(); return;
    }
  }
  el.insertAdjacentHTML('beforeend', buildBubble(role, content));
  scrollToBottom();
}

function updateLastAssistantBubble(content) {
  const el = document.getElementById('chat-messages');
  const last = el.querySelector('.bubble.assistant:last-child');
  if (last) { last.querySelector('.content').innerHTML = renderMarkdown(content); scrollToBottom(); }
}

function scrollToBottom() {
  const el = document.getElementById('chat-messages');
  el.scrollTop = el.scrollHeight;
}

// ── WEBSOCKET ─────────────────────────────────────────────────────────────
function connectWebSocket() {
  const proto = location.protocol === 'https:' ? 'wss' : 'ws';
  state.ws = new WebSocket(`${proto}://${location.host}/ws/chat`);
  state.ws.onmessage = (e) => {
    const msg = JSON.parse(e.data);
    if (msg.type === 'token') {
      state._streamBuffer += msg.token;
      updateLastAssistantBubble(state._streamBuffer);
    } else if (msg.type === 'done') {
      finishGeneration(msg.response);
    } else if (msg.type === 'error') {
      finishGeneration(null, msg.message);
    }
  };
  state.ws.onclose = () => setTimeout(connectWebSocket, 2000);
}

// ── SEND ──────────────────────────────────────────────────────────────────
async function sendMessage() {
  if (state.generating) return;
  const input = document.getElementById('message-input');
  const text  = input.value.trim();
  if (!text && !state.attachments.length) return;
  if (!state.ws || state.ws.readyState !== WebSocket.OPEN) {
    alert('WebSocket reconnecting, please try again in a moment.'); return;
  }
  if (!state.model) { alert('Please select a model first.'); return; }
  if (!state.currentSession) await newSession();

  const displayText = state.attachments.length
    ? `[📎 ${state.attachments.map(a => a.name).join(', ')}]\n${text}`.trim()
    : text;

  state.currentSession.messages = state.currentSession.messages || [];
  state.currentSession.messages.push({ role: 'user', content: displayText });
  appendBubble('user', displayText);

  await fetch(`/api/sessions/${state.currentSession.id}`, {
    method: 'PUT', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages: state.currentSession.messages })
  });

  input.value = '';
  state._streamBuffer = '';
  appendBubble('assistant', '');
  state.generating = true;
  setGenerating(true);

  state.ws.send(JSON.stringify({
    model: state.model,
    messages: state.currentSession.messages,
    temperature: state.temperature,
    max_tokens: state.maxTokens,
    system_prompt: state.systemPrompt,
    attachments: state.attachments,
  }));

  clearAttachments();
}

async function finishGeneration(response, error = null) {
  state.generating = false;
  setGenerating(false);
  document.getElementById('typing-indicator').textContent = '';
  state._streamBuffer = '';

  if (error) { appendBubble('error', error); return; }

  state.currentSession.messages.push({ role: 'assistant', content: response });
  await fetch(`/api/sessions/${state.currentSession.id}`, {
    method: 'PUT', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages: state.currentSession.messages })
  });
  const s = state.sessions.find(x => x.id === state.currentSession.id);
  if (s) s.msg_count = state.currentSession.messages.length;
  document.getElementById('msg-count-display').textContent = `${state.currentSession.messages.length} messages`;
  renderSessionList();
}

function stopGeneration() {
  if (state.ws) { state.ws.close(); }
  state.generating = false;
  setGenerating(false);
  document.getElementById('typing-indicator').textContent = '';
  connectWebSocket();
}

function setGenerating(on) {
  document.getElementById('send-btn').disabled = on;
  document.getElementById('stop-btn').disabled = !on;
  document.getElementById('typing-indicator').textContent = on ? `${state.model} is thinking…` : '';
}

function handleInputKey(e) {
  if (e.key === 'Enter' && e.ctrlKey) { e.preventDefault(); sendMessage(); }
}

// ── FILE ATTACHMENTS ──────────────────────────────────────────────────────
function triggerFileUpload() { document.getElementById('file-input').click(); }

async function handleFileUpload(event) {
  for (const file of event.target.files) {
    const form = new FormData();
    form.append('file', file);
    const data = await fetch('/api/upload', { method: 'POST', body: form }).then(r => r.json());
    addAttachment(data);
  }
  event.target.value = '';
}

function addAttachment(att) { state.attachments.push(att); renderAttachmentBar(); }
function removeAttachment(name) { state.attachments = state.attachments.filter(a => a.name !== name); renderAttachmentBar(); }
function clearAttachments() { state.attachments = []; renderAttachmentBar(); }

function renderAttachmentBar() {
  const bar = document.getElementById('attachment-bar');
  if (!state.attachments.length) { bar.className = ''; bar.innerHTML = ''; return; }
  bar.className = 'visible';
  bar.innerHTML = state.attachments.map(a => `
    <div class="chip ${a.error ? 'error' : ''}">
      ${a.type === 'image' ? '🖼' : a.type === 'text' ? '📄' : '📦'} ${esc(a.name)}
      <button class="chip-remove" onclick="removeAttachment('${escAttr(a.name)}')">✕</button>
    </div>`).join('');
}

// ── SSH BROWSER ───────────────────────────────────────────────────────────
function openSSHBrowser() {
  document.getElementById('ssh-modal').classList.add('open');
  if (state.sshConnId) {
    document.getElementById('ssh-connect-form').style.display = 'none';
    document.getElementById('ssh-browser').style.display = 'block';
  }
}
function closeSSHModal() { document.getElementById('ssh-modal').classList.remove('open'); }

async function sshConnect() {
  const msg = document.getElementById('ssh-connect-msg');
  msg.textContent = 'Connecting…'; msg.style.color = '';
  const data = await fetch('/api/ssh/connect', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      host: document.getElementById('ssh-host').value,
      port: +document.getElementById('ssh-port').value,
      username: document.getElementById('ssh-user').value,
      password: document.getElementById('ssh-pass').value,
      key_path: document.getElementById('ssh-key').value,
    })
  }).then(r => r.json());

  if (data.ok) {
    state.sshConnId = data.conn_id;
    msg.textContent = '✓ ' + data.message;
    document.getElementById('ssh-connect-form').style.display = 'none';
    document.getElementById('ssh-browser').style.display = 'block';
    await sshListDir('/home');
  } else {
    msg.textContent = '✗ ' + data.message;
    msg.style.color = 'var(--error)';
  }
}

async function sshListDir(path) {
  const data = await fetch(`/api/ssh/${state.sshConnId}/ls?path=${encodeURIComponent(path)}`).then(r => r.json());
  document.getElementById('ssh-path').textContent = data.path;
  state.sshSelectedPath = null;
  document.getElementById('ssh-attach-btn').disabled = true;

  const ul = document.getElementById('ssh-file-list');
  let html = '';
  if (data.path !== '/') {
    const parent = data.path.split('/').slice(0, -1).join('/') || '/';
    html += `<li onclick="sshListDir('${escAttr(parent)}')"><span class="entry-icon">📁</span><span class="entry-name">..</span></li>`;
  }
  html += (data.entries || []).map(e => {
    const click = e.is_dir ? `sshListDir('${escAttr(e.path)}')` : `sshSelectFile('${escAttr(e.path)}', this)`;
    const size = !e.is_dir && e.size > 0 ? `<span class="entry-size">${(e.size / 1024).toFixed(1)} KB</span>` : '';
    return `<li onclick="${click}"><span class="entry-icon">${e.is_dir ? '📁' : '📄'}</span><span class="entry-name">${esc(e.name)}</span>${size}</li>`;
  }).join('');
  ul.innerHTML = html;
}

function sshSelectFile(path, el) {
  document.querySelectorAll('#ssh-file-list li').forEach(l => l.classList.remove('selected'));
  el.classList.add('selected');
  state.sshSelectedPath = path;
  document.getElementById('ssh-attach-btn').disabled = false;
}

async function sshAttachSelected() {
  if (!state.sshSelectedPath || !state.sshConnId) return;
  const data = await fetch(`/api/ssh/${state.sshConnId}/read?path=${encodeURIComponent(state.sshSelectedPath)}`).then(r => r.json());
  if (data.ok) { addAttachment({ name: data.name, type: data.type, mime: data.mime || 'text/plain', content: data.content, source: 'ssh' }); closeSSHModal(); }
  else alert('Could not read file: ' + (data.content || 'Unknown error'));
}

// ── MOBILE ────────────────────────────────────────────────────────────────
function toggleSidebar() { document.getElementById('sidebar').classList.toggle('open'); }
function toggleSettings() { document.getElementById('settings-panel').classList.toggle('open'); }

// ── UTILS ─────────────────────────────────────────────────────────────────
function esc(s) {
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}
function escAttr(s) { return String(s).replace(/'/g, "\\'"); }

// ── START ─────────────────────────────────────────────────────────────────
init();
