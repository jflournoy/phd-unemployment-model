/**
 * Tests for report-quality-auditor agent
 *
 * Tests the agent configuration and validates it has all required components
 * for comprehensive report quality auditing.
 */

const { describe, it } = require('node:test');
const assert = require('node:assert');
const fs = require('fs');

describe('Report Quality Auditor Agent', () => {
  const agentPath = '.claude/agents/report-quality-auditor.md';
  let agentContent;

  it('should have agent file', () => {
    assert.ok(fs.existsSync(agentPath), 'Agent file should exist');
    agentContent = fs.readFileSync(agentPath, 'utf8');
  });

  describe('Frontmatter', () => {
    it('should have valid YAML frontmatter', () => {
      assert.ok(agentContent.startsWith('---'), 'Should start with YAML delimiter');
      assert.ok(agentContent.includes('agent-type:'), 'Should have agent-type field');
      assert.ok(agentContent.includes('allowed-tools:'), 'Should have allowed-tools field');
      assert.ok(agentContent.includes('description:'), 'Should have description field');
      assert.ok(agentContent.includes('last-updated:'), 'Should have last-updated field');
    });

    it('should specify general-purpose agent type', () => {
      assert.match(agentContent, /agent-type:\s*general-purpose/, 'Should be general-purpose agent');
    });

    it('should have appropriate tool access', () => {
      assert.ok(agentContent.includes('[Read'), 'Should have Read tool access');
      assert.ok(agentContent.includes('Glob'), 'Should have Glob tool access');
      assert.ok(agentContent.includes('Grep'), 'Should have Grep tool access');
      // WebFetch is useful for fetching external references if needed
    });

    it('should have descriptive summary', () => {
      const descMatch = agentContent.match(/description:\s*(.+)/);
      assert.ok(descMatch, 'Should have description');
      assert.ok(descMatch[1].length > 20, 'Description should be meaningful');
      assert.ok(
        descMatch[1].toLowerCase().includes('report') || descMatch[1].toLowerCase().includes('quality'),
        'Description should mention reports or quality'
      );
    });
  });

  describe('Task Instructions', () => {
    it('should have phased approach', () => {
      assert.ok(agentContent.includes('Phase 1:'), 'Should have Phase 1');
      assert.ok(agentContent.includes('Phase 2:'), 'Should have Phase 2');
      assert.ok(agentContent.includes('Phase 3:'), 'Should have Phase 3');
      assert.ok(agentContent.includes('Phase 4:'), 'Should have Phase 4');
      assert.ok(agentContent.includes('Phase 5:'), 'Should have Phase 5');
      assert.ok(agentContent.includes('Phase 6:'), 'Should have Phase 6');
      assert.ok(agentContent.includes('Phase 7:'), 'Should have Phase 7');
    });

    it('should include report discovery phase', () => {
      assert.ok(
        agentContent.toLowerCase().includes('discovery') || agentContent.toLowerCase().includes('selection'),
        'Should have report discovery/selection phase'
      );
    });

    it('should include structural completeness check', () => {
      assert.ok(
        agentContent.toLowerCase().includes('structural') && agentContent.toLowerCase().includes('completeness'),
        'Should check structural completeness'
      );
    });

    it('should include statistical rigor audit', () => {
      assert.ok(
        agentContent.toLowerCase().includes('statistical') && agentContent.toLowerCase().includes('rigor'),
        'Should audit statistical rigor'
      );
    });

    it('should include code-narrative consistency check', () => {
      assert.ok(
        agentContent.toLowerCase().includes('code') && agentContent.toLowerCase().includes('narrative'),
        'Should check code-narrative consistency'
      );
    });

    it('should include visualization quality audit', () => {
      assert.ok(
        agentContent.toLowerCase().includes('visualization') && agentContent.toLowerCase().includes('quality'),
        'Should audit visualization quality'
      );
    });

    it('should include clarity and communication audit', () => {
      assert.ok(
        agentContent.toLowerCase().includes('clarity') || agentContent.toLowerCase().includes('communication'),
        'Should audit clarity and communication'
      );
    });

    it('should integrate with project standards', () => {
      assert.ok(
        agentContent.toLowerCase().includes('project standards') || agentContent.toLowerCase().includes('integration'),
        'Should integrate with project standards'
      );
    });
  });

  describe('Critical Anti-Pattern Detection', () => {
    it('should detect training data reuse', () => {
      assert.ok(
        agentContent.toLowerCase().includes('training data') && agentContent.toLowerCase().includes('reuse'),
        'Should detect training data reuse anti-pattern'
      );
    });

    it('should detect DGP inconsistency', () => {
      assert.ok(
        agentContent.includes('DGP') && agentContent.toLowerCase().includes('inconsisten'),
        'Should detect DGP inconsistency'
      );
    });

    it('should detect prediction interval confusion', () => {
      assert.ok(
        agentContent.toLowerCase().includes('prediction interval') && agentContent.toLowerCase().includes('confusion'),
        'Should detect prediction interval confusion'
      );
    });

    it('should detect undocumented defaults', () => {
      assert.ok(
        agentContent.toLowerCase().includes('undocumented') && agentContent.toLowerCase().includes('default'),
        'Should detect undocumented defaults'
      );
    });
  });

  describe('Statistical Validation Criteria', () => {
    it('should check coverage rates', () => {
      assert.ok(agentContent.toLowerCase().includes('coverage'), 'Should check coverage rates');
      assert.ok(
        agentContent.includes('93-97%') || agentContent.includes('95%'),
        'Should specify target coverage range'
      );
    });

    it('should assess bias', () => {
      assert.ok(agentContent.toLowerCase().includes('bias'), 'Should assess bias');
      assert.ok(
        agentContent.includes('%') && agentContent.toLowerCase().includes('bias'),
        'Should have bias thresholds'
      );
    });

    it('should check false positive control', () => {
      assert.ok(
        agentContent.toLowerCase().includes('false positive') || agentContent.toLowerCase().includes('type i'),
        'Should check false positive control'
      );
    });

    it('should verify simulation sample size', () => {
      assert.ok(
        agentContent.toLowerCase().includes('simulation') && agentContent.toLowerCase().includes('sample'),
        'Should verify simulation sample size'
      );
      assert.ok(
        agentContent.includes('100') || agentContent.includes('200'),
        'Should specify minimum simulation runs'
      );
    });
  });

  describe('Visualization Audit Components', () => {
    it('should check legend completeness', () => {
      assert.ok(agentContent.toLowerCase().includes('legend'), 'Should check legend completeness');
    });

    it('should check axis clarity', () => {
      assert.ok(
        agentContent.toLowerCase().includes('axis') || agentContent.toLowerCase().includes('axes'),
        'Should check axis clarity'
      );
    });

    it('should check visual separation', () => {
      assert.ok(
        agentContent.toLowerCase().includes('visual separation') || agentContent.toLowerCase().includes('distinguishable'),
        'Should check visual separation'
      );
    });

    it('should check caption quality', () => {
      assert.ok(agentContent.toLowerCase().includes('caption'), 'Should check caption quality');
    });

    it('should flag common visualization errors', () => {
      assert.ok(
        agentContent.toLowerCase().includes('visualization error') || agentContent.toLowerCase().includes('common'),
        'Should list common visualization errors'
      );
    });
  });

  describe('Report Structure Requirements', () => {
    it('should require overview section', () => {
      assert.ok(agentContent.toLowerCase().includes('overview'), 'Should require overview section');
    });

    it('should require DGP documentation', () => {
      assert.ok(
        agentContent.includes('DGP') || agentContent.includes('data-generating process'),
        'Should require DGP documentation'
      );
    });

    it('should require coverage validation results', () => {
      assert.ok(
        agentContent.toLowerCase().includes('coverage') && agentContent.toLowerCase().includes('validation'),
        'Should require coverage validation results'
      );
    });

    it('should require bias analysis', () => {
      assert.ok(
        agentContent.toLowerCase().includes('bias analysis'),
        'Should require bias analysis section'
      );
    });

    it('should require interpretation section', () => {
      assert.ok(agentContent.toLowerCase().includes('interpretation'), 'Should require interpretation section');
    });

    it('should require reproducibility information', () => {
      assert.ok(
        agentContent.toLowerCase().includes('reproducib'),
        'Should require reproducibility information'
      );
    });
  });

  describe('Output Format', () => {
    it('should specify output format', () => {
      assert.ok(
        agentContent.toLowerCase().includes('output format'),
        'Should specify output format'
      );
    });

    it('should include overall grade', () => {
      assert.ok(
        agentContent.toLowerCase().includes('overall grade') || agentContent.toLowerCase().includes('pass/fail'),
        'Should include overall grade'
      );
    });

    it('should include critical issues section', () => {
      assert.ok(
        agentContent.toLowerCase().includes('critical issues'),
        'Should have critical issues section'
      );
    });

    it('should include warnings section', () => {
      assert.ok(agentContent.toLowerCase().includes('warning'), 'Should have warnings section');
    });

    it('should include strengths section', () => {
      assert.ok(agentContent.toLowerCase().includes('strength'), 'Should highlight strengths');
    });

    it('should include recommendations', () => {
      assert.ok(agentContent.toLowerCase().includes('recommendation'), 'Should provide recommendations');
    });

    it('should include quality metrics', () => {
      assert.ok(
        agentContent.toLowerCase().includes('quality metric') || agentContent.toLowerCase().includes('score'),
        'Should include quality metrics'
      );
    });

    it('should include approval checklist', () => {
      assert.ok(
        agentContent.toLowerCase().includes('checklist') || agentContent.toLowerCase().includes('approval'),
        'Should include approval checklist'
      );
    });
  });

  describe('Integration with Project Standards', () => {
    it('should reference project learnings', () => {
      assert.ok(
        agentContent.includes('.claude/learnings') || agentContent.toLowerCase().includes('project learning'),
        'Should reference project learnings'
      );
    });

    it('should reference CLAUDE.md standards', () => {
      assert.ok(
        agentContent.includes('CLAUDE.md'),
        'Should reference CLAUDE.md standards'
      );
    });

    it('should reference validation functions', () => {
      assert.ok(
        agentContent.includes('parameter-recovery-validation.R') || agentContent.toLowerCase().includes('validation function'),
        'Should reference validation functions'
      );
    });

    it('should check consistency with previous reports', () => {
      assert.ok(
        agentContent.toLowerCase().includes('previous report') || agentContent.toLowerCase().includes('consistent'),
        'Should check consistency with previous reports'
      );
    });
  });

  describe('Success Criteria', () => {
    it('should have success criteria section', () => {
      assert.ok(
        agentContent.toLowerCase().includes('success criteria'),
        'Should have success criteria section'
      );
    });

    it('should define complete audit scope', () => {
      const successSection = agentContent.split('## Success Criteria')[1]?.split('##')[0] || '';
      assert.ok(
        successSection.toLowerCase().includes('complete'),
        'Should define complete audit scope'
      );
    });
  });

  describe('Error Handling', () => {
    it('should have error handling section', () => {
      assert.ok(
        agentContent.toLowerCase().includes('error handling'),
        'Should have error handling section'
      );
    });

    it('should handle corrupted files gracefully', () => {
      const errorSection = agentContent.split('## Error Handling')[1]?.split('##')[0] || '';
      assert.ok(
        errorSection.toLowerCase().includes('corrupt') || errorSection.toLowerCase().includes('unreadable'),
        'Should handle corrupted files'
      );
    });

    it('should continue audit despite partial failures', () => {
      const errorSection = agentContent.split('## Error Handling')[1]?.split('##')[0] || '';
      assert.ok(
        errorSection.toLowerCase().includes('continue'),
        'Should continue audit despite failures'
      );
    });
  });

  describe('Quality Standards Reference', () => {
    it('should have quality standards section', () => {
      assert.ok(
        agentContent.toLowerCase().includes('quality standard'),
        'Should have quality standards section'
      );
    });

    it('should define critical standards', () => {
      assert.ok(
        agentContent.toLowerCase().includes('critical standard') || agentContent.toLowerCase().includes('must pass'),
        'Should define critical standards'
      );
    });

    it('should define important standards', () => {
      assert.ok(
        agentContent.toLowerCase().includes('important standard') || agentContent.toLowerCase().includes('should pass'),
        'Should define important standards'
      );
    });

    it('should define enhancement standards', () => {
      assert.ok(
        agentContent.toLowerCase().includes('enhancement') || agentContent.toLowerCase().includes('nice to have'),
        'Should define enhancement standards'
      );
    });
  });

  describe('Comprehensive Coverage', () => {
    it('should cover all major quality dimensions', () => {
      const dimensions = [
        'structural',
        'statistical',
        'visualization',
        'clarity',
        'reproducib',
        'consistency'
      ];

      dimensions.forEach(dimension => {
        assert.ok(
          agentContent.toLowerCase().includes(dimension),
          `Should cover ${dimension} quality dimension`
        );
      });
    });

    it('should provide actionable guidance', () => {
      assert.ok(
        agentContent.toLowerCase().includes('actionable') || agentContent.toLowerCase().includes('specific'),
        'Should provide actionable guidance'
      );
    });

    it('should prioritize by severity', () => {
      assert.ok(
        agentContent.toLowerCase().includes('priority') || agentContent.toLowerCase().includes('severity'),
        'Should prioritize by severity'
      );
    });
  });
});
