/**
 * Tests for evaluate-report.R script
 *
 * Tests the report evaluation functionality including:
 * - Script execution and exit codes
 * - Evaluation criteria checking
 * - Output formatting
 * - Error handling
 */

const { describe, it } = require('node:test');
const assert = require('node:assert');
const { execSync } = require('node:child_process');
const fs = require('fs');

// Check if R is available
function isRAvailable() {
  try {
    execSync('which Rscript', { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}

describe('Report Evaluation Script', () => {

  const scriptPath = 'scripts/evaluate-report.R';
  const hasR = isRAvailable();

  describe('Script Availability', () => {
    it('should have evaluate-report.R script', () => {
      assert.ok(fs.existsSync(scriptPath), 'evaluate-report.R script should exist');
    });

    it('should be executable', () => {
      const stats = fs.statSync(scriptPath);
      const isExecutable = (stats.mode & 0o111) !== 0;
      assert.ok(isExecutable, 'Script should be executable');
    });

    it('should have proper shebang', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.startsWith('#!/usr/bin/env Rscript'), 'Should have Rscript shebang');
    });
  });

  describe('npm Scripts Integration', () => {
    it('should have report:evaluate npm script', () => {
      const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8'));
      assert.ok(pkg.scripts['report:evaluate'], 'Should have report:evaluate script');
    });

    it('should have report:evaluate:verbose npm script', () => {
      const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8'));
      assert.ok(pkg.scripts['report:evaluate:verbose'], 'Should have verbose variant');
    });

    it('should reference the evaluation script', () => {
      const pkg = JSON.parse(fs.readFileSync('package.json', 'utf8'));
      // Should reference either the R script directly or the interactive wrapper
      const script = pkg.scripts['report:evaluate'];
      assert.ok(
        script.includes('evaluate-report') || script.includes('evaluate-report-interactive'),
        'Should reference evaluation script'
      );
    });
  });

  describe('Command Documentation', () => {
    it('should have /evaluate-report command file', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      assert.ok(fs.existsSync(cmdFile), 'Command file should exist');
    });

    it('should document evaluation criteria', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      const content = fs.readFileSync(cmdFile, 'utf8').toLowerCase();

      // Check for key evaluation criteria
      assert.ok(content.includes('coverage'), 'Should mention coverage validation');
      assert.ok(content.includes('dgp'), 'Should mention DGP consistency');
      assert.ok(content.includes('bias'), 'Should mention bias quantification');
      assert.ok(content.includes('false positive'), 'Should mention false positive control');
    });

    it('should reference project learnings', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      const content = fs.readFileSync(cmdFile, 'utf8').toLowerCase();

      assert.ok(
        content.includes('learning') || content.includes('.claude/learnings'),
        'Should reference project learnings'
      );
    });

    it('should document anti-patterns', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      const content = fs.readFileSync(cmdFile, 'utf8').toLowerCase();

      assert.ok(content.includes('anti-pattern'), 'Should document anti-patterns');
      assert.ok(content.includes('training data'), 'Should mention training data reuse issue');
    });
  });

  describe('Script Functionality', () => {
    const testFn = hasR ? it : it.skip;

    testFn('should show usage when run without arguments', () => {
      try {
        execSync('Rscript scripts/evaluate-report.R', { encoding: 'utf8' });
        assert.fail('Should exit with error when no arguments provided');
      } catch (error) {
        assert.match(error.stderr || error.stdout, /Usage/, 'Should show usage message');
      }
    });

    testFn('should handle non-existent report file gracefully', () => {
      try {
        execSync('Rscript scripts/evaluate-report.R nonexistent-report.html 2>&1', { encoding: 'utf8' });
        assert.fail('Should exit with error for non-existent file');
      } catch (error) {
        const output = error.stderr || error.stdout;
        assert.ok(
          output.includes('not found') || output.includes('Error'),
          'Should report file not found'
        );
      }
    });
  });

  describe('Evaluation Criteria Implementation', () => {
    it('should implement coverage validation checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check_coverage_validation'), 'Should have coverage check function');
      assert.ok(content.includes('validate_difference_coverage'), 'Should check for proper methodology');
    });

    it('should implement DGP consistency checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check_dgp_consistency'), 'Should have DGP check function');
      assert.ok(content.includes('baseline_rate') || content.includes('parameters'), 'Should check parameters');
    });

    it('should implement bias quantification checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check_bias'), 'Should have bias check function');
      assert.ok(content.includes('precision') || content.includes('standard deviation'), 'Should check precision');
    });

    it('should implement false positive checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check_false_positive'), 'Should have false positive check function');
      assert.ok(content.includes('type i error') || content.includes('false.*positive'), 'Should check Type I error');
    });

    it('should implement visualization quality checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check_visualization'), 'Should have visualization check function');
      assert.ok(content.includes('ggplot') || content.includes('figures'), 'Should check for plots');
    });

    it('should implement structure checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('check.*structure') || content.includes('required_sections'), 'Should check report structure');
    });

    it('should implement statistical rigor checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('rigor') || content.includes('n_sims'), 'Should check statistical rigor');
    });

    it('should implement documentation quality checks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('documentation'), 'Should have documentation check function');
    });
  });

  describe('Output and Scoring', () => {
    it('should calculate overall score', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('overall_score'), 'Should calculate overall score');
    });

    it('should assign grades', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('Pass') && content.includes('Fail'), 'Should assign Pass/Fail grades');
    });

    it('should identify critical issues', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('critical') || content.includes('CRITICAL'), 'Should flag critical issues');
    });

    it('should provide recommendations', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(
        content.includes('recommendation') || content.includes('Recommendation'),
        'Should provide recommendations'
      );
    });
  });

  describe('Anti-Pattern Detection', () => {
    it('should detect training data reuse anti-pattern', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(
        content.includes('training.*data') || content.includes('anti_pattern'),
        'Should check for training data reuse'
      );
    });

    it('should flag missing coverage validation', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(
        content.includes('Missing coverage') || content.includes('No.*coverage validation'),
        'Should flag missing coverage'
      );
    });

    it('should check for adequate simulation runs', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      // Check that we look for n_sims with reasonable values (>=100 or >=200)
      assert.ok(
        content.includes('n_sims') && (content.includes('[1-9][0-9]{2}') || content.includes('>=') || content.includes('100')),
        'Should check for sufficient simulations'
      );
    });
  });

  describe('HTML and QMD Support', () => {
    it('should support HTML report files', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('html') || content.includes('read_html'), 'Should parse HTML');
    });

    it('should support QMD/Rmd report files', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('qmd') || content.includes('Rmd'), 'Should parse QMD/Rmd');
    });

    it('should extract sections from reports', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('sections'), 'Should extract sections');
    });

    it('should extract code chunks', () => {
      const content = fs.readFileSync(scriptPath, 'utf8');
      assert.ok(content.includes('code_chunk'), 'Should extract code chunks');
    });
  });

  describe('Integration with Project Standards', () => {
    it('should reference validation functions', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      const content = fs.readFileSync(cmdFile, 'utf8');
      assert.ok(
        content.includes('parameter-recovery-validation.R'),
        'Should reference validation functions'
      );
    });

    it('should align with CLAUDE.md standards', () => {
      const cmdFile = '.claude/commands/evaluate-report.md';
      const content = fs.readFileSync(cmdFile, 'utf8').toLowerCase();

      // Check alignment with project principles
      assert.ok(content.includes('statistical'), 'Should emphasize statistical rigor');
      assert.ok(content.includes('reproducib'), 'Should check reproducibility');
      assert.ok(content.includes('visuali'), 'Should check visualization');
    });
  });
});
